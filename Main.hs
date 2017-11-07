{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM
import qualified Control.Distributed.Backend.P2P as P2P
import qualified Control.Distributed.Process as D
import qualified Control.Distributed.Process.Node as D
import Control.Monad (mzero, forever, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.Catch as Ex
import qualified Crypto.Random as CR
import Data.Bits (shiftL, (.|.))
import qualified Data.Binary as Bin
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as B
import Data.Foldable (traverse_, for_, toList)
import Data.Function (fix)
import Data.List ((\\))
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Time as Time
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Crypto.Hash as Cry
import qualified Crypto.Hash.Algorithms as Cry
import GHC.Generics (Generic)
import qualified Network.Socket as NS
import Numeric.Natural (Natural)
import qualified Options.Applicative as OA
import qualified System.Environment as Sys
import qualified System.Exit as Sys
import qualified System.IO as Sys

--------------------------------------------------------------------------------

newtype TransactionId = TransactionId Word64
  deriving (Eq, Show, Generic)

instance Bin.Binary TransactionId

---
data Transaction = Transaction
  { _transactionId   :: !TransactionId
  , _transactionData :: !Double
  } deriving (Eq, Show, Generic)

instance Bin.Binary Transaction

---
newtype BlockHash = BlockHash (Cry.Digest Cry.SHA256)
  deriving (Eq, Show)

instance Bin.Binary BlockHash where
  put = \(BlockHash x) -> bin_put_SHA256 x
  get = BlockHash <$> bin_get_SHA256

---
newtype BlockchainHeader = BlockchainHeader
  { _blockchainHeaderPrevHash :: BlockHash
  } deriving (Eq, Show, Generic)

instance Bin.Binary BlockchainHeader

---
-- | Like 'Blockchain' but without the header that proves the data was mined.
data Blockchainish
  = BlockchainishGenesis
  | BlockchainishNode !(Seq Transaction) !Blockchain
    -- ^ Transactions ordered chronologically. Oldest on the left.
  deriving (Eq, Show, Generic)

instance Bin.Binary Blockchainish

blockchainishHash :: Blockchainish -> BlockHash
blockchainishHash = BlockHash . Cry.hashlazy . Bin.encode

---
data Blockchain = Blockchain
  { _blockchainHeader :: !BlockchainHeader
  , _blockchainBlockchainish :: !Blockchainish
  } deriving (Eq, Show, Generic)

instance Bin.Binary Blockchain

genesis :: Blockchain
genesis = mineForFree BlockchainishGenesis

blockchainHeight :: Blockchain -> Natural
blockchainHeight = \(Blockchain _ bt) -> case bt of
  BlockchainishGenesis -> 0
  BlockchainishNode _ bc -> 1 + blockchainHeight bc

-- | Ordered chronologically. Oldest on the left.
blockchainTransactions :: Blockchain -> Seq Transaction
blockchainTransactions = \(Blockchain _ bt) -> case bt of
  BlockchainishGenesis -> mempty
  BlockchainishNode txs bct -> blockchainTransactions bct <> txs

showBlockchainHash :: Blockchain -> String
showBlockchainHash (Blockchain (BlockchainHeader (BlockHash x)) _) = show x

---

mineForFree :: Blockchainish -> Blockchain
mineForFree bct0 = Blockchain (BlockchainHeader (blockchainishHash bct0)) bct0

mine :: CR.DRG g => g -> Blockchainish -> IO (g, Blockchain)
mine drg0 bct0 = do
  -- Proof of Work? More like Proof of Wait. Essentially, the "work" here is to
  -- wait between 1 and 3 seconds, which is fine for our toy example.
  let (x, drg1) = CR.withDRG drg0 randomSmallPositiveDouble
  threadDelay (truncate ((1 + (x * 2)) * 1000000))
  pure (drg1, mineForFree bct0)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Env = Env
  { _envBlockchain :: STM.TVar Blockchain
  , _envPendingTxs :: STM.TQueue Transaction
    -- ^ List of transactions to be mined next. We mine as many transactions as
    --   are available. No block size drama, yet.
  }

newEnv :: MonadIO m => Blockchain -> m Env
newEnv bc0 = liftIO $ Env <$> STM.newTVarIO bc0 <*> STM.newTQueueIO

---

main :: IO ()
main = do
  co <- getCmdOpts
  let (h, p) = _coHere co
      peers = fmap mkNodeId (_coPeers co)
  P2P.bootstrap h p peers D.initRemoteTable $ do
     -- A minor delay to wait for peers to be discovered and connected.
     liftIO $ threadDelay 2000000 -- 2 seconds
     -- Get an environment with some initial blockchain.
     env <- newEnv =<< case peers of
        [] -> pure genesis
        _  -> getInitialBlockhain
     -- Share the blockchain with new peers when requested.
     D.spawnLocal $ on_giveMeBlockchain env
     -- Replace the local blockchain by other provided by a peer.
     D.spawnLocal $ on_newBlockchain env
     let drg0 = CR.drgNewSeed (_coRandomSeed co)
         (drg1, drg2) = drgSplit drg0
     -- Mine new blocks and share them with peers.
     D.spawnLocal $ miner env drg2
     -- Make new transactions to be added to a future block.
     transactionMaker env drg1 (_coSendFor co)
     liftIO $ do
        -- Wait grace time
        debug ("Grace time. Waiting " ++ show (_coWaitFor co))
        threadDelay (truncate (_coWaitFor co * 1000000))
        renderFinalOutput env

-- | Get a blockchain from some peer. Retries forever.
getInitialBlockhain :: D.Process Blockchain
getInitialBlockhain = do
  selfPid <- D.getSelfPid
  fix $ \k -> do
     P2P.getCapable "giveMeBlockchain" >>= \case
        [] -> do
           debug "Waiting for some peer to share the blockchain..."
           liftIO $ threadDelay 1000000 -- 1 second
           k
        (pid:_) -> do
           D.send pid (GiveMeBlockchain selfPid)
           NewBlockchain bc <- D.expect
           debug ("Got initial blockchain from " ++ show pid)
           pure bc

renderFinalOutput :: Env -> IO ()
renderFinalOutput env = do
   (bc, qptxs) <- STM.atomically $ (,)
       <$> STM.readTVar (_envBlockchain env)
       <*> fmap not (STM.isEmptyTQueue (_envPendingTxs env))
   let txs = blockchainTransactions bc
   for_ (zip [1..] (toList txs)) $ \(i, t) -> do
      debug ("Transaction number " ++ show (i :: Word32) ++ ": " ++ show t)
   putStrLn ("Blockchain height: " ++ show (blockchainHeight bc))
   putStrLn ("Processed transactions: " ++ show (Seq.length txs))
   putStrLn ("Pending transactions: " ++ show qptxs)
   putStrLn ("Blockchain hash: " ++ showBlockchainHash bc)


transactionMaker
  :: forall g
  .  CR.DRG g
  => Env
  -> g
  -> Time.NominalDiffTime   -- ^ _coSendFor
  -> D.Process ()
transactionMaker env = \drg0 sfor -> do
   tstop <- liftIO $ Time.addUTCTime sfor <$> Time.getCurrentTime
   go tstop drg0
  where
   go :: Time.UTCTime -> g -> D.Process ()
   go tstop = fix $ \k drg0 -> do
      -- Instead of this syscall on every iteration, we could have this process
      -- killed after some seconds from a different thread. However, this is
      -- simpler to implement, and as such, suitable for our toy example.
      tnow <- liftIO Time.getCurrentTime
      when (tnow < tstop) $ do
         let (tx, drg1) = CR.withDRG drg0 $ do
                   Transaction <$> fmap TransactionId randomWord64
                               <*> randomSmallPositiveDouble
         liftIO $ STM.atomically $ STM.writeTQueue (_envPendingTxs env) tx
         liftIO $ threadDelay 3000 -- delay of 3ms just to limit the
                                   -- throughput, just because this makes it
                                   -- easier to look at the output
         k drg1

miner :: CR.DRG g => Env -> g -> D.Process ()
miner env = \drg0 -> do
   selfPid <- D.getSelfPid
   go selfPid drg0
  where
   go selfPid = fix $ \k drg0 -> do
      (bc0, txs) <- liftIO $ STM.atomically $ do
         bc0 <- STM.readTVar (_envBlockchain env)
         txs <- readTQueueAll (_envPendingTxs env)
         STM.check (not (null txs))
         pure (bc0, Seq.fromList txs)
      (drg1, bc1) <- liftIO $ mine drg0 (BlockchainishNode txs bc0)
      new <- liftIO $ STM.atomically $ do
         bc0' <- STM.readTVar (_envBlockchain env)
         case (bc0 == bc0') of
            False -> pure False
            True -> do
               STM.writeTVar (_envBlockchain env) $! bc1
               pure True
      when new $ do
         -- Broadcast new chain. We should only broadcast new blocks, not
         -- the entire thing, but...
         debug ("Mined new block " ++ showBlockchainHash bc1)
         P2P.nsendPeers "newBlockchain" (NewBlockchain bc1)
      k drg1


--------------------------------------------------------------------------------

data NewBlockchain = NewBlockchain !Blockchain
  deriving (Generic)
instance Bin.Binary NewBlockchain

-- | Receive a new blockchain from a remote peer, perhaps adopting it if it is
-- longer than ours. If that happens, we'll have to re-mine previously mined
-- transactions missing in the new blockchain.
on_newBlockchain :: Env -> D.Process ()
on_newBlockchain env = do
    D.register "newBlockchain" =<< D.getSelfPid
    debug "Registered newBlockchain"
    forever $ do
       NewBlockchain bc1 <- D.expect
       y <- liftIO $ STM.atomically $ do
          bc0 <- STM.readTVar (_envBlockchain env)
          -- If new blockchain is longer than ours, then we adopt it.
          case (blockchainHeight bc1 > blockchainHeight bc0) of
             False -> pure Nothing
             True -> do
                STM.writeTVar (_envBlockchain env) bc1
                -- Lost transactions are re-queued for mining
                traverse_ (STM.writeTQueue (_envPendingTxs env))
                          (toList (blockchainTransactions bc0) \\
                           toList (blockchainTransactions bc1))
                pure (Just (showBlockchainHash bc0))
       for_ y $ \hsh -> do
          debug ("Replaced old blockchain " ++ hsh ++
                 " with new blockchain " ++ showBlockchainHash bc1)


--------------------------------------------------------------------------------

data GiveMeBlockchain = GiveMeBlockchain !D.ProcessId
  deriving (Generic)
instance Bin.Binary GiveMeBlockchain

-- | Sends the blockchain to a remote peer that requests it.
on_giveMeBlockchain :: Env -> D.Process ()
on_giveMeBlockchain env = do
  D.register "giveMeBlockchain" =<< D.getSelfPid
  debug "Registered giveMeBlockchain"
  forever $ do
     GiveMeBlockchain pid <- D.expect
     bc <- liftIO $ STM.readTVarIO (_envBlockchain env)
     D.send pid (NewBlockchain bc)

--------------------------------------------------------------------------------

data CmdOpts = CmdOpts
  { _coSendFor :: !Time.NominalDiffTime
  , _coWaitFor :: !Time.NominalDiffTime
  , _coRandomSeed :: !CR.Seed
  , _coHere :: !(NS.HostName, NS.ServiceName)
  , _coPeers :: ![(NS.HostName, NS.ServiceName)]
  }

oa_hostAndPort :: OA.ReadM (NS.HostName, NS.ServiceName)
oa_hostAndPort = OA.maybeReader $ \s -> case break (== ':') s of
  (h, ':':p) | length h > 0 && length p > 0 -> Just (h, p)
  _ -> Nothing

getCmdOpts :: IO CmdOpts
getCmdOpts = OA.execParser $ OA.info (OA.helper <*> pCmdOpts) $ mconcat
    [ OA.fullDesc, OA.progDesc "Very fake Proof-of-Wait blockchain" ]
  where
    pCmdOpts :: OA.Parser CmdOpts
    pCmdOpts = CmdOpts
        <$> (fmap fromInteger $ OA.option OA.auto $
               OA.long "send-for"    <>
               OA.metavar "SECONDS" <>
               OA.help "How many seconds to send data for")
        <*> (fmap fromInteger $ OA.option OA.auto $
               OA.long "wait-for" <>
               OA.metavar "SECONDS" <>
               OA.help "How many seconds to wait for after sending data")
        <*> (fmap CR.seedFromInteger $ OA.option OA.auto $
               OA.long "with-seed" <>
               OA.metavar "INTEGER" <>
               OA.help "How many seconds to wait for after sending data")
        <*> (OA.option oa_hostAndPort $
               OA.long "bind" <>
               OA.metavar "HOST:PORT" <>
               OA.help "Local host and TCP port to bind")
        <*> OA.many (OA.option oa_hostAndPort $
               OA.long "peer" <>
               OA.metavar "HOST:PORT" <>
               OA.help "Seed remote host and TCP port to connect to")


--------------------------------------------------------------------------------
-- Miscellaneous assorted inefficient functions

randomWord16 :: CR.MonadRandom m => m Word16
randomWord16 = do  -- Could be more efficient, I know.
  bs :: ByteArray.Bytes <- CR.getRandomBytes 2
  let [a,b] = map fromIntegral (ByteArray.unpack bs) :: [Word16]
  pure (shiftL a 8 .|. b)

randomWord32 :: CR.MonadRandom m => m Word32
randomWord32 = do  -- Could be more efficient, I know.
  bs :: ByteArray.Bytes <- CR.getRandomBytes 4
  let [a,b,c,d] = map fromIntegral (ByteArray.unpack bs) :: [Word32]
  pure (shiftL a 24 .|. shiftL b 16 .|. shiftL c 8 .|. d)

randomWord64 :: CR.MonadRandom m => m Word64
randomWord64 = do  -- Could be more efficient, I know.
  bs :: ByteArray.Bytes <- CR.getRandomBytes 8
  let [a,b,c,d,e,f,g,h] = map fromIntegral (ByteArray.unpack bs) :: [Word64]
  pure (shiftL a 64 .|. shiftL b 48 .|. shiftL c 40 .|. shiftL d 32 .|.
        shiftL e 24 .|. shiftL f 16 .|. shiftL g  8 .|. h)

-- | Double in the range [0, 1)
randomSmallPositiveDouble :: CR.MonadRandom m => m Double
randomSmallPositiveDouble = do   -- Could be more efficient, I know.
  a :: Double <- fmap fromIntegral randomWord32
  b :: Double <- fmap fromIntegral randomWord32
  pure (min a b / (max a b + 1))

-- | 32 bytes
bin_put_SHA256 :: Cry.Digest Cry.SHA256 -> Bin.Put
bin_put_SHA256 = traverse_ Bin.put . ByteArray.unpack

-- | 32 bytes
bin_get_SHA256 :: Bin.Get (Cry.Digest Cry.SHA256)
bin_get_SHA256 = do
   words <- sequenceA (replicate 32 Bin.get)
   let bs = ByteArray.pack words :: B.ByteString
   case Cry.digestFromByteString bs of
      Just x -> pure x
      Nothing -> mzero

mkNodeId :: (NS.HostName, NS.ServiceName) -> D.NodeId
mkNodeId (h, p) = P2P.makeNodeId (h ++ ":" ++ p)

-- | Gets all of the data in the queue until the queue is drained.
readTQueueAll :: STM.TQueue a -> STM.STM [a]
readTQueueAll tq = STM.tryReadTQueue tq >>= \case
   Nothing -> pure []
   Just a -> fmap (a:) (readTQueueAll tq)

-- | Prints a string to 'Sys.stderr'
debug :: MonadIO m => String -> m ()
debug s = liftIO (Sys.hPutStrLn Sys.stderr s)

-- | Split a 'CR.DRG' into two independent generators.
drgSplit :: CR.DRG g => g -> (CR.ChaChaDRG, g)
drgSplit drg0 = CR.withDRG drg0 CR.drgNew

