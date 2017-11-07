{ mkDerivation, base, binary, bytestring, containers, cryptonite
, distributed-process, distributed-process-p2p, exceptions, memory
, network, optparse-applicative, stdenv, stm, time, vector
}:
mkDerivation {
  pname = "powot";
  version = "0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary bytestring containers cryptonite distributed-process
    distributed-process-p2p exceptions memory network
    optparse-applicative stm time vector
  ];
  homepage = "https://github.com/k0001/powot";
  description = "Proot-of-Waste-of-Time";
  license = stdenv.lib.licenses.asl20;
}
