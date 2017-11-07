{ nixpkgs ? (import <nixpkgs> {}).fetchFromGitHub
      { owner = "NixOS";
        repo = "nixpkgs-channels";
        rev = "c99239bca0";
        sha256 = "1d3hwaflsyb6vj2czj3jpaxvdmsr448sd0536lhaillvsm087y0g"; }
, pkgs ? import nixpkgs {}
}:

let drv = pkgs.haskell.packages.ghc802.callPackage ./pkg.nix {};
in if pkgs.lib.inNixShell then drv.env else drv

