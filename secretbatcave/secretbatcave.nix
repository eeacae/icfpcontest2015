{ mkDerivation, aeson, array, base, bytestring, containers, deepseq
, directory, fgl, hspec, mtl, optparse-applicative, parallel
, QuickCheck, stdenv, text, vector, vector-algorithms
, vector-th-unbox
}:
mkDerivation {
  pname = "secretbatcave";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson array base bytestring containers deepseq fgl mtl
    optparse-applicative parallel QuickCheck text vector
    vector-algorithms vector-th-unbox
  ];
  testDepends = [ array base directory hspec mtl QuickCheck ];
  license = stdenv.lib.licenses.unfree;
}
