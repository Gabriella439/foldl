{ mkDerivation, base, bytestring, comonad, containers
, contravariant, hashable, mwc-random, primitive, profunctors
, stdenv, text, transformers, unordered-containers, vector
, vector-builder
}:
mkDerivation {
  pname = "foldl";
  version = "1.2.5";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring comonad containers contravariant hashable
    mwc-random primitive profunctors text transformers
    unordered-containers vector vector-builder
  ];
  description = "Composable, streaming, and efficient left folds";
  license = stdenv.lib.licenses.bsd3;
}
