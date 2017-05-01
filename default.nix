{ mkDerivation, base, bytestring, comonad, containers
, contravariant, hashable, mwc-random, primitive, profunctors
, stdenv, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "foldl";
  version = "1.2.4";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring comonad containers contravariant hashable
    mwc-random primitive profunctors text transformers
    unordered-containers vector
  ];
  description = "Composable, streaming, and efficient left folds";
  license = stdenv.lib.licenses.bsd3;
}
