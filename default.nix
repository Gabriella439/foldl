{ mkDerivation, base, bytestring, comonad, containers
, contravariant, mwc-random, primitive, profunctors, stdenv, text
, transformers, vector
}:
mkDerivation {
  pname = "foldl";
  version = "1.2.3";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring comonad containers contravariant mwc-random
    primitive profunctors text transformers vector
  ];
  description = "Composable, streaming, and efficient left folds";
  license = stdenv.lib.licenses.bsd3;
}
