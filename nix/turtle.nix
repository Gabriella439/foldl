{ mkDerivation, base, bytestring, comonad, containers
, contravariant, criterion, hashable, mwc-random, primitive
, profunctors, semigroups, stdenv, text, transformers
, unordered-containers, vector, vector-builder
}:
mkDerivation {
  pname = "foldl";
  version = "1.4.1";
  src = ./..;
  libraryHaskellDepends = [
    base bytestring comonad containers contravariant hashable
    mwc-random primitive profunctors semigroups text transformers
    unordered-containers vector vector-builder
  ];
  benchmarkHaskellDepends = [ base criterion ];
  description = "Composable, streaming, and efficient left folds";
  license = stdenv.lib.licenses.bsd3;
}
