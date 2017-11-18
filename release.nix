# You can build this repository using Nix by running:
#
#     $ nix-build -A foldl release.nix
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell -A foldl.env release.nix
#
# ... and then Nix will supply the correct Haskell development environment for
# you
let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          foldl = haskellPackagesNew.callPackage ./default.nix { };

          # `vector-builder`'s test suite depends on `foldl`
          vector-builder = pkgs.haskell.lib.dontCheck haskellPackagesOld.vector-builder;
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { foldl = pkgs.haskellPackages.foldl;
  }
