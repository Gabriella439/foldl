let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOs/nixpkgs/archive/5e8bfbb4c7fd0534d9aa4b1b54da278f96ee3512.tar.gz";

    sha256 = "0xjwbs706lwcgrpgl1xj1fqxb9z6i5zzijyg3crzjffp2la1aijy";
  };

  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override {
      overrides =
        let
          manualOverrides = haskellPackagesNew: haskellPackagesOld: {
          };

        in
          pkgsNew.lib.fold pkgsNew.lib.composeExtensions (_: _: {}) [
            (pkgsNew.haskell.lib.packagesFromDirectory { directory = ./nix; })
            manualOverrides
          ];
    };
  };

  pkgs = import nixpkgs { config = {}; overlays = [ overlay ]; };

in
  { inherit (pkgs.haskellPackages) foldl;

    shell = (pkgs.haskell.lib.doBenchmark pkgs.haskellPackages.foldl).env;
  }
