let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOs/nixpkgs/archive/18038cee44aa0c3c99a2319c3c1c4d16d6612d81.tar.gz";

    sha256 = "0b1c2sj6k72nwwny1ibayl2hfgwk3kdwksiijpjl4w48s8s4p5di";
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
