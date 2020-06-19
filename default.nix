let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOs/nixpkgs/archive/312a059bef8b29b4db4e73dc02ff441cab7bb26d.tar.gz";

    sha256 = "1j52yvkhw1inp6ilpqy81xv1bbwgwqjn0v9647whampkqgn6dxhk";
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
