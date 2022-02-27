{
  description = "playground";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/a3d847c3bd3a3b75b3057d7b3730d3308dd8fd59";
    flake-utils.url = "github:numtide/flake-utils";
    souffle-haskell.url = "github:luc-tielen/souffle-haskell";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, souffle-haskell, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [ ]
          ++ souffle-haskell.overlays.${system} # bring in souffle/souffle-haskell
          ++ [
          (self: super: {
            haskellPackages = super.haskellPackages.override {
              overrides = hself: hsuper: {
                souffle-haskell = super.souffle-haskell;
              };
            };
          })
        ]; # this overlay is necessary because github:luc-tielen/souffle-haskell doesn't stick these in haskellPackages

        pkgs =
          import nixpkgs { inherit system overlays; config.allowBroken = true; };

          project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "playground";
            root = ./.;
            withHoogle = false;
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
                (with pkgs.haskellPackages; [
                  # Specify your build/dev dependencies here.
                  cabal-fmt
                  cabal-install
                  ghcid
                  haskell-language-server
                  ormolu
                  pkgs.souffle

                  pkgs.nixpkgs-fmt
                ]);
          };
      in
      {
        # Used by `nix build` & `nix run` (prod exe)
        defaultPackage = project false;

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}
