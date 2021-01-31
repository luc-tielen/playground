{ compiler ? "ghc881", pkgs ? import ./nix/packages.nix { } }:

with pkgs;

let
  inherit (haskell.lib) dontCheck doJailbreak addBuildTools;
  haskellPackages = haskell.packages.${compiler};
  souffle = callPackage ./nix/souffle.nix {};
  haskellPkgs = haskellPackages.override {
    overrides = self: super: {
      neat-interpolation = dontCheck super.neat-interpolation;
    };
  };
  source = nix-gitignore.gitignoreSource [ ] ./.;
  playground = haskellPkgs.callCabal2nix "playground" source {};
  drv = (addBuildTools playground [souffle pkgs.which]);
in {
  playground = drv;
  playground-shell = haskellPkgs.shellFor {
    packages = p: [ drv ];
    buildInputs = with haskellPkgs; [
      cabal-install
      hpack
      hlint
      ghcid
    ];
    withHoogle = true;
  };
}
