{ compiler ? "ghc881", pkgs ? import ./nix/packages.nix { } }:

with pkgs;

let
  inherit (haskell.lib) dontCheck doJailbreak addBuildTools;
  haskellPackages = haskell.packages.${compiler};
  souffle = callPackage ./nix/souffle.nix {};
  souffle-haskell-src = pkgs.fetchFromGitHub {
    owner = "luc-tielen";
    repo  = "souffle-haskell";
    rev = "1f8970e534b8db03cef7f277062a61717b3db543";
    sha256 = "0ml21yfdrkzn41yhrsl6v7akwspwydidll8lxcrm8yld4hr2z89c";
  };
  haskellPkgs = haskellPackages.override {
    overrides = self: super: {
      neat-interpolation = dontCheck super.neat-interpolation;
      souffle-haskell = dontCheck (super.callCabal2nix "souffle-haskell" souffle-haskell-src {});
      hspec-hedgehog = self.callCabal2nix "hspec-hedgehog" (builtins.fetchTarball {
        url = "https://github.com/parsonsmatt/hspec-hedgehog/archive/0.0.1.1.tar.gz";
        sha256 = "sha256:1058x99b8sgqywrps36ajkib72d3qym3df1bypyd45vffz71fxb0";
      }) {};
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
