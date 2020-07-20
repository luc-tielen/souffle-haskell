{ compiler ? "ghc881", pkgs ? import ./nix/packages.nix { } }:

with pkgs;

let
  inherit (haskell.lib) dontCheck doJailbreak;
  haskellPackages = haskell.packages.${compiler};
  souffle-master = callPackage ./nix/souffle.nix {};
  haskellPkgs = haskellPackages.override {
    overrides = self: super: {
      neat-interpolation = dontCheck super.neat-interpolation;
      type-errors-pretty = doJailbreak super.type-errors-pretty;
      hspec-hedgehog = builtins.fetchTarball {
        url = "https://github.com/parsonsmatt/hspec-hedgehog/archive/0.0.1.1.tar.gz";
        sha256 = "sha256:1058x99b8sgqywrps36ajkib72d3qym3df1bypyd45vffz71fxb0";
      };
    };
  };
  source = nix-gitignore.gitignoreSource [ ] ./.;
  drv = haskellPkgs.callCabal2nix "souffle-haskell" source { };
in {
  souffle-haskell = drv;
  souffle-shell = haskellPkgs.shellFor {
    packages = p: [ drv ];
    buildInputs = with haskellPkgs; [
      pkgs.which
      souffle-master
      cabal-install
      hpack
      hlint
      ghcid
    ];
    withHoogle = true;
  };
  souffle = souffle-master;
}
