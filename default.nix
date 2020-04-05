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
    };
  };
  source = nix-gitignore.gitignoreSource [ ] ./.;
  drv = haskellPkgs.callCabal2nix "souffle-haskell" source { };
in {
  souffle-haskell = drv;
  souffle-shell = haskellPkgs.shellFor {
    packages = p: [ drv ];
    buildInputs = with haskellPkgs; [
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
