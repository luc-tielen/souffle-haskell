{ compiler ? "ghc881", pkgs ? import ./nix/packages.nix { } }:

with pkgs;

let
  inherit (haskell.lib) dontCheck doJailbreak;
  haskellPackages = haskell.packages.${compiler};
  hpack = haskellPackages.callPackage ./nix/hpack.nix { };
  souffle-master = callPackage ./nix/souffle.nix {};
  haskellPkgs = haskellPackages.override {
    overrides = self: super: {
      hedgehog = self.hedgehog_1_0_1;
      hpack = self.callCabal2nix "hpack" hpack { };
      neat-interpolation = dontCheck super.neat-interpolation;
      type-errors-pretty = doJailbreak super.type-errors-pretty;
    };
  };
  # We have to call hpack now ourselves since current hpack is broken on ghc881
  hpack2cabal = name: src:
  pkgs.runCommand "hpack2cabal-${name}" { } ''
    ${hpack}/bin/hpack '${src}' - > "$out"
  '';
  source = nix-gitignore.gitignoreSource [ ] ./.;
  processedSource = hpack2cabal "souffle-haskell" source;
  drv = haskellPkgs.callCabal2nix "souffle-haskell" processedSource { };
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
