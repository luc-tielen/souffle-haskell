{ compiler ? "ghc881", pkgs ? import ./nix/packages.nix { } }:

with pkgs;

let
  inherit (haskell.lib) dontCheck;
  haskellPackages = haskell.packages.${compiler};
  hpack = haskellPackages.callPackage ./nix/hpack.nix { };
  souffle-fork = callPackage ./nix/souffle.nix { };
  haskellPkgs = haskellPackages.override {
    overrides = self: super: {
      hpack = self.callCabal2nix "hpack" hpack { };
      neat-interpolation = dontCheck super.neat-interpolation;
    };
  };
  # We have to call hpack now ourselves since current hpack is broken on ghc881
  hpack2cabal = name: src:
  pkgs.runCommand "hpack2cabal-${name}" { } ''
    ${hpack}/bin/hpack '${src}' - > "$out"
  '';
  source = nix-gitignore.gitignoreSource [ ] ./.;
  processedSource = hpack2cabal "souffle" source;
  drv = haskellPkgs.callCabal2nix "souffle" processedSource { };
in {
  souffle-hs = drv;
  souffle-shell = haskellPkgs.shellFor {
    packages = p: [ drv ];
    buildInputs = with haskellPkgs; [
      souffle-fork
      cabal-install
      hpack
      hlint
      ghcid
    ];
    withHoogle = true;
  };
}
