{
  description = "Souffle Datalog bindings for Haskell";
  inputs = {
    np.url = "github:nixos/nixpkgs?ref=master";
    fu.url = "github:numtide/flake-utils?ref=master";
    hls.url = "github:haskell/haskell-language-server?ref=master";
  };
  outputs = { self, np, fu, hls }:
    with fu.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let
        version = with np.lib;
          "${substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
        config = { };
        overlay = final: _:
          with final;
          with haskellPackages.extend (final: _: { }); {
            souffle-haskell = with haskell.lib;
              dontCheck ((callCabal2nix "souffle-haskell" ./. { }).overrideAttrs
                (o: { version = "${o.version}.${version}"; }));
          };
        overlays = [ overlay hls.overlay ];
      in with (import np { inherit system config overlays; }); rec {
        inherit overlays;
        packages = flattenTree (recurseIntoAttrs { inherit souffle-haskell; });
        defaultPackage = packages.souffle-haskell;
      });
}
