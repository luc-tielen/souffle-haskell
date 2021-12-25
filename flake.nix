{
  description = "Souffle Datalog bindings for Haskell";
  inputs = {
    np.url = "github:nixos/nixpkgs?ref=haskell-updates";
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
          with haskellPackages.extend (_: _: rec { }); rec {
            souffle =
              callPackage (import ./nix/souffle.nix { pkgs = final; }) { };
            souffle-haskell = with haskell.lib;
              (overrideCabal (addBuildDepends
                (addBuildTools (callCabal2nix "souffle-haskell" ./. { }) [
                  hpack
                  souffle
                ]) [ makeWrapper ]) (o: {
                  version = "${o.version}.${version}";
                  doCheck = true;
                  checkPhase = ''
                    runHook preCheck
                    DATALOG_DIR="${o.src}/tests/fixtures/" SOUFFLE_BIN="${souffle}/bin/souffle" ./Setup test
                    runHook postCheck
                  '';
                }));
          };
        overlays = [ overlay hls.overlay ];
      in with (import np { inherit system config overlays; }); rec {
        inherit overlays;
        packages = flattenTree (recurseIntoAttrs { inherit souffle-haskell; });
        defaultPackage = packages.souffle-haskell;
        devShell = with haskellPackages;
          shellFor {
            packages = p: with p; [ hspec-discover ];
            buildInputs = [
              cabal-install
              ghc
              haskell-language-server
              hspec-discover
              souffle
              packages.souffle-haskell
            ];
          };
      });
}
