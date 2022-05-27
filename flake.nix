{
  description = "Souffle Datalog bindings for Haskell";
  inputs = {
    np.url = "github:nixos/nixpkgs?ref=master";
    fu.url = "github:numtide/flake-utils?ref=master";
    ds.url = "github:numtide/devshell?ref=master";
  };
  outputs = { self, np, fu, ds }:
    with fu.lib;
    eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        ghc = "ghc902";
        version = with np.lib;
          "${substring 0 8 self.lastModifiedDate}.${ghc}.${
            self.shortRev or "dirty"
          }";
        config = { };
        overlay = final: super:
          let
            souffle = with final;
              callPackage (import ./nix/souffle.nix { pkgs = final; }) { };
            haskellPackages = super.haskell.packages.${ghc}.override {
              overrides = hself: hsuper: {
                # ghc-9.2.1: see, https://github.com/kowainik/type-errors-pretty/issues/24
                type-errors-pretty = with final.haskell.lib;
                  dontCheck (doJailbreak (hsuper.type-errors-pretty));
                souffle-haskell = with final.haskell.lib;
                  with hself;
                  (overrideCabal
                    (callCabal2nix "souffle-haskell" ./. { }) (o: {
                      version = "${o.version}.${version}";
                      # NOTE: next line needs to be changed to "doCheck = false;"
                      # when upgrading Souffle, so the test fixtures can be
                      # upgraded with the new Souffle compiler before running the tests.
                      doCheck = true;
                      checkPhase = ''
                        runHook preCheck
                        DATALOG_DIR="${o.src}/tests/fixtures/" SOUFFLE_BIN="${souffle}/bin/souffle" ./Setup test
                        runHook postCheck
                      '';
                    }));
              };
            };
            souffle-haskell-lint = with final;
              writeShellScriptBin "souffle-haskell-lint" ''
                ${hlint}/bin/hlint ${haskellPackages.souffle-haskell.src} -c
              '';
          in { inherit souffle souffle-haskell-lint haskellPackages; };
        overlays = [ overlay ds.overlay ];
      in with (import np { inherit system config overlays; }); rec {
        inherit overlay;
        packages = flattenTree (recurseIntoAttrs {
          inherit (haskellPackages) souffle-haskell;
          inherit souffle-haskell-lint;
        });
        defaultPackage = packages.souffle-haskell;
        apps = {
          souffle-haskell-lint = mkApp { drv = souffle-haskell-lint; };
        };
        devShell = devshell.mkShell {
          name = "SOUFFLE-HASKELL";
          packages = with haskellPackages; [
            cabal-install
            haskellPackages.ghc
            haskell-language-server
            hlint
            hpack
            hspec-discover
            souffle
            souffle-haskell-lint
          ];
        };
      });
}
