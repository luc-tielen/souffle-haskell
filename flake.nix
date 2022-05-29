{
  description = "souffle-haskell: Souffle Datalog bindings for Haskell";

  inputs.devshell.url = "github:numtide/devshell";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-filter.url = "github:numtide/nix-filter";

  outputs = { self, flake-utils, nix-filter, devshell, nixpkgs }:
    with nixpkgs.lib;
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        rmDot = replaceStrings [ "." ] [ "" ];
        supportedGHCs = [ "default" "902" "922" ];
        config = { };
        overlays.devshell = devshell.overlay;
        overlays.default = f: p:
          let
            ghcVersion = "ghc${rmDot p.haskellPackages.ghc.version}";

            mkHaskellPackages = hspkgs:
              (hspkgs.override (old: {
                overrides = composeExtensions (old.overrides or (_: _: { }))
                  (_: hp: {
                    souffle = with f;
                      callPackage (import ./nix/souffle.nix { pkgs = f; }) { };
                  });
              })).extend (hf: hp:
                with f.haskell.lib;
                composeExtensions (hf: hp: {
                  souffle-haskell = disableLibraryProfiling
                    ((hf.callCabal2nix "souffle-haskell" (with nix-filter.lib;
                      filter {
                        root = self;
                        exclude = [ (matchExt "cabal") ];
                      }) { }).overrideAttrs (old: {
                        version = "${rmDot hp.ghc.version}-${old.version}-${
                            substring 0 8 self.lastModifiedDate
                          }.${self.shortRev or "dirty"}";
                        # NOTE: next line needs to be changed to "doCheck = false;"
                        # when upgrading Souffle, so the test fixtures can be
                        # upgraded with the new Souffle compiler before running the tests.
                        doCheck = true;
                        checkPhase = ''
                          runHook preCheck
                          DATALOG_DIR="${old.src}/tests/fixtures/" SOUFFLE_BIN="${hf.souffle}/bin/souffle" ./Setup test
                          runHook postCheck
                        '';
                      }));
                }) (hf: hp:
                  {
                    souffle-haskell-lint =
                      f.writeShellScriptBin "souffle-haskell-lint" ''
                        ${hf.hlint}/bin/hlint ${hf.souffle-haskell.src} -c
                      '';
                  } // (if "${rmDot hp.ghc.version}" == "922" then {
                    # https://github.com/kowainik/type-errors-pretty/issues/24
                    type-errors-pretty = disableLibraryProfiling (dontHaddock
                      (dontCheck (doJailbreak (hp.type-errors-pretty))));
                  } else
                    { })) hf hp);

            # all haskellPackages
            allHaskellPackages = let
              cases = listToAttrs (map (n: {
                name = "${n}";
                value = mkHaskellPackages
                  f.haskell.packages."${if n == "default" then
                    "${ghcVersion}"
                  else
                    "ghc${n}"}";
              }) supportedGHCs);
            in cases;

            # all packages
            allPackages = listToAttrs (map (n: {
              name = if n == "default" then n else "souffle-haskell-${n}";
              value = allHaskellPackages."${n}".souffle-haskell;
            }) supportedGHCs);

            # make dev shell
            mkDevShell = g:
              p.devshell.mkShell {
                name = "souffle-haskell-${
                    if g == "default" then "${ghcVersion}" else g
                  }-${substring 0 8 self.lastModifiedDate}.${
                    self.shortRev or "dirty"
                  }";
                packages = with f;
                  with f.allHaskellPackages."${g}"; [
                    ghcid
                    souffle
                    souffle-haskell-lint
                    (ghcWithPackages (hp:
                      with hp; [
                        # souffle-haskell
                        ghc
                        cabal-install
                        haskell-language-server
                        hpack
                        hspec-discover
                        hsc2hs
                      ]))
                  ];
              };

            # all packages
            allDevShells = listToAttrs (map (n: {
              name = "${n}";
              value = mkDevShell n;
            }) supportedGHCs);
          in {
            haskellPackages = allHaskellPackages.default;
            inherit allHaskellPackages allDevShells allPackages;
          };

        pkgs = import nixpkgs {
          inherit system config;
          overlays = [ overlays.devshell overlays.default ];
        };

      in with pkgs.lib; rec {
        inherit overlays;
        packages = flattenTree (pkgs.recurseIntoAttrs pkgs.allPackages);
        devShells = flattenTree (pkgs.recurseIntoAttrs pkgs.allDevShells);
      });
}
