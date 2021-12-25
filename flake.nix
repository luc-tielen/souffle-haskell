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
        mkSouffle = { p, gcc ? 10 }:
          p.lib.makeOverridable ({ stdenv ? p."gcc${toString gcc}Stdenv" }:
            stdenv.mkDerivation {
              pname = "souffle";
              version = "2.1";
              src = p.fetchFromGitHub {
                owner = "souffle-lang";
                repo = "souffle";
                rev = version;
                sha256 = "11x3v78kciz8j8p1j0fppzcyl2lbm6ib4svj6a9cwi836p9h3fma";
              };
              postPatch = ''
                substituteInPlace CMakeLists.txt \
                  --replace "DESTINATION \''${BASH_COMPLETION_COMPLETIONSDIR}" "DESTINATION $out/share/completions/"
              '';
              postInstall = ''
                wrapProgram "$out/bin/souffle" --prefix PATH : "${
                  p.lib.makeBinPath [ p.mcpp ]
                }"
              '';
              nativeBuildInputs = with p; [
                bison
                bash-completion
                flex
                mcpp
                doxygen
                graphviz
                makeWrapper
                perl
                cmake
                ninja
                git
                lsb-release
              ];
              buildInputs = with p; [ ncurses zlib sqlite libffi ];
              propagatedBuildInputs = with p; [ ncurses zlib sqlite libffi ];
              outputs = [ "out" ];
            });
        overlay = final: _:
          with final;
          with haskellPackages.extend (_: _: rec { }); rec {
            souffle = callPackage (mkSouffle {
              p = final;
              gcc = 10;
            }) { };
            souffle-haskell = with haskell.lib;
              dontCheck
              ((addBuildTools (callCabal2nix "souffle-haskell" ./. { })
                [ souffle ]).overrideAttrs
                (o: { version = "${o.version}.${version}"; }));
          };
        overlays = [ overlay hls.overlay ];
      in with (import np { inherit system config overlays; }); rec {
        inherit overlays;
        packages = flattenTree (recurseIntoAttrs { inherit souffle-haskell; });
        defaultPackage = packages.souffle-haskell;
        devShell = with haskellPackages;
          shellFor {
            packages = p: with p; [ ];
            buildInputs = [ cabal-install ghc haskell-language-server souffle ];
          };
      });
}
