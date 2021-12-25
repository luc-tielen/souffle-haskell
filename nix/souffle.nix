{ pkgs, gcc ? 10, ... }:

with pkgs;
lib.makeOverridable ({ stdenv ? pkgs."gcc${toString gcc}Stdenv" }:
  stdenv.mkDerivation rec {
    pname = "souffle";
    version = "2.1";
    src = fetchFromGitHub {
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
        lib.makeBinPath [ mcpp ]
      }"
    '';
    nativeBuildInputs = with pkgs; [
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
    buildInputs = with pkgs; [ ncurses zlib sqlite libffi ];
    propagatedBuildInputs = with pkgs; [ ncurses zlib sqlite libffi ];
    outputs = [ "out" ];
    meta = with lib; {
      description =
        "A translator of declarative Datalog programs into the C++ language";
      homepage = "http://souffle-lang.github.io/";
      platforms = platforms.unix;
      maintainers = with maintainers; [ thoughtpolice copumpkin wchresta ];
      license = licenses.upl;
    };
  })
