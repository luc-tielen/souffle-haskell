{ pkgs, cc ? 10, ... }:

with pkgs;
lib.makeOverridable ({ stdenv ? (if pkgs.stdenv.isDarwin then
  pkgs."llvmPackages_${toString cc}".stdenv
else
  pkgs."gcc${toString cc}Stdenv") }:
  stdenv.mkDerivation rec {
    pname = "souffle";
    version = "2.2";
    src = fetchFromGitHub {
      owner = "souffle-lang";
      repo = "souffle";
      rev = version;
      sha256 = "15c8wnv0i2sbzys8m4f6k74d59ahf50xl7wcfc5v8pgx6bwc46y2";
    };
    patches = [ ../patches/1-souffle-2.1-macosx.patch ];
    cmakeFlags = [ "-DSOUFFLE_GIT=OFF" "-DSOUFFLE_BASH_COMPLETION=OFF" ];
    ninjaFlags = [ "-v" ];
    postPatch = ''
      substituteInPlace CMakeLists.txt \
        --replace "DESTINATION \''${BASH_COMPLETION_COMPLETIONSDIR}" "DESTINATION $out/share/completions/"
    '';
    postInstall = ''
      wrapProgram "$out/bin/souffle" --prefix PATH : "${
        lib.makeBinPath [ mcpp ]
      }"
    '';
    nativeBuildInputs = with pkgs;
      [ bison cmake flex git mcpp makeWrapper ninja perl ]
      ++ (lib.optionals stdenv.isDarwin)
      [ pkgs."llvmPackages_${toString cc}".bintools-unwrapped ]
      ++ (lib.optionals stdenv.isLinux [ lsb-release ]);
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
