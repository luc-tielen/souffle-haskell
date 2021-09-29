
{ stdenv, fetchFromGitHub
, perl, ncurses, zlib, sqlite, libffi
, mcpp, bison, flex, doxygen, graphviz
, cmake, makeWrapper, git, lsb-release
}:

let
  toolsPath = stdenv.lib.makeBinPath [ mcpp ];
in
stdenv.mkDerivation rec {
  pname = "souffle";
  version = "2.1";

  src = fetchFromGitHub {
    owner  = "souffle-lang";
    repo   = "souffle";
    rev    = version;
    sha256 = "11x3v78kciz8j8p1j0fppzcyl2lbm6ib4svj6a9cwi836p9h3fma";
  };

  nativeBuildInputs = [ bison flex mcpp doxygen graphviz makeWrapper perl ];
  buildInputs = [ git ncurses zlib sqlite libffi cmake lsb-release ];

  # these propagated inputs are needed for the compiled Souffle mode to work,
  # since generated compiler code uses them. TODO: maybe write a g++ wrapper
  # that adds these so we can keep the propagated inputs clean?
  propagatedBuildInputs = [ ncurses zlib sqlite libffi ];

  #patchPhase = ''
  #  substituteInPlace CMakeLists.txt --replace 'set(PACKAGE_VERSION "UNKNOWN")' 'set(PACKAGE_VERSION "${version}")'
  #'';

  configurePhase = ''
    mkdir build
    cd build
    cmake ..
  '';

  buildPhase = ''
    make
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp -r ./src/souffle $out/bin/
    wrapProgram "$out/bin/souffle" --prefix PATH : "${toolsPath}"
  '';

  dontFixCmake = true;

  outputs = [ "out" ];

  meta = with stdenv.lib; {
    description = "A translator of declarative Datalog programs into the C++ language";
    homepage    = "http://souffle-lang.github.io/";
    platforms   = platforms.unix;
    maintainers = with maintainers; [ thoughtpolice copumpkin wchresta ];
    license     = licenses.upl;
  };
}
