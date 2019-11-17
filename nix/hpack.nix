{ mkDerivation, aeson, base, bifunctors, bytestring, Cabal
, containers, cryptonite, deepseq, directory, filepath, Glob
, hspec, hspec-discover, http-client, http-client-tls, http-types
, HUnit, infer-license, interpolate, mockery, pretty, QuickCheck
, scientific, stdenv, template-haskell, temporary, text
, transformers, unordered-containers, vector, yaml
}:

let
  url = "https://github.com/luc-tielen/hpack.git";
  ref = "fix/cabal-3.0";
  rev = "bb984da7c3afc3930c5ccd4c0b40edaad45a67b5";
  repo = builtins.fetchGit { inherit url ref rev; };
in mkDerivation {
  pname = "hpack";
  version = "0.32.0";
  src = repo;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bifunctors bytestring Cabal containers cryptonite
    deepseq directory filepath Glob http-client http-client-tls
    http-types infer-license pretty scientific text transformers
    unordered-containers vector yaml
  ];
  libraryToolDepends = [];
  executableHaskellDepends = [
    aeson base bifunctors bytestring Cabal containers cryptonite
    deepseq directory filepath Glob http-client http-client-tls
    http-types infer-license pretty scientific text transformers
    unordered-containers vector yaml
  ];
  testHaskellDepends = [
    aeson base bifunctors bytestring Cabal containers cryptonite
    deepseq directory filepath Glob hspec http-client http-client-tls
    http-types HUnit infer-license interpolate mockery pretty
    QuickCheck scientific template-haskell temporary text transformers
    unordered-containers vector yaml
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/sol/hpack#readme";
  description = "A modern format for Haskell packages";
  license = stdenv.lib.licenses.mit;
}
