{ mkDerivation, base, colour, comonad, foundation, hpack, hspec
, hspec-discover, mtl, parsec, repa, repa-devil, stdenv, text
, vector
}:
mkDerivation {
  pname = "template";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base colour comonad foundation mtl parsec repa repa-devil text
    vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base colour comonad foundation mtl parsec repa repa-devil text
    vector
  ];
  testHaskellDepends = [
    base colour comonad foundation hspec mtl parsec repa repa-devil
    text vector
  ];
  testToolDepends = [ hspec-discover ];
  preConfigure = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
