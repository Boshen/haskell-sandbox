{ mkDerivation, base, comonad, foundation, hpack, hspec
, hspec-discover, megaparsec, mtl, parsec, stdenv, text, vector
}:
mkDerivation {
  pname = "template";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base comonad foundation megaparsec mtl parsec text vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base comonad foundation megaparsec mtl parsec text vector
  ];
  testHaskellDepends = [
    base comonad foundation hspec megaparsec mtl parsec text vector
  ];
  testToolDepends = [ hspec-discover ];
  preConfigure = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
