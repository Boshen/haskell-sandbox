{ mkDerivation, base, foundation, hpack, hspec, hspec-discover, mtl
, parsec, stdenv, text
}:
mkDerivation {
  pname = "template";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base foundation mtl parsec text ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base foundation mtl parsec text ];
  testHaskellDepends = [ base foundation hspec mtl parsec text ];
  testToolDepends = [ hspec-discover ];
  preConfigure = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
