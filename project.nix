{ mkDerivation, base, foundation, hpack, hspec, hspec-discover
, stdenv
}:
mkDerivation {
  pname = "template";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base foundation ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base foundation ];
  testHaskellDepends = [ base foundation hspec ];
  testToolDepends = [ hspec-discover ];
  preConfigure = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
