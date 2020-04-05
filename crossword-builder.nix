{ mkDerivation, base, containers, miso, protolude, stdenv, text
, time
}:
mkDerivation {
  pname = "crossword-builder";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers protolude text time ];
  executableHaskellDepends = [
    base containers miso protolude text time
  ];
  testHaskellDepends = [ base containers protolude text time ];
  homepage = "https://github.com/janeymunoz/crossword-builder#readme";
  license = stdenv.lib.licenses.bsd3;
}
