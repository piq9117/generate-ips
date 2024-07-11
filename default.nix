{ mkDerivation, async, base, conduit, lib, optparse-applicative
, random, relude, resourcet, stm, stm-conduit
}:
mkDerivation {
  pname = "generate-ips";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base conduit optparse-applicative random relude resourcet stm
    stm-conduit
  ];
  executableHaskellDepends = [ base relude ];
  license = "unknown";
  mainProgram = "generate-ips";
}
