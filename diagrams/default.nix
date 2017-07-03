{ mkDerivation, base, diagrams, diagrams-lib, diagrams-svg
, directory, stdenv, SVGFonts, text
}:
mkDerivation {
  pname = "waterflow";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base diagrams diagrams-lib diagrams-svg directory SVGFonts text
  ];
  license = stdenv.lib.licenses.bsd3;
}
