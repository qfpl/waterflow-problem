{ mkDerivation, base, diagrams, diagrams-lib, diagrams-svg, stdenv
, SVGFonts
}:
mkDerivation {
  pname = "waterflow";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base diagrams diagrams-lib diagrams-svg SVGFonts
  ];
  license = stdenv.lib.licenses.bsd3;
}
