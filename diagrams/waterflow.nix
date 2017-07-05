{ mkDerivation, base, containers, diagrams, diagrams-lib
, diagrams-svg, directory, lens, mtl, stdenv, SVGFonts
}:
mkDerivation {
  pname = "waterflow";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers diagrams diagrams-lib diagrams-svg directory lens
    mtl SVGFonts
  ];
  license = stdenv.lib.licenses.bsd3;
}
