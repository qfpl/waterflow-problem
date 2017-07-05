{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  waterflow = haskellPackages.callPackage ./waterflow.nix {};

in
  pkgs.haskell.lib.overrideCabal waterflow (drv: {
    executableToolDepends = [pkgs.pandoc];
    postInstall = ''
      mkdir -p $out
      cd $out
      $out/bin/waterflow
      pandoc slides.md -s -t slidy -o slides.html
      rm -Rf $out/bin
      rm -Rf $out/share
      rm $out/slides.md
    '';
  })

