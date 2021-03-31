{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, filepath, lens, miso
      , protolude, stdenv, text, time
      }:
      mkDerivation {
        pname = "crossword-builder";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base containers filepath lens miso protolude text
        ];
        executableHaskellDepends = [ base miso protolude ];
        testHaskellDepends = [ base containers protolude text time ];
        homepage = "https://github.com/janeymunoz/crossword-builder#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
