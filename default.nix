{ pkgs ? import <nixpkgs> {} }:
let
  dependencies = import (import ./project.nix {}) {};
  weather-api =
    pkgs.stdenv.mkDerivation {
      name = "weather-api";
      buildInputs = [
        pkgs.haskellPackages.ghc
      ];
      phases = [ "unpackPhase" "buildPhase" "installPhase" ];
      src = [
        ./src
        ./app
        ./prod.dhall
        ./weather-api.cabal
      ];

      unpackPhase = ''
        for srcFile in $src; do
          echo "----"
          echo $srcFile
          ls $srcFile
        done

        for srcFile in $src; do
          cp -r $srcFile $(stripHash $srcFile)
        done
      '';

      buildPhase = ''
        ${pkgs.cabal-install}/bin/cabal build
      '';

      installPhase = ''
        #cp -r dist $out/
      '';
    };
in
{
  weather-api = weather-api;
}
