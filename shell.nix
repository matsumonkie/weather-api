{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  name = "weather-api";
  buildInputs = [
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.cabal2nix
    pkgs.postgresql
    pkgs.zlib
    pkgs.cabal-install
    pkgs.nix
  ];
}
