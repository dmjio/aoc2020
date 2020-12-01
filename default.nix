{ pkgs ? import <nixpkgs> {} }:
{
  aoc2020 = pkgs.haskellPackages.callCabal2nix "aoc2020" ./. {};
  inherit pkgs;
}
