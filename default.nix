{ pkgs ? import <nixpkgs> {} }:
let
  pkg = pkgs.haskellPackages.callCabal2nix "aoc2020" ./. {};
  aoc2020 = pkg.overrideAttrs (drv: {
    postInstall = ''
      cp -v ${drv.src}/1/day1.txt $out/bin
      cp -v ${drv.src}/2/day2.txt $out/bin
      cp -v ${drv.src}/3/day3.txt $out/bin
      cp -v ${drv.src}/4/day4.txt $out/bin
      cp -v ${drv.src}/5/day5.txt $out/bin
      cp -v ${drv.src}/6/day6.txt $out/bin
      cp -v ${drv.src}/7/day7.txt $out/bin
    '';
  });
  runAoc = pkgs.writeScriptBin "run" ''
    cd ${aoc2020}/bin
    ${aoc2020}/bin/day1
    ${aoc2020}/bin/day2
    ${aoc2020}/bin/day3
    ${aoc2020}/bin/day4
    ${aoc2020}/bin/day5
    ${aoc2020}/bin/day6
    ${aoc2020}/bin/day7
  '';
in
{
  inherit aoc2020 pkgs runAoc;
}
