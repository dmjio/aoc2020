with (import ./default.nix {});
aoc2020.env.overrideAttrs (drv: {
  shellHook = ''
    function run () {
      ${pkgs.haskellPackages.ghcid.bin}/bin/ghcid
    }
  '';
})
