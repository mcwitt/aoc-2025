{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      forAllSystems' = nixpkgs.lib.genAttrs systems;
      mkPkgs = system: nixpkgs.legacyPackages.${system};
      forAllSystems = f: forAllSystems' (system: f (mkPkgs system) system);
    in
    {
      devShells = forAllSystems (
        pkgs: system: {
          default = pkgs.haskellPackages.shellFor {
            packages =
              ps:
              let
                package = ps.callCabal2nix "aoc" ./. { };
              in
              [ package ];

            withHoogle = true;

            buildInputs =
              with pkgs.haskellPackages;
              [
                cabal-fmt
                haskell-language-server
                hlint
                ormolu
              ]
              ++ [
                pkgs.aoc-cli
              ];
          };
        }
      );
    };
}
