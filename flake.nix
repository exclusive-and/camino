{
    description = "Widely-applicable searching and planning algorithms";

    inputs = {
        nixpkgs.url = "nixpkgs/nixos-24.11";
        flake-parts.url = "github:hercules-ci/flake-parts";
        haskell.url = "git+https://git.computeroid.org/xand/haskell-nix";
    };

    outputs = { flake-parts, haskell, self, ... } @ inputs:
        flake-parts.lib.mkFlake { inherit inputs; } {
            imports = [ haskell.flakeModule ];

            perSystem = { pkgs, ... }: {
                haskell.default = {
                    hackage = pkgs.haskell.packages.ghc9101;
                    packages.camino.source = "${self}/camino.nix";
                };
            };
            
            systems = [ "x86_64-linux" ];
        };
}