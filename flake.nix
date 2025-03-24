{
    description = "Widely-applicable searching and planning algorithms";

    inputs = {
        nixpkgs.url = "nixpkgs/nixos-24.11";

        haskellNix = {
            url = "git+https://git.computeroid.org/xand/haskell-nix";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = { nixpkgs, haskellNix, self } @ inputs:
    let
        overlays = [
            haskellNix.overlay
            self.overlay
        ];

        system = "x86_64-linux";

        pkgs = import nixpkgs {
            inherit overlays system;
        };

        inherit (pkgs) haskellPackages;
    in
    {
        overlays = import ./nix/overlays;

        overlay = self.overlays.default;

        packages.${system}.default = haskellPackages.camino;

        devShells.${system}.default = haskellPackages.shellFor {
            packages = final: [ final.camino ];

            nativeBuildInputs = [
                haskellPackages.cabal-install
                haskellPackages.haskell-language-server
            ];
        };
    };
}