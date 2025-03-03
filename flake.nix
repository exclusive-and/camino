{
    description = "Widely-applicable searching and planning algorithms";

    inputs = {
        nixpkgs.url = "nixpkgs/nixos-24.11";
    };

    outputs = { nixpkgs, self } @ inputs:
    let
        haskellOverlay = import ./overlay.nix;
        overlays = [
            haskellOverlay
        ];

        system = "x86_64-linux";

        pkgs = import nixpkgs {
            inherit overlays system;
        };

        inherit (pkgs) haskellPackages;
    in
    {
        overlays.haskellPackages = haskellOverlay;

        devShells.${system}.default = haskellPackages.shellFor {
            packages = final: [ final.computeroid-search ];

            nativeBuildInputs = [
                haskellPackages.cabal-install
                haskellPackages.haskell-language-server
            ];
        };
    };
}