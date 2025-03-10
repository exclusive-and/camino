{ inputs }:

let
    computeroid = final: prev:
    {
        computeroid = final.callPackage ./computeroid.nix {};
    };

    haskell = final: prev:
    {
        haskellPackages = prev.haskellPackages.extend computeroid;
    };
in
    haskell
