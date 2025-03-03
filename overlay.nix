let
    haskellOverlay = final: prev: {
        computeroid-search = final.callPackage ./computeroid-search.nix {};
    };
in
    final: prev: {
        haskellPackages = prev.haskell.packages.ghc9101.extend haskellOverlay;
    }