let
    overlays = {
        haskellAddons = final: prev: {
            haskellPackages = prev.haskellPackages.extend (import ./haskell-addons.nix);
        };
    };

    composeExtensions = f: g: final: prev:
        let
            applied = f final prev;
            prev'   = prev // applied;
        in
            applied // g final prev';

    composeManyExtensions = builtins.foldl' composeExtensions (_: _: {});

    default = composeManyExtensions (with overlays; [
        haskellAddons
    ]);
in
    overlays // { inherit default; }