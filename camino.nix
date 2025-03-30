{ mkDerivation
, lib
, base
, containers
, primitive
, transformers
}:

mkDerivation {
    pname   = "camino";
    version = "0.2.0.0";
    src     = ./.;
    libraryHaskellDepends = [
        base
        containers
        primitive
        transformers
    ];
    librarySystemDepends = [];
    license = lib.licenses.bsd3;
}