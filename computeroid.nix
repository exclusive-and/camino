{ mkDerivation
, lib
, base
, containers
, primitive
, transformers
}:

mkDerivation {
    pname   = "computeroid";
    version = "0.1.0.0";
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
