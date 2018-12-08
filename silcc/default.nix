{ mkDerivation
, base
, bytestring
, cereal
, lens
, recursion-schemes
, vector }:
mkDerivation {
    pname = "silcc";
    version = "0.0.0.0";
    license = null;
    src = ./.;
    buildDepends = [
        base
        bytestring
        cereal
        lens
        recursion-schemes
        vector
    ];
}
