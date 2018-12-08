{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
{
    silcc = nixpkgs.haskellPackages.callPackage ./silcc {};
}
