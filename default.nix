{ pkgs ? import <nixpkgs> { } }:
let
  project = pkgs.haskellPackages.callPackage ./project.nix { };
in pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskell.lib.justStaticExecutables haskellPackages.cabal2nix)
    (haskell.lib.justStaticExecutables haskellPackages.hpack)
    (haskell.lib.justStaticExecutables haskellPackages.ghcid)
  ];
  inputsFrom = [ project.env ];
}
