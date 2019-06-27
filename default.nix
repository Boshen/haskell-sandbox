let
  bootstrap = import <nixpkgs> { };

  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs-channels";
    inherit (nixpkgs) rev sha256;
  };

  pkgs = import src { };

  project = pkgs.haskellPackages.callPackage ./project.nix { };

in pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskell.lib.justStaticExecutables haskellPackages.cabal2nix)
    (haskell.lib.justStaticExecutables haskellPackages.hpack)
  ];
  inputsFrom = [ project.env ];
}
