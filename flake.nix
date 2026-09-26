{
  description = "Following crafting interpreters";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    hp = pkgs.haskellPackages;
    crafting-interpreters = hp.callCabal2nix "crafting-interpreters" ./. {};
  in {

    packages.${system}.default = crafting-interpreters;
    devShells.${system}.default = hp.shellFor {
        packages = _: [ crafting-interpreters ];
        nativeBuildInputs = [
            pkgs.cabal-install
            hp.hoogle
        ];
    };
  };
}
