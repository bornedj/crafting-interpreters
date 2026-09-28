{
  description = "Following crafting interpreters";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
    hp = pkgs.haskellPackages;
    jlox = hp.callCabal2nix "jlox" ./. {};
  in {

    packages.${system}.default = jlox;
    devShells.${system}.default = hp.shellFor {
        packages = _: [ jlox ];
        nativeBuildInputs = [
            pkgs.cabal-install
            hp.hoogle
        ];
    };
  };
}
