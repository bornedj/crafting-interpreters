{
  description = "Following the crafting interpreters book";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils = {
        url = "github:numtide/flake-utils";
        inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, flake-utils, ... }: 
    flake-utils.lib.eachDefaultSystem (system: let 
        pkgs = import nixpkgs { inherit system; };
        rustPlatform = pkgs.rustPlatform;
    in {
        packages.default = rustPlatform.buildRustPackage {
            pname = "crafting-interpreters";
            version = "0.1.0";

            src = ./.;
            cargoLock.lockFile = ./Cargo.lock;
            cargoBuildFlags = [
                "--workspace"
            ];
            packages = with pkgs; [
                rustc
                cargo
                rustfmt
                clippy
                rust-analyzer
            ];
        };
    })
  ;
}

