{
  description = "";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        devShells.default =
          with pkgs;
          pkgs.mkShell rec {
            nativeBuildInputs = [
              gnumake
              upx

              ghciwatch
              haskellPackages.fourmolu
              haskellPackages.cabal-gild
              haskellPackages.hlint

              sqlite
            ];
            buildInputs = [
              haskell.compiler.ghc914
              haskellPackages.cabal-install
              pkg-config
              zlib
              gmp
            ];

            LD_LIBRARY_PATH = "${lib.makeLibraryPath buildInputs}";

          };
      }
    );
}
