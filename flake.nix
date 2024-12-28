{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-24.11";
    };
  };

  outputs =
    { self, nixpkgs }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ ];
      };
    in
    {
      devShells."x86_64-linux".default = pkgs.mkShell {
        buildInputs = with pkgs; [
          haskell-language-server # .override { supportedGhcVersions = [ "96" ]; })
          cabal-install
          ghc
          ghcid

          sbt
          metals
          scalafmt

          blas
          lapack
        ];
      };
    };
}
