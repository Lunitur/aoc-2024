{
  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-24.11";
    };
  };

  outputs =
    { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
    in
    {
      devShells."x86_64-linux".default = pkgs.mkShell {
        buildInputs = with pkgs; [
          haskell-language-server
          cabal-install
          (ghc.withPackages (hs: with hs; [ raw-strings-qq ]))
          ghcid
          # ghcide
        ];

        # shellHook = ''
        # '';
      };
    };
}
