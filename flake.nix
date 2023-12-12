{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let pkgs = nixpkgs.legacyPackages."aarch64-darwin";
    in {
      devShell."aarch64-darwin" = pkgs.mkShell {
        buildInputs = with pkgs; [
          clojure
          nodejs
        ];
      };
    };
}
