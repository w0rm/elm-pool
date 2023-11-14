{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = with pkgs; with elmPackages; mkShell {
          buildInputs = [
            elm
            elm-format
            elm-test
            elm-review
            elm-json
            nodePackages.uglify-js
            butler
            ghostscript # for generating ball textures
            pngquant # for minimizing ball textures
          ];
        };
      });
}
