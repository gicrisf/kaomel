{
  description = "kaomel — a snappy kaomoji picker for Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        eldev = pkgs.stdenv.mkDerivation {
          pname = "eldev";
          version = "1.10";
          src = pkgs.fetchFromGitHub {
            owner = "doublep";
            repo = "eldev";
            rev = "1.10";
            sha256 = "sha256-9x9KaeMCf3Zyf/fTq/2HwMCM1uDrnvQsWUUcXq3R0ws=";
          };
          buildInputs = [ pkgs.emacs ];
          installPhase = ''
            mkdir -p $out/bin
            cp bin/eldev $out/bin/
            chmod +x $out/bin/eldev
          '';
        };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [ pkgs.emacs eldev ];
          shellHook = ''
            echo "kaomel development shell"
            echo "Run 'eldev build' to regenerate kaomel-data.el"
            echo "Run 'eldev test'  to run the test suite"
          '';
        };
      });
}
