{
  description = "Zig bindings for quickjs-ng";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    zig.url = "github:mitchellh/zig-overlay";

    flake-compat = {
      url = github:edolstra/flake-compat;
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  } @ inputs: let
    overlays = [
      (final: prev: {
        zigpkgs = inputs.zig.packages.${prev.system};
        zig = inputs.zig.packages.${prev.system}."0.17.0";
      })
    ];

    systems = builtins.attrNames inputs.zig.packages;
  in
    flake-utils.lib.eachSystem systems (
      system: let
        pkgs = import nixpkgs {inherit overlays system;};
      in rec {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs;
            [
              typos
              zig
            ]
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
              qemu
            ];
        };

        devShell = self.devShells.${system}.default;
      }
    );
}
