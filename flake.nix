# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0
{
  description = "Lambda Calculus-like thing in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};

      haskellPackages = pkgs.haskellPackages;

      jailbreakUnbreak = pkg:
        pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: {meta = {};}));

      packageName = "lc-hs";
    in {
      packages.${packageName} =
        haskellPackages.callCabal2nix packageName self {
        };

      packages.default = self.packages.${system}.${packageName};
      defaultPackage = self.packages.${system}.default;

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          haskellPackages.haskell-language-server # you must build it with your ghc to work
          cabal-install
        ];
        inputsFrom = builtins.attrValues self.packages.${system};
      };
      devShell = self.devShells.${system}.default;
    });
}
