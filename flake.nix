{
  description = "A very basic flake";

  nixConfig = {
    extra-experimental-features = [ "nix-command" "flakes" "ca-derivations" ];
    extra-substituters = [ "https://cache.iog.io" "https://public-plutonomicon.cachix.org" "https://mlabs.cachix.org" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "public-plutonomicon.cachix.org-1:3AKJMhCLn32gri1drGuaZmFrmnue+KkKrhhubQk/CWc=" ];
    allow-import-from-derivation = "true";
    max-jobs = "auto";
    auto-optimise-store = "true";
  };

  inputs = {
    nixpkgs.follows = "liqwid-nix/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";

    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix/seungheonoh/regularHask";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };
  };

  outputs = { self, liqwid-nix, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      imports = [
        liqwid-nix.haskell
        liqwid-nix.ci
        liqwid-nix.run
        ({ self, ... }:
          {
            perSystem = { config, pkgs', self', inputs, system, ... }:
              let
                pkgs = import self.inputs.nixpkgs {
                  inherit system;
                };
              in
              {
                haskell.default = {
                  src = ./.;
                  ghc.version = "ghc925";
                  shell = {
                    extraCommandLineTools =
                      with pkgs; [
                        openscad
                      ];
                  };
                  enableBuildChecks = true;
                  extraHackageDeps = [ ];
                };
                ci.required = [ "all_haskell" ];
                run = {
                  watch = {
                    dependencies = [ pkgs.ghcid ];
                    script = ''
                      MODULE=$1
                      shift
                      if [ "$MODULE" -eq "" ]; then
                        ghcid -c "cabal repl hscad" "$@"
                      else
                        ghcid -c "cabal repl $MODULE" "$@"
                      fi
                    '';
                    doCheck = false;
                  };
                };
              };
          })
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      perSystem = { config, self', inputs', pkgs, system, ... }: { };
    };
}
