{ compiler ? "ghc881", pkgs ? import ./nix/packages.nix {} }:

(import ./. { inherit pkgs compiler; }).souffle-shell
