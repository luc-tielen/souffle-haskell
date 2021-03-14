let
  commit = "6be212cce73f4ce4f65375fbdebcf0b87b182bc0";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-20.09";
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "1m1171q3dk5d57xi8rnpsg10dpwy5q5lw5xxgplzhzyrcpjn1433";
  };
  pkgs = import nixpkgs;
in
  pkgs
