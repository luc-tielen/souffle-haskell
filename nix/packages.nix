let
  commit = "ef955ecb1ea10aaa95d7221048e1f30180929116";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-20.03";
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "07xgr7kj3gnwbvnmx6jwwp2p7a0hjnxcrx99j3k4bcj6ki8x2rk3";
  };
  pkgs = import nixpkgs;
in
  pkgs
