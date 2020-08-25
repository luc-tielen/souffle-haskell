let
  commit = "1177216e45d82399c2ba4cefecbf9e5cb318406c";
  nixpkgs = builtins.fetchTarball {
    name = "haskell-updates";
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "1yy26p1amvs4qkrps3fx20i9d3c6nz5bzbanvxx1zqg6y5pqm764";
  };
  pkgs = import nixpkgs;
in
  pkgs
