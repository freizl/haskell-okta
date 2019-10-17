let
  pkgs = import <nixpkgs> { };

in
  { login-with-okta = pkgs.haskellPackages.callPackage ./examples/login-with-okta/login-with-okta.nix { } ;
  }
