let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          login-with-okta =
            haskellPackagesNew.callPackage ./examples/login-with-okta/login-with-okta.nix { };

          jose =
            pkgs.haskell.lib.dontCheck haskellPackagesOld.jose;

          lens =
            haskellPackagesNew.callPackage ./lens.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { login-with-okta = pkgs.haskellPackages.login-with-okta;
  }
