{ }:

with import <nixpkgs> {};
let haskellPackages = pkgs.haskellPackages_ghcjs.override {
      extension = self: super: {
        oHm = self.callPackage ./. {};
        mvc = self.callPackage ./mvc.nix {};
        # cabal = self.callPackage ./cabal.nix {};
        # cabalInstall_1_22_0_0 = self.callPackage ./cabal-install.nix {};
      };
    };
in pkgs.callPackage ./. {
     cabal = haskellPackages.cabal.override {
       extension = self: super: { 
         buildTools = super.buildTools ++ [ haskellPackages.ghc.ghc.parent.cabalInstallGhcjs ]; 
       };
     };
     inherit (haskellPackages) aeson ghcjsBase ghcjsDom ghcjsPrim lens mvc pipes
                               pipesConcurrency profunctors stm time;
   }
