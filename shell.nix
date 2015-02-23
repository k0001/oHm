{ }:

with import <nixpkgs> {};
let haskellPackages = pkgs.haskellPackages_ghcjs.override {
      extension = self: super: {
        oHm = self.callPackage ./. {};
        lei = self.callPackage ./lei {};
      };
    };

in pkgs.callPackage ./. {
     cabal = haskellPackages.cabal.override {
       extension = self: super: {
         buildTools = super.buildTools ++ [ haskellPackages.ghc.ghc.parent.cabalInstall ];
       };
     };
     inherit (haskellPackages) aeson ghcjsBase ghcjsDom ghcjsPrim lens pipes
                               pipesConcurrency profunctors stm time lei;
   }
