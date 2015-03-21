{ }:

with import <nixpkgs> {};
let haskellPackages = pkgs.haskellPackages_ghcjs.override {
      extension = self: super: {
        oHm = self.callPackage ./. {};
        lei = self.callPackage ./lei {};
        ghcjsDom = self.callPackage ../ghcjs-dom.git {};
        ghcjsBase = self.callPackage ../ghcjs-base.git {};
        ghcjsPrim = self.callPackage ../ghcjs-prim.git {};
      };
    };

in pkgs.callPackage ./. {
     cabal = haskellPackages.cabal.override {
       extension = self: super: {
         buildTools = super.buildTools ++ [ haskellPackages.ghc.ghc.parent.cabalInstall ];
       };
     };
     inherit (haskellPackages) aeson ghcjsBase ghcjsDom ghcjsPrim mmorph
                               lens pipes pipesConcurrency profunctors stm time
                               lei;
   }
