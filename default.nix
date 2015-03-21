{ cabal, aeson, ghcjsBase, ghcjsDom, ghcjsPrim, mmorph, lens, pipes
, pipesConcurrency, profunctors, stm, time, lei, text
}:

cabal.mkDerivation (self: {
  pname = "oHm";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    aeson ghcjsBase ghcjsDom ghcjsPrim mmorph lens pipes pipesConcurrency
    profunctors stm time lei text
  ];
  meta = {
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
