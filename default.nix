{ cabal, aeson, ghcjsBase, ghcjsDom, ghcjsPrim, lens, pipes
, pipesConcurrency, profunctors, stm, time, lei
}:

cabal.mkDerivation (self: {
  pname = "oHm";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    aeson ghcjsBase ghcjsDom ghcjsPrim lens pipes pipesConcurrency
    profunctors stm time lei
  ];
  meta = {
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
