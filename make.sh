#!/bin/sh

nix-shell \
  -I nixpkgs=/nix/var/nix/profiles/per-user/k/channels/nixpkgs-unstable \
  -I . \
  shell.nix \
  --command 'cabal configure --ghcjs && cabal build'
