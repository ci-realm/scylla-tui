{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc843" }:
let
  scylla = nixpkgs.haskell.packages.${compiler}.callPackage ./data-scylla.nix {};
  # testsuite fails due to no Ord for ZonedTime :X
  genvalidity-time = nixpkgs.haskell.lib.dontCheck nixpkgs.haskell.packages.${compiler}.genvalidity-time;
  pretty-relative-time = nixpkgs.haskell.packages.${compiler}.callPackage ./pretty-relative-time.nix { inherit genvalidity-time; };
  scylla-api = nixpkgs.haskell.packages.${compiler}.callPackage ./scylla-api.nix { inherit scylla pretty-relative-time; };
in
nixpkgs.haskell.packages.${compiler}.callPackage ./scylla-tui.nix { inherit scylla scylla-api; }
