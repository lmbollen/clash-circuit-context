-- The project inside the Nix development shell (`nix develop`). The shell
-- already has every dependency, built by Nix from the same clash-compiler
-- commit that cabal.project pins. This file leaves that pin out: with it,
-- cabal would build clash-prelude (and the packages above it) a second time
-- from git instead of using the shell's.
--
-- Usage: ./check.sh --project-file=cabal.project.nix
packages: .

tests: True
