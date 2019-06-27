.PHONY : test

start:
	ghcid --warnings --clear --command="cabal new-repl exe:template" --test="Main.main"

watch:
	ghcid --warnings --clear --command="cabal new-repl spec" --test="Main.main"

build:
	cabal new-build

run:
	cabal new-run template

test:
	cabal new-run spec

ci:
	cabal new-update && make test

nix:
	hpack && cabal2nix . > project.nix
