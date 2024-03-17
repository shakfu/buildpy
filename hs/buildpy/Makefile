
.PHONY: all build run repl clean indent release


all: build

build:
	@cabal build

run:
	@cabal run

repl:
	@cabal repl

clean:
	@rm -rf dist-newstyle build

indent:
	@hindent --indent-size 2 app/*.hs
	@hindent --indent-size 2 app/Models/*.hs

release:
	@cabal build --enable-optimization=2 --enable-executable-stripping
	@mkdir -p build
