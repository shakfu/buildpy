
GHC_VER = $(shell ghc --version | sed 's/[^0-9.]*\([0-9.]*\).*/\1/')

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
	@mkdir -p ../../bin
	@cabal build --enable-optimization=2 --enable-executable-stripping
	@cp dist-newstyle/build/aarch64-osx/ghc-9.4.8/buildpy-0.1.0.0/x/buildpy/opt/build/buildpy/buildpy ../../bin/buildpy-hs
