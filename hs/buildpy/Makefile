
#GHC_VER = $(shell ghc --version | sed 's/[^0-9.]*\([0-9.]*\).*/\1/')
EXECUTABLE := $(shell cabal list-bin buildpy)


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
	@cp $(EXECUTABLE) ../../bin/buildpy-hs
