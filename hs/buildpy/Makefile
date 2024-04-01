
#GHC_VER = $(shell ghc --version | sed 's/[^0-9.]*\([0-9.]*\).*/\1/')
EXECUTABLE := $(shell cabal list-bin buildpy)
INDENT_SIZE := 4


.PHONY: all build run repl clean indent link release


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
	@hindent --indent-size $(INDENT_SIZE) exe/*.hs
	@hindent --indent-size $(INDENT_SIZE) src/*.hs
	@hindent --indent-size $(INDENT_SIZE) src/Models/*.hs

link:
	@rm ./buildpy
	@ln -s $(EXECUTABLE) .

release: build
	@mkdir -p ../../bin
	@cabal build --enable-optimization=2 --enable-executable-stripping
	@cp $(EXECUTABLE) ../../bin/buildpy-hs
