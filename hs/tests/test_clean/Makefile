EXECUTABLE := $(shell cabal list-bin test-clean)

.PHONY: all build clean link

all: build


build:
	@cabal build


clean:
	@rm -rf dist-newstyle


link:
	@rm -f ./test-clean
	@ln -s $(EXECUTABLE) .

