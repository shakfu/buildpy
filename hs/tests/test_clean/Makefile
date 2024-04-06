EXECUTABLE := $(shell cabal list-bin test-clean)

.PHONY: all build clean link

all: build


build:
	@cabal build


clean:
	@rm -rf dist-newstyle

reset:
	@rm -rf python3.12
	@unzip python3.12.zip

link:
	@rm -f ./test-clean
	@ln -s $(EXECUTABLE) .

