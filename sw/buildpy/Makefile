
EXECUTABLE_DIR := $(shell swift build -c release --show-bin-path)

.PHONY: all build run release clean format lint test


all: build

build:
	@swift build

run:
	@swift run

release:
	@mkdir -p ../../bin
	@swift build -c release
	@mv $(EXECUTABLE_DIR)/buildpy ../../bin/buildpy-sw

clean:
	@rm -rf .build build

fmt:
	@swift-format --recursive -i  Sources

lint:
	@swift-format lint --recursive Sources

test:
	@swift test