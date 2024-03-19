


.PHONY: all, build, run, release, clean, format, lint


all: build

build:
	@swift build

run:
	@swift run

release:
	@mkdir -p ../../bin
	@swift build -c release
	@mv .build/arm64-apple-macosx/release/buildpy ../../bin/buildpy-sw

clean:
	@rm -rf .build build

fmt:
	@swift-format --recursive -i  Sources

lint:
	@swift-format lint --recursive Sources
