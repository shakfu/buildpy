


.PHONY: all, build, run, release, clean, format, lint


all: build

build:
	@swift build

run:
	@swift run

release:
	@swift build -c release

clean:
	@rm -rf .build


fmt:
	@swift-format --recursive -i  Sources


lint:
	@swift-format lint --recursive Sources
