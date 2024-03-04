GO_FILES=$(shell go list ./... | grep -v /vendor/)

.PHONY: all run build build-test test clean fmt vet tidy repl repl-test \
		rpath

all: build

run:
	@go run main.go build

build:
	@go build -ldflags "-w"

build-test: clean build
	@./buildpy build

repl:
	@build/install/python/bin/python3

repl-test:
	@LD_LIBRARY_PATH=$(PWD)/build/install/python/lib \
		build/install/python/bin/python3

fmt:
	@go fmt $(GO_FILES)

vet:
	@go vet $(GO_FILES)

test:
	@go test $(GO_FILES)

clean:
	@rm -rf ./buildpy ./build/src/python ./build/install/python

rpath:
	otool -L build/install/python/bin/python3

tidy: clean
	@go mod tidy

reset: clean
	@go clean -cache -modcache -i -r
