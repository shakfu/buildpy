
.PHONY: all run build test clean fmt tidy repl rpath


all: build


run:
	@go run main.go build

build:
	@go build -ldflags "-w"

repl:
	@build/install/python/bin/python3

fmt:
	@go fmt main.go
	@go fmt cmd/*.go
	@go fmt internal/config/*.go
	@go fmt internal/shell/*.go
	@go fmt internal/models/*.go

clean:
	@rm -rf ./buildpy ./build/src/python ./build/install/python

rpath:
	otool -L build/install/python/bin/python3

test:
	@echo "testing.."

tidy: clean
	@go mod tidy

reset: clean
	@go clean -cache -modcache -i -r
