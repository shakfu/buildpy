
.PHONY: all run build test clean fmt tidy


all: build


run:
	@go run main.go build

build:
	@go build -ldflags "-w"

fmt:
	@go fmt main.go
	@go fmt cmd/*.go
	@go fmt internal/config/*.go
	@go fmt internal/shell/*.go
	@go fmt internal/models/*.go

clean:
	@rm -rf ./buildpy ./build/lib/python ./build/src/cpython

test:
	@echo "testing.."

tidy: clean
	@go mod tidy

reset: clean
	@go clean -cache -modcache -i -r
