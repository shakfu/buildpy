

.PHONY: all run build test clean fmt


all: build


run:
	@go run main.go config

build:
	@go build -ldflags "-w"

fmt:
	@go fmt main.go
	@go fmt cmd/*.go
	@go fmt internal/config/*.go
	@go fmt internal/shell/*.go
	@go fmt internal/builder/*.go

clean:
	@rm -f ./buildpy

test:
	@echo "testing.."

reset: clean
	@go clean -modcache

