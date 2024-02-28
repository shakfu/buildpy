

.PHONY: all run build test clean fmt


all: build


run:
	@go run main.go config

build:
	@go build

fmt:
	@go fmt main.go
	@go fmt cmd/*.go
	@go fmt internal/config/*.go

clean:
	@rm -f ./buildpy

test:
	@echo "testing.."