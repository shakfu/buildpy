

.PHONY: all run build test clean fmt


all: build


run:
	@go run main.go config

build:
	@go build -ldflags "-w"
	@./buildpy build

fmt:
	@go fmt main.go
	@go fmt cmd/*.go
	@go fmt internal/config/*.go
	@go fmt internal/shell/*.go
	@go fmt internal/models/*.go

clean:
	@rm -rf ./buildpy ./build

test:
	@echo "testing.."

reset: clean
	@go clean -modcache

