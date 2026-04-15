# Makefile for buildpy Zig implementation

.PHONY: build release test clean run help

# Default target
all: build

# Build debug version
build:
	zig build

# Build release version
release:
	zig build -Doptimize=ReleaseFast
	mkdir -p ../../bin
	cp zig-out/bin/buildpy ../../bin/buildpy-zig

# Run tests
test:
	zig build test

# Clean build artifacts
clean:
	rm -rf zig-out .zig-cache

# Run the tool
run:
	zig build run -- $(ARGS)

# Run with specific Python version
run-3.13:
	zig build run -- -v 3.13.11 -c shared_mid

run-3.12:
	zig build run -- -v 3.12.12 -c shared_mid

# Build Python using the tool
build-python:
	zig build run -- -v 3.13.11 -c shared_mid -j 4

# Build with optimizations
build-python-optimized:
	zig build run -- -v 3.13.11 -c shared_mid -j 4 -o

# Write config file only
write-config:
	zig build run -- -w -c shared_mid

# Show help
help:
	@echo "buildpy Zig implementation"
	@echo ""
	@echo "Targets:"
	@echo "  build           Build debug version"
	@echo "  release         Build optimized release version"
	@echo "  test            Run unit tests"
	@echo "  clean           Clean build artifacts"
	@echo "  run             Run the tool (use ARGS= for arguments)"
	@echo "  run-3.13        Run with Python 3.13"
	@echo "  run-3.12        Run with Python 3.12"
	@echo "  build-python    Build Python 3.13 with shared_mid config"
	@echo "  write-config    Write config file only"
	@echo "  help            Show this help"
	@echo ""
	@echo "Examples:"
	@echo "  make build"
	@echo "  make test"
	@echo "  make run ARGS='-v 3.12.12 -c static_max'"
	@echo "  make release"
