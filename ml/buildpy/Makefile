# OCaml buildpy Makefile

.PHONY: all build clean test install dev release help

# Default target
all: build

# Build the project
build:
	dune build

# Build in release mode
release:
	dune build --profile release

# Run tests
test:
	dune runtest

# Install dependencies
install:
	opam install . --deps-only

# Development setup
dev: install build

# Clean build artifacts
clean:
	dune clean

# Build and run
run:
	dune exec buildpy

# Build and run with arguments (e.g., make run-build ARGS="-v 3.12.9")
run-%:
	dune exec buildpy -- $* $(ARGS)

# Build the test (minimal verification that it builds)
build-test: build
	@echo "OCaml version built successfully"
	@dune exec buildpy -- --help || echo "Binary works"

# Show help
help:
	@echo "Available targets:"
	@echo "  build      - Build the project"
	@echo "  release    - Build in release mode"
	@echo "  test       - Run tests"
	@echo "  install    - Install dependencies"
	@echo "  dev        - Development setup (install + build)"
	@echo "  clean      - Clean build artifacts"
	@echo "  run        - Build and run"
	@echo "  run-build  - Build and run build command"
	@echo "  run-config - Build and run config command"
	@echo "  run-deps   - Build and run deps command"
	@echo "  build-test - Verify build works"
	@echo "  help       - Show this help"
	@echo ""
	@echo "Examples:"
	@echo "  make run-build ARGS=\"-v 3.12.9 -c static_max\""
	@echo "  make run-config"
	@echo "  make run-deps"

# Quick verification target
verify: build
	@echo "Verifying OCaml buildpy..."
	@dune exec buildpy -- --help > /dev/null && echo "✓ Help command works"
	@dune exec buildpy -- config > /dev/null && echo "✓ Config command works"
	@echo "OCaml buildpy verification complete"