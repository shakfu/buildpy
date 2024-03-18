
.PHONY: all build release fmt run clean

all: build


build:
	@cargo build

release:
	@mkdir -p ../../bin
	@cargo build --release
	@mv target/release/buildpy ../../bin/buildpy-rs

run:
	@cargo run

fmt:
	@cargo fmt -- --emit=files

clean:
	@rm -rf build *.log
	@cargo clean
