
.PHONY: all build release fmt run clean

all: build


build:
	@cargo build

release:
	@cargo build --release

run:
	@cargo run

fmt:
	@cargo fmt -- --emit=files
	# @rustfmt src/**/*.rs

clean:
	@cargo clean
