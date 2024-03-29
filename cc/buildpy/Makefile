

.PHONY: all build release clean format

all: build


build:
	@mkdir -p build && cd build && cmake .. && cmake --build .

release:
	@mkdir -p build && cd build && cmake .. && cmake --build . --config Release
	@mkdir -p ../../bin
	@mv build/buildpy ../../bin/buildpy-cc

clean:
	@rm -rf build


format:
	@clang-format --style=webkit -i src/buildpy.cpp
	@clang-format --style=webkit -i src/buildpy.hpp
	@clang-format --style=webkit -i src/config.hpp
