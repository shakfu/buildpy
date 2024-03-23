

.PHONY: all build clean format

all: build


build:
	@mkdir -p build && cd build && cmake .. && cmake --build .

clean:
	@rm -rf build


format:
	@clang-format -i src/buildpy.cpp
	@clang-format -i src/buildpy.hpp
