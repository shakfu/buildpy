

.PHONY: all build virtual

all: virtual


build:
	@g++ -std=c++11 -stdlib=libc++ -o demo3 test_join_split_vec1.cpp
	@g++ -std=c++11 -stdlib=libc++ -o demo2 test_join_split_vec2.cpp
	@g++ -std=c++11 -stdlib=libc++ -o demo1 test_join_vec.cpp

virtual:
	@g++ -std=c++11 -stdlib=libc++ -o demo test_virtual.cpp

clean:
	@rm demo*
