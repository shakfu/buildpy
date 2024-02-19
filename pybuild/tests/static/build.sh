
echo "compiling..."

ROOT="/Users/sa/Documents/shared/projects/pybuild/build/lib/python-static"

CFLAGS="-I${ROOT}/include/python3.9 -Wno-unused-result -Wsign-compare -Wunreachable-code -DNDEBUG -fwrapv -O3 -Wall"

LDFLAGS="-lintl -ldl -lz -framework CoreFoundation"

LIB="${ROOT}/lib/libpython3.9.a"

clang $CFLAGS $LDFLAGS $LIB main.c -o main

echo "cleaning up..."
rm -rf *.dSYM



