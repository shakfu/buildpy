
echo "compiling..."

INCLUDE=`python3-config --include`

VERSION=`python3 -c 'import sysconfig; print(sysconfig.get_python_version())'`

CFLAGS="${INCLUDE} -Wno-unused-result -Wsign-compare -Wunreachable-code -DNDEBUG -fwrapv -O3 -Wall"

LDFLAGS=`python3-config --ldflags`

SRCDIR=`python3 -c 'import sysconfig; print(sysconfig.get_config_var("srcdir"))'`

LIB="${SRCDIR}/libpython${VERSION}.a"

clang $CFLAGS $LDFLAGS $LIB main.c -o main

echo "cleaning up..."
rm -rf *.dSYM



