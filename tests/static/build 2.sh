
echo "compiling..."

CFLAGS="-I/usr/local/Cellar/python/3.7.7/Frameworks/Python.framework/Versions/3.7/include/python3.7m -Wno-unused-result -Wsign-compare -Wunreachable-code -fno-common -static -DNDEBUG -g -fwrapv -O3 -Wall -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX10.14.sdk -I/Library/Developer/CommandLineTools/SDKs/MacOSX10.14.sdk/usr/include -I/Library/Developer/CommandLineTools/SDKs/MacOSX10.14.sdk/System/Library/Frameworks/Tk.framework/Versions/8.5/Headers"

LDFLAGS="-L/usr/local/opt/python/Frameworks/Python.framework/Versions/3.7/lib/python3.7/config-3.7m-darwin -lpython3.7m -ldl -framework CoreFoundation"




for fname in main
do
    echo "compiling $fname"
    clang -v $CFLAGS $LDFLAGS $fname.c -o $fname
done

echo "cleaning up..."
rm -rf *.dSYM



