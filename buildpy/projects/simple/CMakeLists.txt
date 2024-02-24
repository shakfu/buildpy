
set(PROJECT_SRC ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(demo
    ${PROJECT_SRC}/main.cpp
)

target_include_directories(demo
    PUBLIC
    ${PYTHON_PREFIX}/include/python${PYTHON_VER}
)

target_compile_options(demo
    PUBLIC
    -fno-strict-overflow
    -Wsign-compare
    -Wunreachable-code
    -fno-common
    -dynamic
    -DNDEBUG
    -g
    -O3
    -Wall
    "-isysroot/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"
)

# target_link_options(demo
#     PUBLIC
# )

target_link_directories(demo
    PUBLIC
    ${PYTHON_PREFIX}/lib
)

target_link_libraries(demo
    -ldl
    "-framework CoreFoundation"
    "-framework SystemConfiguration"
    ${PYTHON_PREFIX}/lib/libpython${PYTHON_VER}.a
    ${SSL_PREFIX}/lib/libssl.a
    ${SSL_PREFIX}/lib/libcrypto.a
    ${BZ2_PREFIX}/lib/libbz2.a
    ${XZ_PREFIX}/lib/liblzma.a
    -lz
    -lsqlite3
    # -lmpdec
    -lreadline
)

