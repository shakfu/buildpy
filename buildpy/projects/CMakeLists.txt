
set(PYTHON_PREFIX ${CMAKE_SOURCE_DIR}/build/install/python)
set(SSL_PREFIX ${CMAKE_SOURCE_DIR}/build/install/openssl)
set(XZ_PREFIX ${CMAKE_SOURCE_DIR}/build/install/xz)
set(BZ2_PREFIX ${CMAKE_SOURCE_DIR}/build/install/bzip2)

execute_process(
    COMMAND "${PYTHON_PREFIX}/bin/python3" -c "import sysconfig; print(sysconfig.get_python_version())"
    OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE PYTHON_VER
)

add_subdirectory(simple)
add_subdirectory(pybind)
