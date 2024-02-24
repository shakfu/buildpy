
# set(Python_ROOT_DIR build/install)

set(PROJECT_SRC ${CMAKE_CURRENT_SOURCE_DIR})

find_package(Python 3.8 COMPONENTS Interpreter Development.Module REQUIRED)

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

#add_subdirectory(pybind11)
# Detect the installed nanobind package and import it into CMake

execute_process(
    COMMAND "${Python_EXECUTABLE}" -m pybind11 --cmakedir
    OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE PB_DIR
)
list(APPEND CMAKE_PREFIX_PATH "${PB_DIR}")
find_package(pybind11 CONFIG REQUIRED)

add_executable(demo2
    ${PROJECT_SRC}/main.cpp
)

target_include_directories(demo2
    PUBLIC
    ${PYTHON_PREFIX}/include/python${PYTHON_VER}
)

target_link_directories(demo2
    PUBLIC
    ${PYTHON_PREFIX}/lib
)

target_link_libraries(demo2 
    pybind11::embed
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
    -lreadline
)