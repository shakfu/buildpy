cmake_minimum_required(VERSION 3.12...3.21)

project(
    buildpy
    VERSION 14.2.4
    DESCRIPTION "builds python from source"
    LANGUAGES CXX
)

option(ENABLE_ADDRESS_SANITIZER "enable clang address sanitizer" OFF)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(PROJECT_NAME buildpy)


set(THIRDPARTY_SRC
    src/thirdparty/format.cc
)

add_executable(${PROJECT_NAME}
    ${THIRDPARTY_SRC}
    src/buildpy.cpp 
)

target_include_directories(${PROJECT_NAME}
    PUBLIC
    ${CMAKE_SOURCE_DIR}/include
)

target_compile_options(${PROJECT_NAME}
    PUBLIC
    $<IF:$<BOOL:${ENABLE_ADDRESS_SANITIZER}>,-O1,-O2>
    $<$<BOOL:${ENABLE_ADDRESS_SANITIZER}>:-g>
    $<$<BOOL:${ENABLE_ADDRESS_SANITIZER}>:-fsanitize=address>
    $<$<BOOL:${ENABLE_ADDRESS_SANITIZER}>:-fno-omit-frame-pointer>
)

target_link_options(${PROJECT_NAME}
    PUBLIC
    $<$<BOOL:${ENABLE_ADDRESS_SANITIZER}>:-g>
    $<$<BOOL:${ENABLE_ADDRESS_SANITIZER}>:-fsanitize=address>
)

target_compile_definitions(${PROJECT_NAME}
    PUBLIC
    "VERBOSE=1"
    "DEBUG"
)

target_link_libraries(${PROJECT_NAME}
    PUBLIC
    -pthread
)