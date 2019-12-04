cmake_minimum_required(VERSION 3.0.0)
cmake_policy(VERSION 3.0.0)
project(MyCAD VERSION 0.0.1)

# --------------- Compile Options ------------------
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_BUILD_TYPE "Debug")
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR}/lib)
add_compile_options(-std=c++11 -Wpedantic -Werror -Wextra -Wall)

# --------------- Add Sub-Directories --------------
add_subdirectory(test)
