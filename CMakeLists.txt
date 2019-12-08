cmake_minimum_required(VERSION 3.11)
cmake_policy(VERSION 3.11)
project(MyCAD VERSION 0.0.1)

# --------------- Compile Options ------------------
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_BUILD_TYPE "Debug")
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR}/lib)
add_compile_options(-std=c++11 -Wpedantic -Werror -Wextra -Wall)

# --------- Various CMake Configs --------------
message("${CMAKE_CURRENT_SOURCE_DIR}/misc/cmake")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/misc/cmake")
include_directories("include")

# ------------ Find Things ---------------------
find_package(CGAL REQUIRED)
include(${CGAL_USE_FILE})

# --------------- Add Sub-Directories --------------
add_subdirectory(src)
add_subdirectory(test)
