cmake_minimum_required(VERSION 3.0.1)
cmake_policy(VERSION 3.0.1)
project(MyCAD VERSION 0.0.1)

# This sub-directory will contain various files that are all `include`d automatically when
# we add it using add_subdirectory. The intent is to, as much as possible, keep heavy
# "logic" out of this file, and defer it to the sub-directory.
add_subdirectory(cmake)

# --------------- Compile Options ------------------
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_BUILD_TYPE "Debug")
set(EXECUTABLE_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR}/lib)
add_compile_options(-std=c++11 -Wpedantic -Werror -Wextra -Wall)

# --------- Various CMake Configs --------------
SetupCppMacros()
include_directories("include")

# ------------ Find Things ---------------------
if(MYCAD_BUILD_GUI)
    mycad_find_qt()
endif()
find_package(CGAL REQUIRED)
include(${CGAL_USE_FILE})

# --------------- Add Sub-Directories --------------
add_subdirectory(src)
add_subdirectory(test)
