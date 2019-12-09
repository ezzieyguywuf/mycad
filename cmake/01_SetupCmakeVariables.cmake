# This is a variable used by CMAKE, but we'll cache it anyway because it is so common to
# us.
set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Change to Release to build without debug symbols")

# These are specific to MyCAD
set(MYCAD_BUILD_GUI OFF CACHE BOOL "Determines whether or not to build the GUI.")
