set(TEST_SRCS
    main.cpp
    GeometryTests.cpp
    ShapesTests.cpp
    ServerTests.cpp
)

set(TEST_LINKS
    MyCAD_Shapes
    MyCAD_Exceptions
    MyCAD_Server
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_executable(MyCAD_Tests ${TEST_SRCS})
target_link_libraries(MyCAD_Tests ${TEST_LINKS})

if(MYCAD_BUILD_GUI)
    add_subdirectory(gui)
    target_link_libraries(MyCAD_Tests ${_MYCAD_GUI_TEST_LIBS})
endif()
