set(TEST_SRCS
    main.cpp
    GeometryTests.cpp
    ShapesTests.cpp
)

set(TEST_LINKS
    MyCAD_Shapes
    MyCAD_Exceptions
)

add_executable(MyCAD_Tests ${TEST_SRCS})
target_link_libraries(MyCAD_Tests ${TEST_LINKS})

if(MYCAD_BUILD_GUI)
    add_subdirectory(gui)
endif()
