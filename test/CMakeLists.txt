set(TEST_SRCS
    main.cpp
    GeometryTests.cpp
    ShapesTests.cpp
)

set(TEST_LINKS
    MyCAD_Geometry
    MyCAD_Shapes
    MyCAD_Exceptions
)
add_executable(MyCAD_Tests ${TEST_SRCS})
target_link_libraries(MyCAD_Tests ${TEST_LINKS})
