set(TEST_SRCS
    main.cpp
    ShapesTests.cpp
)

set(TEST_LINKS
    MyCAD_Shapes
)
add_executable(MyCAD_Tests ${TEST_SRCS})
target_link_libraries(MyCAD_Tests ${TEST_LINKS})
