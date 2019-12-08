find_package(Qt5 COMPONENTS Test REQUIRED)

add_executable(MyCAD_GuiTests ApplicationTests.cpp)
target_link_libraries(MyCAD_GuiTests Qt5::Widgets Qt5::Test)
