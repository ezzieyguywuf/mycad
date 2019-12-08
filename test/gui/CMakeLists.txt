add_compile_options(-fPIC)

find_package(Qt5 COMPONENTS Core Widgets Test Network REQUIRED)
include_directories(/usr/include/qt5)

add_executable(MyCAD_GuiTests ApplicationTests.cpp)
target_link_libraries(MyCAD_GuiTests Qt5Test)
set_property(TARGET MyCAD_GuiTests APPEND PROPERTY AUTOMOC_MACRO_NAMES "Q_OBJECT")
