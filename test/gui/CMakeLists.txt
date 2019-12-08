target_sources(MyCAD_Tests PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/ApplicationTests.cpp)
set(_MYCAD_GUI_TEST_LIBS Qt5::Widgets PARENT_SCOPE)
#target_link_libraries(MyCAD_Tests Qt5::Widgets)
