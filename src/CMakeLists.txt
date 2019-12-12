add_library(MyCAD_Exceptions SHARED Exceptions.cpp)

add_library(MyCAD_Geometry SHARED Geometry.cpp)
target_link_libraries(MyCAD_Geometry CGAL::CGAL MyCAD_Exceptions)

add_library(MyCAD_Shapes SHARED Shapes.cpp)
target_link_libraries(MyCAD_Shapes MyCAD_Geometry)

add_library(MyCAD_Communication SHARED Communication.cpp)

add_executable(MyCAD main.cpp)
target_link_libraries(MyCAD MyCAD_Communication MyCAD_Shapes)
