macro(mycad_find_qt)
    set(CMAKE_AUTOMOC ON)
    set(CMAKE_AUTORCC ON)
    set(CMAKE_AUTOUIC ON)

    find_package(Qt5 COMPONENTS Widgets REQUIRED)
endmacro(mycad_find_qt)
