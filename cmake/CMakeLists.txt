include(01_SetupCmakeVariables.cmake)
if(MYCAD_BUILD_GUI)
    include(05_SetupQt.cmake)
endif()
