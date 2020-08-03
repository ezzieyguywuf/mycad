This package contains the main executable(s) of the MyCAD program. It utilizes relies heavily on the Entity module, which itself brings together the ideas of Geometry and Topology which form the basis for a useful CAD system.

There are a few different executables that you can build in this package:

    - MyCAD:exe:mycad is the main executable. It includes a command-line "TUI"
      which is used to issue commands to the CAD kernel. These commands are
      made visible in an openGL "GUI" window which is opened by default when
      the program is started.
    - MyCAD:exe:mycad-tui is a standalone, command-line only version of MyCAD.
      It includes all the functionality of the main mycad executable, but does
      *not* include any GUI. This means that it has much fewer dependencies,
      and may be useful for you depending on your desired workflow
    - MyCAD:exe:mycad-gui is a standalone, GUI-only version of MyCAD. Unlike
      mycad-tui, this executable is missing almost all the features of the main
      mycad executable. This is because there are currently no GUI
      buttons/tools available for executing any of the TUI-commands. As such,
      this executable is mostly useful for development, though it is expected
      to add more features over time.
    - MyCAD:exe:mycad-dev is an executable used during development. It can
      safely be ignored, as it will contain little (if anything) if interest to
      users.

Please see the [MyCAD gitlab](http://gitlab.com/ezzieyguywuf/mycad) homepage
for more information
