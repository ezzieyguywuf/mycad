# General
- ADD INTERNATIONALIZATION!!!!
- Figure out if we can set up a cross-platform (i.e. including windows and mac) automated
  build system
- Should we explicitly `#include` every header needed, regardless of whether its already
  been included elsewhere?
- Make a logging facility.
- Decide whether or not to keep individual libraries or one monolithic library - maybe
  different for dev vs. release?
- Figure out how to make groups in member definitions share the same header styling as,
  say "Public Member Functions" - this is a doxygen thing
- Can we get rid of boost dependency? Does that mean we can't use CGAL?
- Should we ship mfpr and gmp rather than requiring it as a dependency?
- Use readline or similar to make i/o a little more bearable in main loop.
# Communication
- Improve the shutdown procedure - it's pretty hacked right now.
- File bug-report or merge request with upstream cxxopts so that we can default-construct
  the cxxapts::ParseResult
# Commands
- Extend `Add` command to accept a sub-command which describes what is being added.
- Add more error-checking to commands -> probably at least check for `operator>>` return
  value?
- Maybe transition towards a full-blown lexer, like FreeCAD does in places?
- Maybe split out each command as a standalone executable, like Unix does? Will the
  inter-process communication be too slow?
- If we're going to allow commands to have sub-commands, we need to abstract something
  out. Notice how the Add command is duplicating code that already exists in Server.
# Geometry
- Fully abstract CGAL out of Shapes -> we need to make a wrapper around Arrangement_2 in
  our Geometry namespace
# Shapes
- Consider renaming from `Shapes` to `Topology`
- Rename `getLineSegment` to `getGeometry`? Or add an abstraction or inheritence level?
- Update Wire constructor to accept Edge rather than LineSegment?

# GUI
