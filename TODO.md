# General
- ADD INTERNATIONALIZATION!!!!
- Add gitlab CI integration back, using the haskell stuff.
- Figure out how to host haddock documentation on gitlab
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
- Move the "MyCAD© v" into main, so that Server can return strictly the version number.
- Change the default value for `MYCAD_BUILD_GUI` to `OFF`?
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
- Code smell (?) is back - is there a better/different way to keep a static list of
  KNOWN_COMMANDS without needing to carry it around as a member variable?
- Allow for a "brief" and a "long" help message.
# Geometry
- Fully abstract CGAL out of Shapes -> we need to make a wrapper around Arrangement_2 in
  our Geometry namespace
# Shapes
- Consider renaming from `Shapes` to `Topology`
- Rename `getLineSegment` to `getGeometry`? Or add an abstraction or inheritence level?
- Update Wire constructor to accept Edge rather than LineSegment?

# HaskellCAD
- run hlint on Geometry and GeometrySpec
- use Set as appropriate in Topology. We can then get rid of hasAny
- move pretty printing to it's own module
- re-evaluate if Geometry needs typeclasses
- update topo functions to return added value, or add a prime version of such functions
- update Geometry.getComponents to return a tuple.
- Figure out better way to get "last X" from Topology or even Entity.
- refactor Entity to use State monad better.
- Add test for "If removeXXX called with non-existent XXX, nothing happens"
- check out 'boxes' for prettyprinter using 'columns'
