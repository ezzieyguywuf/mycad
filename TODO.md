# General
- ADD INTERNATIONALIZATION!!!!
- Figure out if we can set up a cross-platform (i.e. including windows and mac) automated
  build system
- Update documentation with examples.
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
- Make Server::getResponse less fragile. This can probably be done by storing a custom
  object, say `MyCAD::Communication::Response` which has some sort of `state` enum. We can
  set the `state` to something like `EMPTY` prior to running the command - this way, we
  can safely check `state` and always rely on an empty string being the actual response
  from the executed command.
- Extend `Add` command to accept a sub-command which describes what is being added.
- Now that Server is understanding more and more commands, it's time to use the Command
  design pattern (I think....)
- Is it ok to forward-declare `Server` in order to use it as a parameter in `Command`? Is
  this bad coding practice/design?
- File bug-report or merge request with upstream cxxopts so that we can default-construct
  the cxxapts::ParseResult
- Is our `Request->Server->Response` design flimsy/faulty? Is it ok to expect the user to
  inspect the return-value of `Server::processRequest` before calling
  `Server::getResponse`?
- What about the whole notion of storing a response as a member variable in Server? What
  happens when we get multiple Requests that haven't been queried yet?
  - I guess keeping a Queue of `Request` with associated `Response` would probably be
    wise...
  - This also suggests that we need some sort of ID for each Request. And the Caller would
    need to know about this ID...
- Move processArgs out of Server...maybe put it directly in main? or in a new header?
# Geometry
- Fully abstract CGAL out of Shapes -> we need to make a wrapper around Arrangement_2 in
  our Geometry namespace
# Shapes
- Consider renaming from `Shapes` to `Topology`
- Rename `getLineSegment` to `getGeometry`? Or add an abstraction or inheritence level?
- Update Wire constructor to accept Edge rather than LineSegment?

# GUI
