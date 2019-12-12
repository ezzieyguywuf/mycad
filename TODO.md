# General
- Figure out if we can set up a cross-platform (i.e. including windows and mac) automated
  build system
- Update documentation with examples.
- Should we explicitly `#include` every header needed, regardless of whether its already
  been included elsewhere?
- Make a logging facility.
- Decide whether or not to keep individual libraries or one monolithic library - maybe
  different for dev vs. release?
- Figure out how to make groups in member definitions share the same header styling as,
  say "Public Member Functions"
# Communication
- Code smell? Look at the `static bool first` line in Server constructor
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
# Geometry
- Fully abstract CGAL out of Shapes -> we need to make a wrapper around Arrangement_2 in
  our Geometry namespace
- We should probably also make a `Number` class. This provide our users with public
  access to any arbitrary number-type used in the implementation, i.e. `int`,
  `double`, `CGAL:FT`, etc...
- Check logic in Wire constructor -> is it ok to simply check for equality in order to
  determine if we're on the first iteration of the for-loop? Or should we do:
  ```cpp
  bool first = true
  for(...)
  {
      if (first)
      {
          first = false;
          continue;
      }
  }
  ```
# Shapes
- Consider renaming from `Shapes` to `Topology`
- Rename `getLineSegment` to `getGeometry`? Or add an abstraction or inheritence level?
- Update Wire constructor to accept Edge rather than LineSegment?

# GUI
