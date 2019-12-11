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
# Server
- Code smell? Look at the `static bool first` line in Server.cpp
- File bug-report or merge request with upstream cxxopts so that we can default-construct
  the cxxapts::ParseResult
# Geometry
- Take out any CGAL-specific stuff from the Geometry headers, i.e. the public API. This
  should make it feasible to implement this api using different "backends" - let's test
  this out by duplicating the existing implementation using OpenCascade rather than CGAL,
  just for fun.
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
