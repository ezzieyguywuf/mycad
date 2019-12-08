# General
- Update documentation to provide clear instructions on what each class does/doesn't do,
  with examples.
- Should we explicitly `#include` every header needed, regardless of whether its already
  been included elsewhere?
- Make a logging facility.
- Decide whether or not to keep individual libraries or one monolithic library - maybe
  different for dev vs. release?
- Fully abstract CGAL out of Shapes -> we need to make a wrapper around Arrangement_2 in
  our Geometry namespace
# Documentation
- Figure out how to make groups in member definitions share the same header styling as,
  say "Public Member Functions"
# Geometry
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
