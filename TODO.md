# General
- Update documentation to provide clear instructions on what each class does/doesn't do,
  with examples.
- Should we explicitly `#include` every header needed, regardless of whether its already
  been included elsewhere?
# Geometry
- Fix Wire constructor -> should actually check whether to throw error
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
