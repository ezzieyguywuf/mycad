# General
- ADD INTERNATIONALIZATION!!!!
- Add gitlab CI integration back, using the haskell stuff.
- Figure out how to host haddock documentation on gitlab
- Update build instructions for haskell - should be cross-platform automatically!
  build system
- Update all imports to "explicit", i.e. `import SomeModule (f1, f2, etc...)`
    - Or, `import qualified SomeModule as SM` if you need a bunch of stuff
- Make a logging facility.
- Add "version" command to return current version
- Figure out how to package executable
- move pretty printing to it's own module
- check out 'boxes' for prettyprinter using 'columns'

# Commands
- Move away from individual "addXXX" (i.e. `addVertex` right now) and instead
  use a generic `add` which accepts a sub-command, i.e. `add vertex`
- Allow for a "brief" and a "long" help message.
- Add actual help messages

# Geometry
- run hlint on Geometry and GeometrySpec
- re-evaluate if Geometry needs typeclasses (probably not)

# Topology
- use Set as. We can then get rid of hasAny

# GUI
- Add lighting/shading
- infinite zoom.
- Remove hard-coded paths - allow for configurable or something.
- Update Shader data type to actually use the Uniform data type 
- Figure out why line thickness zooms - it shouldn't, it should always be the same
  not in getAttributeDataIndex
- Update mouse rotation so that it's not so twitchy.
- Allow simultaneous keys, i.e. arrows, to control things
- Update resize technique so that there are no visual artifacts.
- Fix window resize to also adjust aspectRatio, i.e. in the lineVShader
- Refactor main so that we don't have to pass a Window around.

# UX
- Add icons to GUI

# TUI
- Allow run-time determination (is this a good idea?) of the parametrization of
  `Command a` in CommandParser. Right now, we hard-code a `Geo.Point Float`,
  but it'd be nice for this to be configurable by the User somehow
- Clean up Main - should we use `catchError`? (probably)
