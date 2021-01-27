# General
- ADD INTERNATIONALIZATION!!!!
- Add gitlab CI integration back, using the haskell stuff.
- Figure out how to host haddock documentation on gitlab
- Make it so that `quit` in TUI will close GUI as well.
- Update build instructions for haskell - should be cross-platform
  automatically!  build system
- Update all imports to "explicit", i.e. `import SomeModule (f1, f2, etc...)`
    - Or, `import qualified SomeModule as SM` if you need a bunch of stuff
- Make a logging facility.
- Add "version" command to return current version
- Figure out how to package executable
- move pretty printing to it's own module
- check out 'boxes' for prettyprinter using 'columns'
- Can we get away from the fgl library altogether? It seems that we're jumping
  through quite a number of hoops to make it work for us.

# Commands
- Allow for a "brief" and a "long" help message.
- Add actual help messages
- Add unit tests for parser

# Geometry
- re-evaluate if Geometry needs typeclasses (probably not)

# Topology
- Update removeXXX to return a list of modified topology. This should be useful
  to the caller, for instance if an Edge gets accidentally turned into an
  infinite Edge (because it only has one Vertex)
- Update to use ErrorT, especially vertexFromID should have two different
  errors, one from no nodes, one for more than one node
- Should vertexEdges and friends return a Set rather than a List?
- Add pictures to the documentation - or, at least add pictures *somewhere*

# Entity

# GUI
- Add lighting/shading
- infinite zoom.
- Remove hard-coded paths - allow for configurable or something. Maybe use file-embed
- Update Shader data type to actually use the Uniform data type 
- Figure out why line thickness zooms - it shouldn't, it should always be the same
  not in getAttributeDataIndex
- Reduce "cumulative delta" in mouse rotation so that it starts rotating sooner
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
- Start using TUI.Errors again, we got rid of it when we started using
  megaparsec
- LaunchTUI.handleCommand could probably use `modify` or something to clean it
  up a bit
- Accept command-line arguments for things like window size, so that we don't
  have to rebuild the whole thing to try different values
