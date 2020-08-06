This package is useful for testing the entirety of the MyCAD ecosystem at once.
It incorporates all the unit test modules from Geometry, Topology, and Entity,
and also runs an executable intended to kick the tires

I run this with:

```sh
ghcid --command=stack ghci Test:test:MyCAD-test --run="main >> MyCADTest.main"
```

Please see the [MyCAD gitlab](http://gitlab.com/ezzieyguywuf/mycad) homepage
for more information
