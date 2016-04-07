# APPL for R

Wrappers in R for the APPL toolkit for approximate POMDP planning.

```r
# Example model file
example <- system.file("models/example.pomdp", package = "appl")
file.copy(example, "example.pomdp")

# Runs the solver
policy <- pomdpsol("example.pomdp")

# Output XML
readLines(policy)
```

## Notes

Unfortunately the appl source code is a bit dated and not suitable for using as a shared library. It builds with lot of warnings and on Windows it only builds with MS Visual Studio. This package tries to make things as easy as possible for the user by bunding the appl executables and wrap them with `system` calls in R.
