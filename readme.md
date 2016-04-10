# APPL for R

Wrappers in R for the APPL toolkit for approximate POMDP planning.

```r
# Example model
setwd(tempdir())
model <- system.file("models/example.pomdp", package = "appl")

# Create Policy
policy <- pomdpsol(model)
readLines(policy)

# Other tools
evaluation <- pomdpeval(model, policy)
graph <- polgraph(model, policy)
simulations <- pomdpsim(model, policy)
```

## Notes

Unfortunately the appl source code is a bit dated and not suitable for using as a shared library. It builds with lot of warnings and on Windows it only builds with MS Visual Studio. This package tries to make things as easy as possible for the user by bunding the appl executables and wrap them with `system` calls in R.

## Thanks

Mykel Kochenderfer and Markus Herrmann have been helpful in providing windows builds
using MS Visual Studio:

 - http://web.stanford.edu/group/sisl/resources/appl-0.96-win-win32.zip
 - http://web.stanford.edu/group/sisl/resources/appl-0.96-win-x64.zip
