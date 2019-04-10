
# v0.6.1 
### Released 2019-04-10

* patch memoization
* resolve several compiler warnings
* use BH package for boost headers instead of packaging with appl code


# v0.6.0

### Released 2018-11-29

* Added a `NEWS.md` file to track changes to the package.
* Uses a memoization strategy in caching sarsop results.  This deprecates
  the previous behavior where sarsop runs would be set to `eval=FALSE` and results
  manually loaded from cache with `meta_from_log` and `alpha_from_log`.  Just call
  `sarsop` again to get load output from cache, if parameters (call signature) matches
  any available cache that will be loaded instead. 
* Uses `processx` to control execution of sarsop C++ code.
