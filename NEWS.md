# sarsop 0.6.0

* Added a `NEWS.md` file to track changes to the package.
* Uses a memoization strategy in caching sarsop results.  This deprecates
  the previous behavior where sarsop runs would be set to `eval=FALSE` and results
  manually loaded from cache with `meta_from_log` and `alpha_from_log`.  Just call
  `sarsop` again to get load output from cache, if parameters (call signature) matches
  any available cache that will be loaded instead.  
