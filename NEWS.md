## v0.6.15

* Remove CXX=CXX11 from Makevars. This should also fix problems with CRAN
  special check setups.

## v0.6.14 

* Patch for Apple's sprintf -> snprintf requirements

## v0.6.13

* Patch for Apple M1's clang-15 derivative. 
 

## v0.6.12

* Patch for Apple M1's clang-15 derivative. 
  Apple seems not to contain GNU_LIBRARY-flavored stdlib, triggering  alternate
  definitions which did not match templates.  Not all platforms are consistent
  with the signatures of getenv() / getopt(), let's hope Apple's clang is 
  as least.  

## v0.6.11

* Additional patch for compatibility with new warnings on clang-15 + R-devel

## v0.6.10

* Additional patch for compatibility with new warnings on clang-14

## v0.6.9

* Additional modifications for compatibility with new clang-12 warnings

## v0.6.8

*  Additional modifications attempted for Solaris compatibility.

## v0.6.6

* Avoid architecture-sensitive caching checks on CRAN

## v0.6.4

* Indicate no solaris support via SystemRequirements

## v0.6.3

* Additional CRAN patches for Solaris and additional warnings.

## v0.6.2, Released 2020-06-26 to CRAN

* Modernization and patches to C++ code to address warnings on gcc-10
* Copyright and Credit information for known all APPL contributors and 
  other included sources.

## v0.6.1, Released 2019-04-10 (GitHub only)

* patch memoization
* resolve several compiler warnings
* use BH package for boost headers instead of packaging with appl code


## v0.6.0, Released 2018-11-29 (GitHub Only)

* Added a `NEWS.md` file to track changes to the package.
* Uses a memoization strategy in caching sarsop results.  This deprecates
  the previous behavior where sarsop runs would be set to `eval=FALSE` and results
  manually loaded from cache with `meta_from_log` and `alpha_from_log`.  Just call
  `sarsop` again to get load output from cache, if parameters (call signature) matches
  any available cache that will be loaded instead. 
* Uses `processx` to control execution of sarsop C++ code.
