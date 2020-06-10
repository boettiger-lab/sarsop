## Test environments

* local R installation, ubuntu 20.04, R 4.0.0
* GitHub Actions CI:
  - macOS-latest,   R: 'devel'
  - macOS-latest,   r: 'release'
  - windows-latest, r: 'release'
  - ubuntu-16.04,   r: 'release'
  - ubuntu-16.04,   r: 'oldrel'
  - ubuntu-16.04,   r: '3.5'
  - ubuntu-16.04,   r: '3.4
  - ubuntu-16.04,   r: '3.3'
* win-builder (devel)
* r-hub (rhub::check_for_cran(), includes checks with sanitizers)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* This package throws 1 NOTE:

> checking installed package size ... NOTE
    installed size is  5.2Mb
    sub-directories of 1Mb or more:
      bin   4.4Mb

This is necessary and unavoidable element required to execute the
original SARSOP software so that it may be easily and portably distributed
within this package. Also, my apologies about the gcc-10 warnings earlier in the 
initial submission, that has now been rectified. 

Thanks for explaining the NULL error on studio2.h call as well, this has now
also been fixed!

