This update should address all current issues in CRAN checks:
Solaris ERROR, the clang WARNINGs on Fedora,
and the NOTE regarding Matrix export.  



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
