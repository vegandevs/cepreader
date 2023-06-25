Version 1.2-2
=============

* Maintenance release to adapt to new settings in **R** build tools
  for Fortran. Github 
  [issue #7](https://github.com/vegandevs/cepreader/issues/7)

Version 1.2-1
=============

* Maintenance release for new Windows build tools. Fortran binary
  is now 64-bit in Windows.

Version 1.2-0
=============

* Installation failed in 'Apple Silicon'. GitHub
  [issue #6](https://github.com/vegandevs/cepreader/issues/6)

* Can return the data as a sparse matrix (of **Matrix** package). This
  can offer considerable saving in disk storage. However, several
  **R** functions may be unable to handle sparse matrices, but they
  must be changed to ordinary dense matrices or data frames.
  Moreover, sparse matrices are often changed to full dense matrices
  within the analysis.  For instance, a centred or standardized data
  matrix in ordination is dense.

Version 1.1-3
=============

* This version depends on **R** 3.6.0 or later. This is a maintenance
  release that adapts to internal changes in the build system of
  **R**. There are no functional changes in the package, and if the
  current version works in your system, there is no need to upgrade.

Version 1.1-2
=============

* This is the first CRAN release.
