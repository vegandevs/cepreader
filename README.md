#### Released version
[![CRAN version](http://www.r-pkg.org/badges/version/cepreader)](http://cran.rstudio.com/web/packages/cepreader/index.html) [![](http://cranlogs.r-pkg.org/badges/grand-total/cepreader)](http://cran.rstudio.com/web/packages/cepreader/index.html)

#### Build status
[![Build Status](https://travis-ci.org/vegandevs/cepreader.svg?branch=master)](https://travis-ci.org/vegandevs/cepreader) 
[![Build status](https://ci.appveyor.com/api/projects/status/ydo7gbvuckc916kq/branch/master?svg=true)](https://ci.appveyor.com/project/jarioksa/cepreader/branch/master)
[![codecov.io](https://codecov.io/github/vegandevs/cepreader/coverage.svg?branch=master)](https://codecov.io/github/vegandevs/cepreader?branch=master)

# What is cepreader?

Cornell Ecology Programs (CEP) introduced a popular data file format
for ecological community data, and this format was later adopted by
CANOCO software with small modifications. Many legacy data sets are
still written in these formats. The **cepreader** package provides
function `readCEP()` to read these data files into **R** as
`data.frame`. The function does not handle all possible CEP formats,
but it handles ``transposed condensed'' format which was the most
popular one plus some other popular formats.

## Why a Package?

The package has only function `readCEP()`. This function was earlier
included in the **vegan** package. However, the function is written in
`FORTRAN`, and `FORTRAN` input can interfere with other compiled code,
in particular in `Windows`. To minimize the risk of conflicts, reading
of CEP files was moved to a separate package. The CEP format can save
data in flexible forms as defined by a `FORTRAN` format stored in the
file, and this format is best interpreted using `FORTRAN` code.
