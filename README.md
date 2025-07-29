# What is cepreader?

Cornell Ecology Programs (CEP) introduced a popular data file format
for ecological community data, and this format was later adopted by
CANOCO software with small modifications. Many legacy data sets are
still written in these formats. The **cepreader** package provides
function `readCEP()` to read these data files into **R** as a
`data.frame` or a sparse **Matrix**. The function does not handle all
possible CEP formats, but it handles transposed condensed format
which was the most popular one plus some other popular formats.

## Why a Package?

The package has only function `readCEP()`. This function was earlier
included in the **vegan** package. However, the function is written in
`FORTRAN`, and `FORTRAN` input can interfere with other compiled code,
in particular in `Windows`. To minimize the risk of conflicts, reading
of CEP files was moved to a separate package. The CEP format can save
data in flexible forms as defined by a `FORTRAN` format specification 
in the file, and this format is best interpreted using `FORTRAN` code.
The **vegan** package has function `read.cep()` that uses only **R**
code and can read many files written in transposed condensed format, 
but this package is more versatile and robust and can handle a wider
array of CEP formats.

#### Released version
[![R build
status](https://github.com/vegandevs/cepreader/workflows/R-CMD-check/badge.svg)](https://github.com/vegandevs/cepreader/actions)
[![CRAN version](https://www.r-pkg.org/badges/version/cepreader)](https://cran.rstudio.com/web/packages/cepreader/index.html) 
[![status](https://tinyverse.netlify.app/badge/cepreader)](https://CRAN.R-project.org/package=cepreader)
[![](https://cranlogs.r-pkg.org/badges/grand-total/cepreader)](https://cran.rstudio.com/web/packages/cepreader/index.html)
