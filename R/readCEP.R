`readCEP` <-
    function (file, maxdata = 10000, positive = TRUE, sparseMatrix = FALSE,
              ...)
{
    ## launch external binary to write R input data
    cepread <- file.path(path.package("cepreader"),
                                      "bin", "cepread")
    cepfile <- normalizePath(file, mustWork = TRUE)
    outfile <- tempfile()
    on.exit(unlink(outfile))
    retval <- system2(cepread, args = c(cepfile, outfile, maxdata))
    if (retval) {
        switch(as.character(retval),
               "1" = stop("too many non-zero entries: increase 'maxdata' from ",
                    maxdata),
               "2" = stop("unknown CEP file type"),
               stop("error number ", retval))
    }
    ## source result: will return results in 'out'
    source(outfile)
    ## remove blanks from dimnames
    cnam <- out$jnames
    cnam <- gsub(" ", "", cnam)
    cnam <- make.names(cnam, unique = TRUE)
    rnam <- out$inames
    rnam <- gsub(" ", "", rnam)
    rnam <- make.names(rnam, unique = TRUE)
    ## Make a sparse matrix. It is trivial to make a dense matrix
    ## manually (and we did so previously in Fortran code), but we
    ## want to have an option of returning a Matrix::sparseMatrix
    ## object.
    out <- sparseMatrix(i = out$i, j = out$j, x = out$x,
                        dimnames = list(rnam, cnam), ...)
    if (positive) {
        rsum <- rowSums(out)
        csum <- colSums(out)
        if (any(rsum <= 0) || any(csum <= 0))
            out <- out[rsum > 0, csum > 0, drop = FALSE]
    }
    if (!sparseMatrix)
        out <- as.data.frame(as.matrix(out))
    out
}
