`readCEP` <-
    function (file, maxdata = 10000, positive = TRUE, sparseMatrix = FALSE,
              long = FALSE, ...)
{
    ## check allowed formats
    if (sparseMatrix && long)
        stop("only one of 'long' and 'sparseMatrix' can be TRUE")
    ## launch external binary to write R input data
    cepread <- file.path(path.package("cepreader"),
                                      "bin", "cepread")
    cepfile <- normalizePath(file, mustWork = TRUE)
    outfile <- tempfile()
    on.exit(unlink(outfile))
    retval <- system2(cepread, args = c(cepfile, outfile, maxdata),
                      stderr = NULL)
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
    ## Output is either in "long" format of data triplets (row name,
    ## column name, data value) or a matrix-like rows x columns
    ## table. Internally data are already in triplets and needs only
    ## index numbers changed to names. Normal matrix and data.frame is
    ## made via sparseMatrix.
    if (long) {
        out <- data.frame("row" = rnam[out$i], "col" = cnam[out$j],
                          "val" = out$x, row.names = NULL)
        if (positive) {
            if (any(out$val <= 0))
                out <- out[out$val > 0,]
        }
    } else {
        ## always make sparseMatrix first
        out <- sparseMatrix(i = out$i, j = out$j, x = out$x,
                            dimnames = list(rnam, cnam), ...)
        if (positive) {
            rsum <- rowSums(out)
            csum <- colSums(out)
            if (any(rsum <= 0) || any(csum <= 0))
                out <- out[rsum > 0, csum > 0, drop = FALSE]
        } ## change to data.frame of dense matrix
        if (!sparseMatrix)
            out <- as.data.frame(as.matrix(out))
    }
    out
}
