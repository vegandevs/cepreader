`readCEP` <-
    function (file, maxdata = 10000, positive = TRUE)
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
    ## sanitize dimname
    cnam <- names(out)
    cnam <- gsub(" ", "", cnam)
    cnam <- make.names(cnam, unique = TRUE)
    names(out) <- cnam
    rnam <- rownames(out)
    rnam <- gsub(" ", "", rnam)
    rnam <- make.names(rnam, unique = TRUE)
    rownames(out) <- rnam
    if (positive) {
        rsum <- rowSums(out)
        csum <- colSums(out)
        if (any(rsum <= 0) || any(csum <= 0))
            out <- out[rsum > 0, csum > 0, drop = FALSE]
    }
    out
}
