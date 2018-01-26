`readCEP` <-
    function (file, maxdata = 10000, positive = TRUE)
{
    ## launch external binary to write R input data
    cepread <- file.path(path.package("cepreader"),
                                      paste0("bin", .Platform$r_arch),
                                      "cepread")
    cepfile <- normalizePath(file)
    outfile <- tempfile()
    retval <- system2(cepread, args = c(cepfile, outfile))
    if (retval) {
        switch(retval,
               stop("maxdata too low"),
               stop("unknown data type"),
               stop("unknown error"))
    }
    ## source result: will return results in 'out'
    source(outfile)
    unlink(outfile)
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
        out <- out[rsum > 0, csum > 0]
    }
    out
}
