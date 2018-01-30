exec <- "cepread"
if(WINDOWS) exec <- paste0(exec, ".exe")
if ( any(file.exists(exec)) ) {
    dest <- file.path(R_PACKAGE_DIR, 'bin')
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    file.copy(exec, dest, overwrite = TRUE)
}
