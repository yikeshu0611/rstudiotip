config_temp <- function (){
    if (do::is.windows()) {
        temp <- paste0(do::upper.dir(do::upper.dir(list.files(.libPaths(),
                                                              pattern = "devtools", full.names = TRUE))), packageName())
        temp <- do::last(temp[which.max(nchar(temp))])
        if (!dir.exists(temp)) {
            ck <- dir.create(temp, showWarnings = FALSE, recursive = TRUE)
            if (isFALSE(ck)) {
                temp <- "C:/devtools"
                ck <- dir.create(temp, showWarnings = FALSE,
                                 recursive = TRUE)
            }
        }
        temp
    }
    else {
        temp <- sapply(.libPaths(), function(i) paste0(do::upper.dir(do::upper.dir(do::upper.dir(i))),
                                                       packageName()))
        names(temp) <- NULL
        temp <- do::last(temp[which.max(nchar(temp))])
        if (!dir.exists(temp))
            dir.create(temp, showWarnings = FALSE, recursive = TRUE)
        temp
    }
}
