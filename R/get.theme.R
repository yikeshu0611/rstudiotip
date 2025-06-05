.onAttach <- function(...){
    suppressPackageStartupMessages(library(dplyr,quietly = T,warn.conflicts = F))
}
get.theme <- function(){
    (theme <- do::Replace0(rstudioapi::getThemeInfo()$editor,' \\(default\\)'))
    (sprintf('/Applications/RStudio.app/Contents/Resources/app/resources/themes/%s.rstheme',theme))
}
append_theme <- function(txt){
    (th <- get.theme())
    themetxt <- c(readLines(th),txt)
    writeLines(themetxt,th)
}
