#' 修改标题颜色
#'
#' @param color 颜色
#'
#' @returns header color
#' @export
#'
add_color_to_header <- function(color='red'){
    (theme <- do::Replace0(rstudioapi::getThemeInfo()$editor,' \\(default\\)'))
    (tf <- sprintf('/Applications/RStudio.app/Contents/Resources/app/resources/themes/%s.rstheme',theme))
    txt <- '
.ace_sectionhead{

 color:%s;
}'
    txt <- sprintf(txt,color)

    themetxt <- readLines(tf)
    themetxt <- c(themetxt,txt)
    writeLines(themetxt,tf)
    rstudioapi::restartSession()
    message('OK')
}
