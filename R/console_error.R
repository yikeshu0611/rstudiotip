#' 设置console中error的打印
#'
#'
#' @returns console
#' @export
#'
console_error <- function(text_color='red',
                          border_color='red',
                          bg_color='white'){
    txt <- '
.rstudio-themes-default .GPWVOE5CII {
    border-left: 6px solid border_color;
    color:text_color;
    background-color: bg_color;
    width: 500px;
}
.rstudio-themes-default .ace_console_error .GPWVOE5CHI {
    white-space: normal;
}
.rstudio-themes-default .GPWVOE5CLI {
    border-left: 6px solid border_color;
    color:text_color;
    background-color: bg_color;
}
.rstudio-themes-default .GPWVOE5CKI {
    background-color: bg_color;
}
.rstudio-themes-default .GPWVOE5CGI {
    color: text_color;
}
.rstudio-themes-default .GPWVOE5CAJ {
    color: text_color;
}
'
txt <- txt %>%
    do::Replace('text_color',text_color) %>%
    do::Replace('border_color',border_color) %>%
    do::Replace('bg_color',bg_color)
append_theme(txt)
warning('请重新启动后再次查看')
stop('请重新启动后再次查看')
}

