#' 删除rstudio的启动动画
#'
#' @returns
#' @export
#'
delete_splash <- function(){
    if (do::is.mac()){
        writeLines('','/Applications/RStudio.app/Contents/Resources/app/.webpack/renderer/splash/index.html')
        message('OK')
    }else if (do::is.windows()){
        path <- 'C:/Program Files/RStudio/resources/app/.webpack/renderer/splash/index.html'
        if (file.exists(path)){
            writeLines('',path)
            message('OK')
        }else{
            message('请输入rstudio.exe的路径')
            x <- readline()
            file <- paste0(do::Replace0(do::formal_dir(do::Replace0(x,'"')),'rstudio\\.exe'),
                   'resources/app/.webpack/renderer/splash/index.html')
            if (file.exists(file)){
                writeLines('',file)
            }else{
                message('请手动清空rsutido安装目录下的/resources/app/.webpack/renderer/splash/index.html文件里面的内容')
            }
        }
    }
}
