
launch_project_selector <- function() {

    suppressPackageStartupMessages(library(shiny,warn.conflicts = F,quietly = T))
    suppressPackageStartupMessages(library(miniUI,warn.conflicts = F,quietly = T))
    suppressPackageStartupMessages(library(rstudioapi,warn.conflicts = F,quietly = T))
    suppressPackageStartupMessages(library(digest,warn.conflicts = F,quietly = T))


    (configpath <- paste0(config_temp(),'/configtext.txt'))
    if (file.exists(configpath)){
        configtext <- data.table::fread(configpath,data.table = F)
    }else{
        configtext <- data.frame(tabchoice='项目',
                                 width=600)
    }
    proj_path <- projectlist()
    file_path <- filelist()


    proj_data <- reactiveVal(get_df(proj_path))
    file_data <- reactiveVal(get_df(file_path))
    shortcut_script <- tags$script(HTML("
  document.addEventListener('keydown', function(e) {
    if (e.key === 'Escape') {
      e.preventDefault();
      Shiny.setInputValue('close_window', Math.random());
    }
  });
"))


    ui <- miniPage(
        shortcut_script,
        tags$style(HTML("
    table, th, td {
      border: none !important;
    }
    table {
      border-collapse: collapse !important;
    }
    td {
      padding: 6px 12px;
    }
  ")),
        miniContentPanel(
            tabsetPanel(
                id = "main_tabs",
                selected = configtext$tabchoice,
                tabPanel("项目",
                         textInput("proj_search", label = NULL, placeholder = "输入关键词..."),
                         tableOutput("proj_table")),
                tabPanel("文件",
                         textInput("file_search", label = NULL, placeholder = "输入关键词..."),
                         tableOutput("file_table")),
                tabPanel("设置",
                         selectInput('tabchoice','默认打开',c('项目','文件'),configtext$tabchoice),
                         numericInput('width','宽度',configtext$width))
            )
        )
    )


    server <- function(input, output, session) {
        observeEvent(input$close_window, {
            stopApp()
        })

        output$proj_table <- renderTable({
            df <- proj_data()
            proj_keyword <- input$proj_search

            if (!is.null(proj_keyword) && nzchar(proj_keyword)) {
                df <- df[grepl(proj_keyword,do::Replace0(df$name,'.Rproj'), ignore.case = TRUE), ]
            }
            if (nrow(df)==0){
                data.frame(
                    ' '='',
                    stringsAsFactors = FALSE,
                    check.names = F
                )
            }else{
                data.frame(
                    ' '=1:length(df$name),
                    ' ' = do::Replace0(df$name,'.Rproj'),
                    ' ' = sapply(df$id, function(id) as.character(actionLink(paste0("open_new_", id), "新窗口"))),
                    ' ' = sapply(df$id, function(id) as.character(actionLink(paste0("open_here_", id), "旧窗口"))),
                    ' ' = sapply(df$id, function(id) as.character(actionLink(paste0("delete_", id), "删除"))),
                    ' ' = sapply(df$id, function(id) as.character(actionLink(paste0("folder_", id), "目录"))),
                    stringsAsFactors = FALSE,
                    check.names = F
                )
            }

        }, sanitize.text.function = identity)

        output$file_table <- renderTable({
            df <- file_data()
            file_keyword <- input$file_search

            if (!is.null(file_keyword) && nzchar(file_keyword)) {
                df <- df[grepl(file_keyword,do::knife_right(basename(df$name),2), ignore.case = TRUE), ]
            }
            if (nrow(df)==0){
                data.frame(
                    ' '='',
                    stringsAsFactors = FALSE,
                    check.names = F
                )
            }else{
                data.frame(
                    ' '=1:length(df$name),
                    ' ' = do::knife_right(basename(df$name),2),
                    ' ' = sapply(df$id, function(id) as.character(actionLink(paste0("open_file_", id), "打开"))),
                    ' ' = sapply(df$id, function(id) as.character(actionLink(paste0("delete_file_", id), "删除"))),
                    ' ' = sapply(df$id, function(id) as.character(actionLink(paste0("folder_file_", id), "目录"))),
                    stringsAsFactors = FALSE,
                    check.names = F
                )
            }

        }, sanitize.text.function = identity)
        observe({
            df_now <- isolate(proj_data())
            if (nrow(df_now)>0){
                for (i in seq_len(nrow(df_now))) {
                    local({
                        id <- df_now$id[i]
                        path <- df_now$path[i]

                        observeEvent(input[[paste0("open_here_", id)]], {
                            openProject(path, newSession = FALSE)
                            stopApp()
                        })

                        observeEvent(input[[paste0("open_new_", id)]], {
                            openProject(path, newSession = TRUE)
                            stopApp()
                        })

                        observeEvent(input[[paste0("delete_", id)]], {
                            df_old <- isolate(proj_data())
                            df_new <- df_old[df_old$path != path, ]
                            deletproject(path)
                            df_new$id <- vapply(df_new$path, digest, FUN.VALUE = character(1), algo = "md5")
                            proj_data(df_new)
                        })

                        observeEvent(input[[paste0("folder_", id)]], {
                            dir_to_open <- dirname(path)
                            if (.Platform$OS.type == "windows") {
                                shell.exec(dir_to_open)
                            } else {
                                system2("open", dir_to_open)
                            }
                        })
                    })
                }
            }
        })
        observe({
            df_now <- isolate(file_data())
            if (nrow(df_now)>0){
                for (i in seq_len(nrow(df_now))) {
                    local({
                        id <- df_now$id[i]
                        path <- df_now$path[i]

                        observeEvent(input[[paste0("open_file_", id)]], {
                            file.edit(path)
                            stopApp()
                        })

                        observeEvent(input[[paste0("delete_file_", id)]], {
                            df_old <- isolate(file_data())
                            df_new <- df_old[df_old$path != path, ]
                            deletfile(path)
                            df_new$id <- vapply(df_new$path, digest, FUN.VALUE = character(1), algo = "md5")
                            file_data(df_new)
                        })

                        observeEvent(input[[paste0("folder_file_", id)]], {
                            dir_to_open <- dirname(path)
                            if (.Platform$OS.type == "windows") {
                                shell.exec(dir_to_open)
                            } else {
                                system2("open", dir_to_open)
                            }
                        })
                    })
                }
            }
        })
        observe({
            configtext <- data.frame(tabchoice=input$tabchoice,
                                     width=input$width)
            data.table::fwrite(configtext,paste0(config_temp(),'/configtext.txt'))
        })
    }
    runGadget(ui, server, viewer = dialogViewer("项目管理器",
                                                width = configtext$width,
                                                height = 800))
}
projectlist <- function(){
    mru <- '/Users/zhangjing/.local/share/rstudio/monitored/lists/project_mru'
    if (file.exists(mru)){
        p1 <- readLines(mru)
    }else{
        p1 <- c()
    }
    p1
    (localmru <- paste0(config_temp(),'/project.txt'))
    if (file.exists(localmru)){
        p2 <- readLines(localmru)
    }else{
        p2 <- c()
    }

    (projects <- unique(c(p1,p2)))
    projects <- projects[file.exists(projects)]
    (projects <- projects[!duplicated(do::file.name(projects))])

    writeLines(projects,localmru)

    projects[order(do::file.name(projects))]
}
deletproject <- function(path){
    mru <- '/Users/zhangjing/.local/share/rstudio/monitored/lists/project_mru'
    if (file.exists(mru)){
        p1 <- set::not(readLines(mru),path)
        writeLines(p1,mru)
    }

    (localmru <- paste0(config_temp(),'/project.txt'))
    if (file.exists(localmru)){
        p2 <- set::not(readLines(localmru),path)
        writeLines(p2,localmru)
    }
}

filelist <- function(){
    mru <- '/Users/zhangjing/.local/share/rstudio/monitored/lists/file_mru'
    if (file.exists(mru)){
        p1 <- readLines(mru)
    }else{
        p1 <- c()
    }
    p1
    (localmru <- paste0(config_temp(),'/file.txt'))
    if (file.exists(localmru)){
        p2 <- readLines(localmru)
    }else{
        p2 <- c()
    }

    (files <- unique(c(p1,p2)))
    files <- files[file.exists(files)]
    (files <- files[!duplicated(do::file.name(files))])
    ck <- sapply(files,function(i){
        all(file.exists(paste0(do::upper.dir(do::upper.dir(i,end.slash = F)),
                               c('DESCRIPTION','NAMESPACE','R','man'))))
    })
    files <- files[!ck]
    (files <- files[tolower(do::right(files,2)) %in% '.r'])
    writeLines(files,localmru)

    files
}
deletfile <- function(path){
    mru <- '/Users/zhangjing/.local/share/rstudio/monitored/lists/file_mru'
    if (file.exists(mru)){
        p1 <- set::not(readLines(mru),path)
        writeLines(p1,mru)
    }

    (localmru <- paste0(config_temp(),'/file.txt'))
    if (file.exists(localmru)){
        p2 <- set::not(readLines(localmru),path)
        writeLines(p2,localmru)
    }
}
get_df <- function(paths) {
    data.frame(
        name = basename(paths),
        path = paths,
        id = vapply(paths, digest, FUN.VALUE = character(1), algo = "md5"),
        stringsAsFactors = FALSE
    )
}
