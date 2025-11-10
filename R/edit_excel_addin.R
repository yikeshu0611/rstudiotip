edit_excel_addin <- function() {
    library(shiny)
    library(miniUI)
    library(DT)
    library(openxlsx)

    ui <- miniPage(
        gadgetTitleBar("Edit Excel File"),
        miniContentPanel(
            fileInput("file", "Choose Excel File (.xlsx)", accept = ".xlsx"),
            DTOutput("table"),
            actionButton("save", "💾 Save Excel")
        )
    )

    server <- function(input, output, session) {
        data <- reactiveVal()

        observeEvent(input$file, {
            req(input$file)
            df <- openxlsx::read.xlsx(input$file$datapath)
            data(df)
        })

        output$table <- renderDT({
            req(data())
            datatable(data(), editable = TRUE)
        }, server = FALSE)

        observeEvent(input$save, {
            req(data())
            info <- input$table_cell_edit
            if (!is.null(info)) {
                df <- data()
                df[info$row, info$col] <- info$value
                data(df)
            }
            # 保存文件到原路径（覆盖）
            path <- input$file$datapath
            file.copy(path, paste0(path, "_backup.xlsx"), overwrite = TRUE)
            openxlsx::write.xlsx(data(), path)
            showNotification("Saved successfully!", type = "message")
        })

        observeEvent(input$done, {
            stopApp()
        })
    }

    runGadget(ui, server)
}
