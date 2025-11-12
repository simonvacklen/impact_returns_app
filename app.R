library(shiny)
library(readxl)
library(dplyr)
library(quantmod)
library(reshape2)
library(flextable)
library(officer)
#install.packages("officer")

save_directory <- tempdir()  # Temporary directory

ui <- fluidPage(
  titlePanel("Automatic Impact Report App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", accept = c(".xlsx", ".xls")),
      actionButton("process", "Process File"),
      br(),
      uiOutput("downloadLinkDocx")
    ),
    mainPanel(
      # No download link here anymore!
    )
  )
)

server <- function(input, output) {
  observeEvent(input$process, {
    req(input$file)
    withProgress(message = 'Processing file...', value = 0, {
      file_name <- input$file$name
      save_path <- file.path(save_directory, file_name)
      file.copy(input$file$datapath, save_path)
      incProgress(0.2, detail = "File saved.")
      
      load("r_Other_Code/scriptrunner.RData")
      output_list <- scriptrunner(excel_file = save_path, rounding = "Million")
      
      incProgress(0.2, detail = "Script runner completed.")
      
      name <- as.data.frame(output_list[[1]][1])[1, 2]
      output_name <- file.path(tempdir(), paste0("Impact_Report_", name, ".docx"))
      load("r_Other_Code/reportmaker.RData")
      reportmaker(source_path = "AutoReport/Template_Standard_V3.docx",
                  output_path = output_name,
                  output_list = output_list
      )
      incProgress(0.2, detail = "Generated DOCX completed.")
      
      output$downloadLinkDocx <- renderUI({
        tagList(
          p(""),
          downloadLink("downloadDocx", "Download Impact Report")
        )
      })
      
      output$downloadDocx <- downloadHandler(
        filename = function() {
          basename(output_name)
        },
        content = function(file) {
          file.copy(output_name, file)
        }
      )
      incProgress(0.4, detail = "Done!")
    })
  })
}

shinyApp(ui, server)
