library(shiny)
library(readxl)
library(dplyr)
library(quantmod)
library(reshape2)
library(flextable)
library(officer)
#install.packages("officer")

# Define the directory relative to the app's location
save_directory <- tempdir()  # Temporary directory

ui <- fluidPage(
  titlePanel("Upload Excel File"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", accept = c(".xlsx", ".xls")),
      actionButton("process", "Process File")
    ),
    mainPanel(
      tags$div(
        id = "progress-container",
        tags$style(HTML("
          #progress-container {
            display: flex;
            justify-content: center;
            align-items: center;
            height: 50px; /* Adjust height as needed */
          }
          .shiny-progress {
            width: 100%;
            position: relative;
          }
          .download-container {
            margin-top: 20px;
          }
          .download-container p, .download-container a {
            display: inline;
            margin-right: 10px;
          }
        "))
      ),
      uiOutput("downloadLinkDocx")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$process, {
    req(input$file)
    
    # Show progress message
    withProgress(message = 'Processing file...', value = 0, {
      # Get the uploaded file name
      file_name <- input$file$name
      
      # Define the full path where the file will be saved
      save_path <- file.path(save_directory, file_name)
      
      # Save the uploaded file to the specified directory
      file.copy(input$file$datapath, save_path)
      
      # Increment progress
      incProgress(0.2, detail = "File saved.")
      
      #####----------Run Calculations
      load("r_Other_Code/scriptrunner.RData")
      output_list <- scriptrunner(excel_file = save_path, rounding = "M")
      incProgress(0.2, detail = "Script runner completed.")
      
      #####----------DOCX Generation
      name <- as.data.frame(output_list[[1]][1])[1, 2]
      print("name:")
      print(name)
      
      output_name <- file.path(tempdir(), paste0("Impact_Report_", name, ".docx"))
      
      print("output_name:")
      print(output_name)
      
      print("temp_dir:")
      print(tempdir())
      
      load("r_Other_Code/reportmaker.RData")
      reportmaker(source_path = "AutoReport/Template_Standard_V2.docx",
                  output_path = output_name
      )
      incProgress(0.2, detail = "Generated DOCX completed.")
      
      # Generate download link for DOCX with description
      output$downloadLinkDocx <- renderUI({
        tags$div(
          class = "download-container",
          p("Download the DOCX report generated from your data:"),
          downloadLink("downloadDocx", "Download DOCX Report")
        )
      })
      
      # Serve the DOCX file for download
      output$downloadDocx <- downloadHandler(
        filename = function() {
          basename(output_name)  # Return the base name of the DOCX file
        },
        content = function(file) {
          file.copy(output_name, file)  # Copy the DOCX file to the download location
        }
      )
      
      # Finish progress
      incProgress(0.4, detail = "Done!")
    })
  })
}

shinyApp(ui, server)


