library(shiny)
library(bs4Dash)

library(shinyBS)

tableDownloadButtonsUI <- function(id, placement = "bottom") {
  ns <- NS(id)
  
  tagList(
    fluidRow(

      div(class="custom-tooltip-wrapper",
          actionButton(
            inputId = ns("bttn_download_model_results_copy"),
            label = NULL,
            icon = icon("copy", lib = "font-awesome"),
            style = "z-index: 100;"
          ),
          span(class = paste0("custom-tooltip ", placement), "Copy")
      ),
      
      div(class="custom-tooltip-wrapper",
          downloadButton(
            outputId = ns("bttn_download_model_results_csv"),
            label = NULL,
            icon = icon("file-csv", lib = "font-awesome"),
            style = "z-index: 100;"
          ),
          span(class = paste0("custom-tooltip ", placement), "CSV")
      ),
      div(class="custom-tooltip-wrapper",
          actionButton(
            inputId = ns("bttn_download_model_results_xlsx"),
            label = NULL,
            icon = icon("copy", lib = "font-awesome"),
            style = "z-index: 100;"
          ),
          span(class = paste0("custom-tooltip ", placement), "xlsx")
      ),
      
      div(class="custom-tooltip-wrapper",
          downloadButton(
            outputId = ns("bttn_download_model_results_new_window"),
            label = NULL,
            icon = icon("file-csv", lib = "font-awesome"),
            style = "z-index: 100;"
          ),
          span(class = paste0("custom-tooltip ", placement), "New Window")
      )
    ),
    
    # Custom CSS for the tooltip
    tags$style(HTML("
                    
      .custom-tooltip.bottom {
        visibility: visible;
        top: 100%;   /* This positions it right below the button */
        left: 50%;
        transform: translateX(-50%);
        margin-top: 5px;   /* This provides a little space between the button and the tooltip */
      }
      
      .custom-tooltip.top {
          bottom: 100%;
          left: 50%;
          transform: translateX(-50%);
          margin-bottom: 5px;   /* This provides a little space between the button and the tooltip */
      }
      
      .custom-tooltip-wrapper {
        position: relative;
        display: inline-block;
      }
      
      .custom-tooltip {
        visibility: hidden;
        background-color: black;
        color: white;
        text-align: center;
        padding: 5px;
        border-radius: 5px;
        position: absolute;
        z-index: 101;
        bottom: 100%;
        left: 50%;
        transform: translateX(-50%);
        opacity: 0;
        transition: opacity 0.3s;
        pointer-events: none;
        max-width: 200px; 
        white-space: nowrap; 
        box-sizing: border-box;
        min-height: 30px;  /* Minimum height */
        max-height: 70px;  /* Maximum height, adjust as needed */
        overflow: hidden;  /* Ensures content that exceeds max height isn't visible */
      }
      
      .custom-tooltip-wrapper:hover .custom-tooltip {
        visibility: visible;
        opacity: 1;
      }
      
    "))
  )
}

# Server logic for the module
tableDownloadButtons <- function(input, output, session, resultsRV) {
  
  data <- reactive({ resultsRV$results.model.final })
  
  observeEvent(input$bttn_download_model_results_copy, {
    clipr::write_clip(data())
    showModal(modalDialog(
      title = "Copy",
      "Table copied to clipboard.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  output$bttn_download_model_results_csv <- downloadHandler(
    filename = function() {
      "download.csv"
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  output$bttn_download_model_results_xlsx <- downloadHandler(
    filename = function() {
      "download.xlsx"
    },
    content = function(file) {
      writexl::write_xlsx(as.data.frame(data()), path = file)
    }
  )
  
  # Open in New Window
  observeEvent(input$bttn_download_model_results_new_window, {
    showTableInPopup(data(), session, width = 900, height = 500)
  })
}


# Main UI
ui <- dashboardPage(
  header = dashboardHeader(
    title = dashboardBrand(
      title = "BioModME"
    )),
  # navbar = bs4DashNavbar(),
  sidebar = dashboardSidebar(),
  body = dashboardBody(
    box(
      tableDownloadButtonsUI("myButtons", placement = "bottom"),
      DTOutput("irisTable")
    )
  ),
  title = "bs4Dash"
)

# Main Server
server <- function(input, output, session) {
  rv <- reactiveValues()
  rv$results.model.final <- iris
  
  callModule(tableDownloadButtons, "myButtons", rv)
  
  output$irisTable <- renderDT({ iris })
}

shinyApp(ui, server)
