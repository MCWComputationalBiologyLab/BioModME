tableDownloadButtonsUI <- function(id, 
                                   placement = "bottom",
                                   margin_top = "0px") {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      style = paste0("margin-top: ", margin_top, ";"),
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
          downloadButton(
            outputId = ns("bttn_download_model_results_xlsx"),
            label = NULL,
            icon = icon("table", lib = "font-awesome"),
            style = "z-index: 100;"
          ),
          span(class = paste0("custom-tooltip ", placement), "xlsx")
      ),
      
      div(class="custom-tooltip-wrapper",
          actionButton(
            inputId = ns("bttn_download_model_results_new_window"),
            label = NULL,
            # icon = icon("arrow-up-right-from-square", lib = "font-awesome"),
            icon = icon("square-arrow-up-right", lib = "font-awesome"),
            style = "z-index: 100;"
          ),
          span(class = paste0("custom-tooltip ", placement), "New Window")
      )
    ),
    
    # Custom CSS for the tooltip
    tags$style(HTML("
                    
      .custom-tooltip.bottom {
        visibility: visible;
      /* This positions it right below the button */
        top: 100%;   
        left: 50%;
        transform: translateX(-50%);
      /* This provides a little space between the button and the tooltip */
        margin-top: 5px;   
      }
      
      .custom-tooltip.top {
          bottom: 100%;
          left: 50%;
          transform: translateX(-50%);
      /* This provides a little space between the button and the tooltip */
          margin-bottom: 5px;   
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
      /* Minimum height */
        min-height: 30px;
      /* Maximum height, adjust as needed */
        max-height: 70px;
      /* Ensures content that exceeds max height isn't visible */
        overflow: hidden;  
      }
      
      .custom-tooltip-wrapper:hover .custom-tooltip {
        visibility: visible;
        opacity: 1;
      }
      
    "))
  )
}
