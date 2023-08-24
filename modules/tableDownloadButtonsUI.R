tableDownloadButtonsUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    style = "margin-bottom: -35px;",
    actionButton(
      inputId = ns("bttn_download_model_results_copy"),
      label = NULL,
      icon = icon("copy", lib = "font-awesome"),
      style = "z-index: 100;",
      title = "Copy", 
      `data-toggle` = "tooltip", 
      `data-placement` = "bottom"
    ),
    downloadButton(
      outputId = ns("bttn_download_model_results_csv"),
      label = NULL,
      icon = icon("file-csv", lib = "font-awesome"),
      style = "z-index: 100;",
      title = "Download csv", 
      `data-toggle` = "tooltip", 
      `data-placement` = "bottom"
    ),
    downloadButton(
      outputId = ns("bttn_download_model_results_xlsx"),
      label = NULL,
      icon = icon("table", lib = "font-awesome"),
      style = "z-index: 100;",
      title = "Download xlsx", 
      `data-toggle` = "tooltip", 
      `data-placement` = "bottom"
    ),
    actionButton(
      inputId = ns("bttn_download_model_results_new_window"),
      label = NULL,
      icon = icon("arrow-up-right-from-square", lib = "font-awesome"),
      style = "z-index: 100;",
      title = "Open in new window", 
      `data-toggle` = "tooltip", 
      `data-placement` = "bottom"
    )
  )
}
