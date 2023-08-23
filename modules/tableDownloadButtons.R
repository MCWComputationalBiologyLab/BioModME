tableDownloadButtons <- function(input, output, session, data) {
  
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
