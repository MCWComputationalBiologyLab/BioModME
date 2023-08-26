# Define the function to show the table in a popup
showTableInPopup <- function(data, session, width = 600, height = 400) {
  # Create a temporary file
  tmpFile <- tempfile(fileext = ".html")
  
  # Render data as an HTML table with DT::datatable
  tableWidget <- 
    DT::datatable(data,
                  options = list(
                    autoWidth = TRUE,
                    ordering = FALSE,
                    dom = "ltipr",
                    lengthMenu = list(c(5, 15, 100, -1),
                                      c('5', '15', "100", 'All')),
                    pageLength = 100
                  ))
  
  # Save widget to the temporary file
  saveWidget(tableWidget, file = tmpFile, selfcontained = TRUE)
  
  # Read the saved widget into a string
  tableHTML <- readChar(tmpFile, file.info(tmpFile)$size)
  
  # Prepare the JavaScript function call with the given width and height
  jsFunction <- sprintf("tablePopup(%s, %d, %d)", 
                        toJSON(tableHTML, auto_unbox = TRUE), 
                        width, 
                        height)
  
  # Execute the JavaScript function using Shiny's session object
  session$sendCustomMessage(type = "executeJS", message = list(js = jsFunction))
}