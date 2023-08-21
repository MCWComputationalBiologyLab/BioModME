showPlotInPopup <- function(plotFunc, session, width = 600, height = 400) {
  tmpFile <- tempfile(fileext = ".png")
  
  # Save the plot to a temporary PNG file
  png(filename = tmpFile, width = width, height = height)
  plotFunc()
  dev.off()
  
  # Convert PNG to base64
  base64Img <- base64enc::dataURI(file = tmpFile, mime = "image/png")
  
  # Prepare the JavaScript function call with the given width and height
  jsFunction <- sprintf("plotPlot('%s', %d, %d)", base64Img, width, height)
  
  # Execute the JavaScript function using Shiny's session object
  session$sendCustomMessage(type = "executeJS", message = list(js = jsFunction))
}