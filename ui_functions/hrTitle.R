hrTitle <- function(textToShow, 
                    position = "center", 
                    color = "lightgrey", 
                    textColor = "grey", 
                    verticalPosition = "inline") {
  
  position_class <- switch(position,
                           "left" = "left-aligned",
                           "center" = "centered",
                           "right" = "right-aligned",
                           "centered")
  
  vertical_class <- switch(verticalPosition,
                           "top" = "top-positioned",
                           "inline" = "inline-positioned",
                           "top-positioned")
  
  tags$div(
    class = paste("header-line", position_class, vertical_class),
    style = paste("border-color:", color, ";"),
    tags$span(textToShow, style = paste("color:", textColor, ";"))
  )
}