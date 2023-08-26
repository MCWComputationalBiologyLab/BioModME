alignTwoWidgets <- function(widget1, widget2, padding_top = "0px") {
  tags$div(style = "display: flex; align-items: center;",
           widget1,
           tags$div(style = paste0("padding-top: ", padding_top, ";"), widget2)
  )
}