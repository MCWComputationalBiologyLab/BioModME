tableLayoutDualColumns <- function(
    labels, 
    widgets, 
    widgets2, 
    headerLabels = c("Header 1", "Header 2"),
    firstColWidth = "30%",
    fontSize = "16px", 
    paddingBottom = "15px",
    isBold = TRUE,
    headerFontSize = "16px", 
    headerIsBold = TRUE, 
    headerAlignment = "center",
    removeFirstCol = FALSE,
    colPercents = c(50, 50),
    colSpacing = "10px"
) {
  
  if(removeFirstCol) {
    firstColWidth <- "0%"
    labels <- NULL
  }
  
  widgetColWidth1 <- paste0(colPercents[1], "%")
  widgetColWidth2 <- paste0(colPercents[2], "%")
  
  headerFontWeight <- ifelse(headerIsBold, "bold", "normal")
  
  if(length(widgets) != length(widgets2)) {
    stop("Length of widgets must be consistent!")
  }
  
  labelFontWeight <- ifelse(isBold, "bold", "normal")
  
  header <- if (!is.null(headerLabels)) {
    tags$tr(
      if(!removeFirstCol) tags$td(""), 
      tags$td(
        style = paste0("width:", widgetColWidth1, 
                       "; text-align:", headerAlignment,
                       "; font-size:", headerFontSize, 
                       "; font-weight:", headerFontWeight, 
                       if (removeFirstCol) paste0("; padding-right:", colSpacing) else ""),
        headerLabels[1]
      ),
      tags$td(
        style = paste0("width:", widgetColWidth2, 
                       "; text-align:", headerAlignment, 
                       "; font-size:", headerFontSize, 
                       "; font-weight:", headerFontWeight, ";"),
        headerLabels[2]
      )
    )
  } else NULL
  
  rows <- lapply(1:length(widgets), function(i) {
    tags$tr(
      width = "100%",
      if(!removeFirstCol) {
        tags$td(
          width = firstColWidth,
          div(
            style = paste0("font-size:", fontSize, ";", 
                           "padding-bottom:", paddingBottom, ";", 
                           "font-weight:", labelFontWeight, ";"),
            labels[i]
          )
        )
      },
      tags$td(
        style = paste0("width:", widgetColWidth1, "; padding: 0px;", 
                       if (removeFirstCol) paste0(" padding-right:", colSpacing) else ""),
        if(is.null(widgets[[i]])) "" else widgets[[i]]
      ),
      tags$td(
        style = paste0("width:", widgetColWidth2, "; padding: 0px;"),
        if(is.null(widgets2[[i]])) "" else widgets2[[i]]
      )
    )
  })
  
  tags$table(
    class = "input_table",
    header,
    do.call(tags$tbody, rows)
  )
}



# tableLayoutDualColumns <- function(
#     labels, 
#     widgets, 
#     widgets2, 
#     headerLabels = c("Header 1", "Header 2"),
#     firstColWidth = "30%",
#     fontSize = "16px", 
#     paddingBottom = "15px",
#     isBold = TRUE,
#     headerFontSize = "16px", 
#     headerIsBold = TRUE, 
#     headerAlignment = "center") 
#   {
#   
#   # @labels - A vector of label names for the widgets.
#   # @widgets - A list of Shiny UI input elements corresponding to the labels (first set).
#   # @widgets2 - A list of Shiny UI input elements corresponding to the labels (second set).
#   # @headerLabels - A vector of two header labels for the table's first row.
#   # @firstColWidth - The width of the first column (for the labels).
#   # @fontSize - The font size for the label text.
#   # @paddingBottom - The padding at the bottom of the label text.
#   # @isBold - Logical; If TRUE, the label text is displayed in bold.
#   # @headerFontSize - Font size for the table headers.
#   # @headerIsBold - Logical; If TRUE, the header text is displayed in bold.
#   # @headerAlignment - Alignment of the header text. Options: left, center, right.
#   
#   widgetColWidth <- paste0((100 - as.numeric(sub("%", "", firstColWidth)))/2, "%")
#   
#   headerFontWeight <- ifelse(headerIsBold, "bold", "normal")
#   
#   if(length(labels) != length(widgets) || length(widgets) != length(widgets2)) {
#     stop("Length of labels and widgets must be consistent!")
#   }
#   
#   labelFontWeight <- ifelse(isBold, "bold", "normal")
#   
#   header <- tags$tr(
#     tags$td(""),
#     tags$td(
#       style = paste0("width:", widgetColWidth, 
#                      "; text-align:", headerAlignment,
#                      "; font-size:", headerFontSize, 
#                      "; font-weight:", headerFontWeight, ";"),
#       headerLabels[1]
#     ),
#     tags$td(
#       style = paste0("width:", widgetColWidth, 
#                      "; text-align:", headerAlignment, 
#                      "; font-size:", headerFontSize, 
#                      "; font-weight:", headerFontWeight, ";"),
#       headerLabels[2]
#     )
#   )
#   
#   rows <- lapply(1:length(labels), function(i) {
#     tags$tr(
#       width = "100%",
#       tags$td(
#         width = firstColWidth,
#         div(
#           style = paste0("font-size:", fontSize, ";", 
#                          "padding-bottom:", paddingBottom, ";", 
#                          "font-weight:", labelFontWeight, ";"),
#           labels[i]
#         )
#       ),
#       tags$td(
#         style = paste0("width:", widgetColWidth, "; padding: 0px;"),
#         widgets[[i]]
#       ),
#       tags$td(
#         style = paste0("width:", widgetColWidth, "; padding: 0px;"),
#         widgets2[[i]]
#       )
#     )
#   })
#   
#   tags$table(
#     class = "input_table",
#     header,
#     do.call(tags$tbody, rows)
#   )
# }