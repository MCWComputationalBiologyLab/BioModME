tableLayout <- function(labels, 
                        widgets, 
                        firstColWidth = "30%", 
                        secondColWidth = "70%", 
                        fontSize = "16px", 
                        paddingBottom = "15px", 
                        isBold = TRUE) {
  # Inputs: 
  #   @labels - A vector of label names for the widgets.
  #   @widgets - A list of Shiny UI input elements corresponding to the labels.
  #   @firstColWidth - The width of the first column (for the labels). 
  #   @secondColWidth - The width of the second column (for the widgets). 
  #   @fontSize - The font size for the label text.
  #   @paddingBottom - The padding at the bottom of the label text. 
  #   @isBold - Logical; If TRUE, the label text is displayed in bold. 
  
  if(length(labels) != length(widgets)) {
    stop("Length of labels must be the same as the length of widgets!")
  }
  
  fontWeight <- ifelse(isBold, "bold", "normal")
  
  rows <- lapply(1:length(labels), function(i) {
    tags$tr(
      width = "100%",
      tags$td(
        width = firstColWidth,
        div(
          style = paste0("font-size:", fontSize, ";", 
                         "padding-bottom:", paddingBottom, ";", 
                         "font-weight:", fontWeight, ";"),
          labels[i]
        )
      ),
      tags$td(
        width = secondColWidth,
        widgets[[i]]
      )
    )
  })
  
  tags$table(class = "table-widgets", do.call(tagList, rows))
}

# Example
# ui <- fluidPage(
#   tableLayout(
#     labels = c("Reactants", "Products", "Modifiers"),
#     widgets = list(
#       textInput(inputId = "PI_CC_reactants", label = NULL, value = "", placeholder = "x1, x2"),
#       textInput(inputId = "PI_CC_products", label = NULL, value = "", placeholder = "y1"),
#       textInput(inputId = "PI_CC_modifiers", label = NULL, value = "", placeholder = "mod1")
#     ),
#     firstColWidth = "40%", 
#     secondColWidth = "60%", 
#     fontSize = "18px", 
#     paddingBottom = "10px", 
#     isBold = FALSE
#   )
# )
# 
# server <- function(input, output, session) {}
# 
# shinyApp(ui, server)
