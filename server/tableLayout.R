# 
# tableLayout <- function(picker1, picker2) {
#   
#   tags$table(
#     class = "input_table",
#     
#   )
# }

tableLayout <- function(...) {
  
  tagList(
    # This makes web page load the JS file in the HTML head.
    # The call to singleton ensures it's only included once
    # in a page.
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(src = "tableLayout-input-binding.js")
      )
    )
  )
    
  fluidRow(
    lapply(list2(...), function(uiElement) {
      uiElement
    })
  )
}

# tags$table(
#   class = "input_table",
#   tags$tr(
#     width = "100%",
#     tags$td(width = "30%",
#             div(style = "font-size:16px;",
#                 "Name")),
#     tags$td(width = "70%",
#             textInput(
#               inputId = "canvas_var_name",
#               label = NULL,
#               value = "",
#             ))
#   ),
#   tags$tr(
#     width = "100%",
#     tags$td(width = "30%",
#             div(style = "font-size:16px;",
#                 "Initial Value")),
#     tags$td(
#       width = "70%",
#       textInput(
#         inputId = "canvas_var_iv",
#         label = NULL,
#         value = "",
#       )
#     )
#   ),
#   tags$tr(
#     width = "100%",
#     tags$td(width = "30%",
#             div(style = "font-size:16px;",
#                 "Unit")),
#     tags$td(
#       width = "70%",
#       pickerInput(
#         inputId = "var_unit",
#         label = NULL,
#         choices = c(),
#       )
#     )
#   ),
#   tags$tr(class = "blank_row"),
#   tags$tr(class = "blank_row"),
#   tags$tr(class = "blank_row"),
#   tags$tr(
#     width = "100%",
#     tags$td(width = "30%",
#             tags$div(style = "font-size:16pX;",
#                      "Color")),
#     tags$td(
#       width = "70%",
#       colourInput(
#         inputId = "canvas_var_color",
#         label = NULL,
#         value = "#ADD8E6"
#       )
#     )
#   ),
#   tags$tr(
#     width = "100%",
#     tags$td(width = "30%",
#             div(style = "font-size:16px;",
#                 "Shape")),
#     tags$td(
#       width = "70%",
#       pickerInput(
#         inputId = "var_shape",
#         label = NULL,
#         choices = c()
#       )
#     )
#   ),
#   tags$tr(
#     width = "100%",
#     tags$td(width = "30%",
#             div(style = "font-size:16px;",
#                 "Font Size")),
#     tags$td(
#       width = "70%",
#       numericInput(
#         inputId = "canvas_var_font_size",
#         label = NULL,
#         value = 12,
#         min = 0,
#         max = 30
#       )
#     )
#   ),
#   tags$tr(
#     width = "100%",
#     tags$td(width = "30%",
#             div(style = "font-size:16px;",
#                 "Font Color")),
#     tags$td(
#       width = "70%",
#       colourInput(
#         inputId = "canvas_var_font_color",
#         label = NULL,
#         value = "#000000"
#       )
#     )
#   )
# )
