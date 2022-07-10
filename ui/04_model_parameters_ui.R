#This tab corresponds to the "Parameters" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_Parameters <- tabItem(tabName = "TAB_Parameters",
                          bsModal(
                            id = "modal_create_parameter",
                            title = NULL,
                            trigger = "parameter_add_parameter",
                            textInput("modal_param_param_name",
                                      "Parameter Name",
                                      value = ""),
                            textInput("modal_param_value",
                                      "Value",
                                      value = "0"),
                            textInput("modal_param_description",
                                      "Description",
                                      value = ""),
                            hr(),
                            fluidRow(
                              column(
                                width = 12,
                                align = "right",
                                actionButton("modal_create_param_button",
                                             "Confirm Create")
                              )
                            )
                          ),
                         tags$head(tags$style("#modal_create_parameter .modal-footer{ display:none}")),
                         bsModal(
                           id = "modal_delete_param",
                           title = NULL,
                           trigger = "parameter_delete_parameter",
                           "Deleting a parameter that is currently used in the model solver can create a fatal error...",
                           pickerInput(
                             "modal_params_to_delete",
                             "To delete",
                             choices = c()
                           ),
                           hr(),
                           fluidRow(
                             column(
                               width = 12,
                               align = "right",
                               actionButton("modal_delete_param_button",
                                            "Delete")
                             )
                           )
                         ),
                         tags$head(tags$style("#modal_delete_param .modal-footer{ display:none}")),
                         # fluidRow(
                         #   column(
                         #     width = 11,
                         #     box(
                         #       id = "parameter_info_box",
                         #       title = "Info",
                         #       collapsible = TRUE,
                         #       width = 12,
                         #       HTML("The table below hosts all parameters and values for your model. <br>"),
                         #       tags$b("To Use:"),
                         #       tags$div(
                         #         tags$ul(
                         #           tags$li("Double click cell in column you wish to edit. This will open the edit option for the column"),
                         #           tags$li("Change all desired values in that column."),
                         #           tags$li("Press <b>\"Ctrl+Enter\"<\b> to save changes or <b>\"Esc\"</b> to cancel")
                         #         )
                         #       ),
                         #       tags$b("Do not:"),
                         #       tags$div(
                         #         tags$ul(
                         #           tags$li("Edit more than one column at a time. Only changes to the first column save.  I.E do all values, \"Ctrl+Enter\", then do the Descriptions"),
                         #           tags$li("It is recommended that you do not change the parameter value at this time until I program a proper failsafe."),
                         #           tags$li("Or until Viren programs it.")
                         #         )
                         #       ),
                         #       "ToDo:",
                         #       tags$div(
                         #         tags$ul(
                         #           tags$li("Add option to which between table view and panel view?"),
                         #           tags$li("Add sort option to sort variables by parameter type: equations, IO, and other breaks")
                         #         )
                         #       )
                         #     )
                         #   ),
                         #   column(
                         #     width = 1,
                         #     align = "right",
                         #     actionBttn(
                         #       inputId = "parameter_info_button",
                         #       color = "primary",
                         #       icon = icon("info"),
                         #       size = "sm"
                         #     )
                         #   )
                         # ),
                         # br(),
                         br(),
                         br(),
                         fluidRow(
                           column(
                             width = 3,
                             pickerInput(
                               inputId = "parameters_filter_type",
                               label = "View By:",
                               choices = c("All",
                                           "Eqns",
                                           "Inputs",
                                           "Outputs")
                             )
                           ),
                           column(
                             width = 9,
                             align = "right",
                             div(
                               actionButton("parameter_add_parameter",
                                            "Create Parameter"),
                               actionButton("parameter_delete_parameter",
                                            "Delete Parameter")
                             )
                           )
                         ),
                         rHandsontableOutput("parameters_DT"),
                         

                  )