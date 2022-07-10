#This tab corresponds to the "Export" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_export <- 
  tabItem(tabName = "TAB_export",
          bsModal(
            id = "latex_popup",
            title = NULL,
            trigger = "latex_make_document",
            #size = "small",
            fluidRow(
              column(
                width = 6,
                awesomeCheckboxGroup(
                  inputId = "latex_pages_to_add",
                  label = "Pages To Add",
                  choices = c("Variable",
                              "Equations",
                              "Additional Equations",
                              #"Input/Output",
                              "Parameter Table",
                              "Differential Eqns"),
                  selected = c("Variable",
                               "Equations",
                               "Additional Equations",
                               #"Input/Output",
                               "Parameter Table",
                               "Differential Eqns")
                )
              ),
              column(
                width = 6,
                awesomeCheckboxGroup(
                  inputId = "latex_additional_options",
                  label = "Additional Addons",
                  choices = c(#"Show Equation Types" = "show_eqn_type",
                              "Show Equation Descriptions" = "show_eqn_description")
                )
              )
            ),
            hr(),
            fluidRow(
              column(
                width = 12,
                align = "right",
                downloadButton(outputId = "export_latex_document"
                               ,label = "Get Document")
              )
            )
          ),
          tags$head(tags$style("#latex_popup .modal-footer{ display:none}"))
                   ,fluidRow(
                     column(
                       width = 4
                       ,box(
                         title = "Code"
                         ,solidHeader = FALSE
                         #,background="#000"
                         ,collapsible = FALSE
                         ,closable = FALSE
                         ,width = 12
                         ,fluidRow(
                           column(
                             width = 12,
                             textInput(inputId = "export_code_file_name"
                                      ,label = NULL
                                      ,value = ""
                                      ,placeholder = "File Name (no extension)")
                           )
                         )
                         ,downloadButton(outputId = "export_data_to_matlab_script"
                                       ,label = "MatLab Code" )
                         ,downloadButton(outputId = "export_data_to_R_script"
                                      ,label = "R Code")
                       )
                     )
                     ,column(
                       width = 4
                       ,box(
                         title = "Model File"
                         ,solidHeader = FALSE
                         #,background="#000"
                         ,collapsible = FALSE
                         ,closable = FALSE
                         ,width = 12
                         ,fluidRow(
                           column(
                             width = 12,
                             textInput(inputId = "export_model_file_name"
                                       ,label = NULL
                                       ,value = ""
                                       ,placeholder = "File Name (no extension)")
                           )
                         )
                         ,downloadButton(outputId = "export_save_data"
                                       ,label = "Save Model")
                       )
                     )
                     ,column(
                       width = 4
                       ,box(
                         title = "Latex Document"
                         ,solidHeader = FALSE
                         #,background="#000"
                         ,collapsible = FALSE
                         ,closable = FALSE
                         ,width = 12
                         ,fluidRow(
                           column(
                             width = 12,
                             textInput(inputId = "export_eqns_file_name"
                                       ,label = NULL
                                       ,value = ""
                                       ,placeholder = "File Name (no extension)")
                           )
                         )
                         ,actionButton(
                           inputId = "latex_make_document",
                           label = "Latex Download",
                           icon = icon("download")
                         )
                         # ,downloadButton(outputId = "export_latex_document"
                         #              ,label = "Latex Document")
                         # ,dropdownMenu = boxDropdown(
                         #   "Pages to Add"
                         #   # awesomeCheckboxGroup(
                         #   #   inputId = "latex_pages_to_add",
                         #   #   label = "Pages To Add",
                         #   #   choices = c("Variable",
                         #   #               "Equations",
                         #   #               "Additional Equations",
                         #   #               "Input/Output",
                         #   #               "Parameter Table",
                         #   #               "Differential Eqns"),
                         #   #   selected = "all"
                         #   # )
                         #   ,checkboxInput("latex_add_variables"
                         #                  ,"Variables"
                         #                  ,TRUE)
                         #   ,checkboxInput("latex_add_equations"
                         #                  ,"Equations"
                         #                  ,TRUE)
                         #   ,checkboxInput("latex_add_additionalEqns"
                         #                  ,"Additional Equations"
                         #                  ,TRUE)
                         #   ,checkboxInput("latex_add_IO"
                         #                  ,"Input/Output"
                         #                  ,TRUE)
                         #   ,checkboxInput("latex_add_paramTable"
                         #                  ,"Parameter Table"
                         #                  ,TRUE)
                         #   ,checkboxInput("latex_add_diffEqns"
                         #                  ,"Differential Eqns"
                         #                  ,TRUE)
                         # )
                         # ,sidebar = boxSidebar(
                         #   id = "latexSideBar"
                         #   #,width = 25
                         #   # ,checkboxInput(inputId = "latex_equation_headers"
                         #   #                ,label = "Equation Types Shown"
                         #   #                ,value = FALSE)
                         #   # ,checkboxInput(inputId = "latex_equation_description"
                         #   #                ,label = "Equation Descriptions Shown"
                         #   #                ,value = TRUE)
                         #   # ,checkboxInput(inputId = "latex_option_test"
                         #   #                ,label = "Advanced Options"
                         #   #                ,value = FALSE)
                         # )
                       )
                     )
                    )
                   ,hr()
                   ,tabBox(width = 12
                          ,tabPanel("Parameters"
                                    ,DTOutput("table_parameters_export"))
                          ,tabPanel("Equations"
                                    ,DTOutput("table_equations_export"))
                          ,tabPanel("Initial Conditions"
                                    ,DTOutput("table_ICs_export"))
                            ) #end tabbox
                   )#end tabitem