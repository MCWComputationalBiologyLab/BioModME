TAB_PARAMETER_ESTIMATION <-
  tabItem(tabName = "TAB_PARAMETER_ESTIMATION",
  # First Box - Import Data
          box(
            title = "Import Observational Data",
            width = 12,
            fluidRow(
              column(
                width = 4,
                fileInput("pe_obs_data",
                          "Import Observed Data")
              ),
              column(
                width = 4,
                offset = 1,
                div(style = "display:inline-block; padding-top:35px;",
                    checkboxInput(
                      inputId = "pe_show_imported_datatable",
                      label = "View Table",
                      value = FALSE
                    )
                  )
              )
            ),
            conditionalPanel(
              condition = "input.pe_show_imported_datatable",
              rHandsontableOutput(
                outputId = "pe_import_data_table"
                
              )
            )
          ),
  # Box 2: Select Parameters to Estimate
          # fluidRow(
          #   column(
          #     width = 12,
              box(
                title = "Select Parameters to Estimate",
                width = 12,
                dropdownMenu = boxDropdown(
                  tags$h4("Options"),
                  textInput(
                    inputId = "pe_tol_option",
                    label = "Tolerence",
                    value = "10e-6"
                  )
                ),
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      "pe_select_par",
                      "Parameters to Estimate:",
                      choices = c(),
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 8,
                    offset = 1,
                    rHandsontableOutput(
                      outputId = "pe_parameter_value_table"
                    )
                  )
                 ),
                fluidRow(
                  column(
                    width = 1,
                    offset = 11,
                    actionBttn(
                      inputId = "pe_run_parameter_estimation",
                      label = "Run"
                    )
                  )
                )
            #   )
            # )
          ),
  # Box 3: View Results of Parameter Estimation
          fluidRow(
            column(
              width = 12,
              box(
                title = "Estimation Results",
                width = 12,
                plotOutput(
                  outputId = "pe_parameter_estimation_plot"
                ),
                verbatimTextOutput(
                  outputId = "pe_logs",
                  placeholder = TRUE
                ),
                # fluidRow(
                #   column(
                #     width = 4,
                #     verbatimTextOutput(
                #       outputId = "pe_logs",
                #       placeholder = TRUE
                #     )
                #   ),
                #   column(
                #     width = 8,
                #     plotOutput(
                #       outputId = "pe_parameter_estimation_plot"
                #     )
                #   )
                # ),
                fluidRow(
                  column(
                    width = 1,
                    offset = 11,
                    actionBttn(
                      inputId = "pe_store_estimated_parameters",
                      label = "Store"
                    )
                  )
                )
              )
            )
          )
            # tags$head(
            #   tags$style(HTML("
            #                   #pe_logs {
            #                   background-color: white;
            #                   }"
            #                   ))
            # )
        )
  











