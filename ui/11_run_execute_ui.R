#This tab corresponds to the "Execute Model" under Run Model Tab
#  Justin Womack
#  January 26, 2021
#  Last Update: January 26, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_RUN_EXECUTE <- 
  tabItem(
    tabName = "TAB_RUN_EXECUTE",
      tabBox(
        id = "box3",
        title = NULL,
        width = 12,
        tabPanel(
          title = "Time",
          fluidRow(
            column(
              width = 10,
              fluidRow(
                textInput(
                  inputId = "execute_time_start",
                  label = "Starting Time",
                  value = "0"), 
                textInput(
                  inputId = "execute_time_end",
                  label = "End Time",
                  value = "10"), 
                textInput(
                  inputId = "execute_time_step",
                  label = "Time Step",
                  value = "0.1"),
                pickerInput(
                  inputId = "execute_time_unit",
                  label = "Unit",
                  choices = measurements::conv_unit_options$duration
                ),
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              offset = 9,
              align = "right",
              actionButton(
                inputId = "execute_run_model",
                label = "Run Solver",
                width = "145px")
            )
          )
        ),
        tabPanel(
          title = "Table",
          fluidRow(
            column(
              width = 6,
              tableLayoutDualColumns(
                labels = c("", ""),
                widgets = list(
                  div(
                    style = "margin-top: 35px;",
                    checkboxInput(
                      inputId = "execute_view_round_values",
                      label = "Round",
                      value = TRUE
                    )
                  ),
                  div(
                    style = "margin-top: 35px;",
                    checkboxInput(
                      inputId = "execute_view_scientific_notation",
                      label = "Scientific Notation",
                      value = FALSE
                    )
                  )
                ),
                widgets2 = list(
                  numericInput(
                    inputId = "execute_view_round_digits",
                    label = "Round to",
                    value = 3,
                    min = 1,
                    max = 50,
                    step = 1
                  ),
                  pickerInput(
                    inputId = "PI_execute_sci_not_options",
                    label = "Choose:",
                    choices = c("All Values" = "ALL",
                                "Values smaller than round" = "STR"),
                    selected = "STR"
                  )
                ),
                headerLabels = c("", ""),
                removeFirstCol = TRUE,      
                colPercents = c(40,60)
              )
            ),
            column(
              width = 6,
              tableLayoutDualColumns(
                labels = c("", ""),
                widgets = list(
                  div(
                    style = "margin-top: 35px;",
                    checkboxInput(
                      inputId = "CBI_execute_first_col_separate",
                      label = "Round Time Separate",
                      value = TRUE
                    )
                  ),
                  div(
                    style = "margin-top: 35px;",
                    checkboxInput(
                      inputId = "CBI_execute_first_col_use_time",
                      label = "Round with time step",
                      value = TRUE
                    )
                  )
                ),
                widgets2 = list(
                  numericInput(
                    inputId = "execute_view_round_digits_first_col",
                    label = "Round to",
                    value = 3,
                    min = 1,
                    max = 50,
                    step = 1
                  ),
                  NULL
                ),
                headerLabels = c("", ""),
                removeFirstCol = TRUE,      
                colPercents = c(40,60),
                colSpacing = "30px"
              ),
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 3,
              pickerInput(
                inputId = "execute_results_unit",
                label = "Results Units",
                choices = measurements::conv_unit_options$count
              )
            )
          )
        ),
        tabPanel(
          title = "Solver",
          fluidRow(
            column(
              width = 2,
              div(
                style = "padding-top: 33px",
                checkboxInput(
                  inputId = "execute_turnOn_time_scale_var",
                  label = "Scale Output",
                  value = FALSE)
              )
            ),
            column(
              width = 4,
              align = "left",
              conditionalPanel(
                condition = "input.execute_turnOn_time_scale_var",
                textInput(
                  inputId = "execute_time_scale_var",
                  label = "Time Scale Variable",
                  value = "1")
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 4,
              pickerInput(
                inputId = "execute_ode_solver_type",
                label = "Select ODE solver",
                choices = c("lsoda",
                            "lsode",
                            "lsodes",
                            "lsodar",
                            "vode",
                            "daspk",
                            "euler",
                            "rk4",
                            "ode23",
                            "ode45",
                            "radau")
              )
            )
          )
        )
      ),
    hr(),

    tableDownloadButtonsUI("module_execute_buttons"),
    DTOutput("execute_table_for_model"),
    inlineCSS("#execute_table_for_model .dataTables_length  
              {float: right !important;
              margin-top: -35px}
              ")
)