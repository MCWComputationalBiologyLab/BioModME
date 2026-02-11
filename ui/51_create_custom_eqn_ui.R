TAB_CREATE_CUSTOM_EQN <-
  tabItem(
    tabName = "TAB_CREATE_CUSTOM_EQN",
    fluidRow(
      column(
        width = 12,
        box(
          width = 12,
          fluidRow(
            column(
              width = 3,
              textInput(
                inputId = "TI_custom_eqn_LHS", 
                label = "Variable", 
                value = ""
              )
            ),
            column(
              width = 9,
              textInput(
                inputId = "TI_custom_eqn_RHS",
                label = "Expression",
                value = ""
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              "Existing Variables",
              rHandsontableOutput("RHT_custom_eqn_params_existing")
            ),
            column(
              width = 6,
              "New Variables",
              rHandsontableOutput("RHT_custom_eqn_params_new"),
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              uiOutput(
                outputId = "mathjax_custom_eqn_view"
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 12,
              actionButton(
                inputId = "bttn_custom_eqn_enter",
                label = "Add Equation"
              )
            )
          )
        ),
        hr(),
        rHandsontableOutput("RHT_custom_eqn_display_existing")
      )
    )
  )