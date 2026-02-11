TAB_CREATE_CUSTOM_EQN <-
  tabItem(
    tabName = "TAB_CREATE_CUSTOM_EQN",
    # Source Modals
    source(file.path(".", "ui", "modal_custom_eqn_edit.R"), local = TRUE)$value,
    source(file.path(".", "ui", "modal_custom_eqn_delete.R"), local = TRUE)$value,
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
        fluidRow(
          column(
            width = 12,
            box(
              width = 12,
              title = "Custom Equations",
              fluidRow(
                column(
                  width = 12,
                  rHandsontableOutput("RHT_custom_eqn_display_existing")
                )
              ),
              hr(),
              fluidRow(
                column(
                  width = 6,
                  actionButton(
                    inputId = "bttn_custom_eqn_edit",
                    label = "Edit Equation",
                    icon = icon("edit")
                  )
                ),
                column(
                  width = 6,
                  align = "right",
                  actionButton(
                    inputId = "bttn_custom_eqn_delete",
                    label = "Delete Equation",
                    icon = icon("trash")
                  )
                )
              )
            )
          )
        )
      )
    )
  )