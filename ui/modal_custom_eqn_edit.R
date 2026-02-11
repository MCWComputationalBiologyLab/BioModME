shinyBS::bsModal(
  id = "modal_edit_custom_eqn",
  title = "Edit Custom Equation",
  trigger = "bttn_custom_eqn_edit",
  size = "large",
  tags$head(tags$style(HTML("
    #modal_edit_custom_eqn .modal-dialog {
      width: 90%;
      max-width: 1200px;
    }
  "))),
  fluidRow(
    column(
      width = 12,
      "Select the custom equation to edit, then modify the variable or expression below."
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 12,
      pickerInput(
        inputId = "PI_custom_eqn_edit_select",
        label = "Select Custom Equation to Edit",
        choices = "",
        options = pickerOptions(liveSearch = TRUE,
                                liveSearchStyle = "startsWith")
      )
    )
  ),
  fluidRow(
    column(
      width = 3,
      textInput(
        inputId = "TI_custom_eqn_LHS_edit", 
        label = "Variable", 
        value = ""
      )
    ),
    column(
      width = 9,
      tags$label("Expression", style = "font-weight: bold;"),
      tags$div(
        style = "margin-bottom: 10px;",
        textAreaInput(
          inputId = "TI_custom_eqn_RHS_edit",
          label = NULL,
          value = "",
          rows = 4,
          width = "100%",
          placeholder = "Enter expression here..."
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      tags$b("Existing Variables"),
      div(style = "max-height: 200px; overflow-y: auto;",
          rHandsontableOutput("RHT_custom_eqn_params_existing_edit")
      )
    ),
    column(
      width = 6,
      tags$b("New Variables"),
      div(style = "max-height: 200px; overflow-y: auto;",
          rHandsontableOutput("RHT_custom_eqn_params_new_edit")
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$details(
        tags$summary(tags$b("Preview Equation (Click to expand)")),
        style = "margin-top: 10px; margin-bottom: 10px;",
        uiOutput(
          outputId = "mathjax_custom_eqn_view_edit"
        )
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 12,
      align = "right",
      actionButton(
        inputId = "bttn_custom_eqn_update",
        label = "Update Equation",
        class = "btn-primary",
        style = "font-weight: bold;"
      )
    )
  )
)

