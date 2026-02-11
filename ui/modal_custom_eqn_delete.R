shinyBS::bsModal(
  id = "modal_delete_custom_eqn",
  title = "Delete Custom Equation",
  trigger = "bttn_custom_eqn_delete",
  size = "large",
  tags$head(tags$style(HTML("
    #modal_delete_custom_eqn .modal-dialog {
      width: 90%;
      max-width: 1200px;
    }
  "))),
  fluidRow(
    column(
      width = 12,
      pickerInput(
        inputId = "PI_custom_eqn_delete_select",
        label = "Select Custom Equation(s) to Delete",
        choices = "",
        multiple = TRUE,
        options = pickerOptions(liveSearch = TRUE,
                                liveSearchStyle = "startsWith")
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      tags$b("Preview of equations to delete:"),
      div(style = "max-height: 300px; overflow-y: auto; margin-top: 10px;",
          rHandsontableOutput("RHT_custom_eqn_delete_preview")
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      checkboxInput(
        inputId = "checkbox_custom_eqn_delete_keep_modal_active",
        label = "Close on Delete",
        value = TRUE
      )
    ),
    column(
      width = 6,
      align = "right",
      actionButton(
        inputId = "bttn_custom_eqn_delete_confirm",
        label = "Delete",
        class = "btn-danger",
        style = "font-weight: bold;"
      )
    )
  )
)

