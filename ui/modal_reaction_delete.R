shinyBS::bsModal(
  id = "modal_delete_equations",
  title = NULL,
  trigger = "eqns_delete_open_modal",
  size = "large",
  fluidRow(
    column(
      width = 12,
      box(
        width = 12,
        solidHeader = FALSE,
        collapsible = FALSE,
        fluidRow(
          column(
            width = 6,
            pickerInput(
              inputId = "eqnCreate_delete_select_equation",
              label = "Select Equation Number to Delete",
              choices = "",
              multiple = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            rHandsontableOutput("deleteEquations_table_viewer")
          )
        )
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      checkboxInput(
        inputId = "checkbox_modal_delete_keep_modal_active",
        label = "Close on Delete",
        value = TRUE
      )
    ),
    column(
      width = 6,
      align = "right",
      div(
        actionButton("modal_delete_eqn_button",
                     "Delete")
      )
    )
  )
)