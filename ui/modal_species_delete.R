shinyBS::bsModal(
  id = "modal_delete_species",
  title = NULL,
  trigger = "species_del_open_modal",
  size = "large",
  fluidRow(
    column(
      width = 4, 
      pickerInput(
        inputId = "PI_modal_delete_species",
        label = "Select Species To Remove",
        choices = c()
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      checkboxInput(
        inputId = "checkbox_modal_keep_delete_var_open",
        label = "Keep Open",
        value = FALSE
      )
    ),
    column(
      width = 6,
      align = "right",
      div(
        actionButton("button_modal_delete_species",
                     "Delete")
      )
    )
  )
)