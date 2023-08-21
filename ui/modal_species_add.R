bsModal(
  id = "modal_create_variable",
  title = NULL,
  trigger = "createVar_add_variable_to_all_button",
  "Enter the name of the variable to add to all compartments and choose the 
      subsetting term.",
  textInput(
    inputId = "modal_variable_name",
    label = "Name",
    value = ""
  ),
  br(),
  br(),
  br(),
  radioButtons(
    inputId = "modal_variable_name_subset",
    label = "How to subset variable",
    choices = c("Compartment Name" = "COMPNAME",
                "Numerical" = "COMPNUMBER"),
    selected = "COMPNUMBER",
    inline = TRUE
  ),
  hr(),
  fluidRow(
    column(
      width = 12,
      align = "right",
      div(
        actionButton("modal_createVariable_add_button",
                     "Add"),
        actionButton("modal_createVariable_cancel_button",
                     "Cancel")
      )
    )
  )
)