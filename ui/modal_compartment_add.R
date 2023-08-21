bsModal(
  id = "modal_create_compartment",
  title = "Create Compartment",
  trigger = "createVar_add_compartment",
  textInput(
    inputId = "modal_createCompartment_compartment_name",
    label = "Compartment Name",
    value = ""
  ),
  textInput(
    inputId = "modal_createCompartment_volume_variable",
    label = "Volume Variable",
    value = ""
  ),
  textInput(
    inputId = "modal_createCompartment_volume_value",
    label = "Volume Value",
    value = ""
  ),
  pickerInput(
    inputId = "modal_createCompartment_volume_unit",
    label = "Volume Unit",
    choices = c()
  ),
  textAreaInput(
    inputId = "modal_createCompartment_description",
    label = "Description",
    value = "",
    width = NULL,
    height = "200px"
  ),
  hr(),
  fluidRow(
    column(
      width = 12,
      align = "right",
      actionButton("modal_createCompartment_add_button",
                   "Delete")
    )
  )
)