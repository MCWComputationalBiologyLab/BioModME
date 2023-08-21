shinyBS::bsModal(
  id = "modal_delete_io",
  title = NULL,
  trigger = "io_delete_open_modal",
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
              inputId = "PI_delete_select_io",
              label = "Select Input/Output Number to Delete",
              choices = "",
              multiple = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            rHandsontableOutput("deleteIO_table_viewer")
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
        inputId = "checkbox_modal_delete_io_keep_modal_active",
        label = "Close on Delete",
        value = TRUE
      )
    ),
    column(
      width = 6,
      align = "right",
      div(
        actionButton("modal_delete_io_button",
                     "Delete")
      )
    )
  )
)