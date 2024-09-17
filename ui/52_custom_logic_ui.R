TAB_CUSTOM_LOGIC <- 
  tabItem(
    tabName = "TAB_CUSTOM_LOGIC",
    "This tab is experimental.  It is meant to inject custom if/else loops into
    the model.",
    br(),
    br(),
    box(
      width = 12,
      title = "Add Custom Code",
      fluidRow(
        column(
          width = 12,
          textAreaInput(
            inputId = "TI_customLogic_custom_code",
            label = "Code:",
            value = "",
            placeholder = "if(species < 0){ 
              species = 1
            }",
            height = "225px"
          )
        )
      ),
      fluidRow(
        column(
          width = 2,
          align = "right",
          offset = 10,
          actionButton(
            inputId = "button_customLogic_add_custom_code",
            label = "Add"
          )
        )
      )
    ),
    box(
      width = 12,
      title = "View/Remove Logic",
      fluidRow(
        column(
          width = 4,
          div(
            style = "float: left; width: 100%;", # Ensure the wrapping div takes full width
            div(
              style = "width: 100%;", # Set the width of the selectInput to 100%
              selectInput(
                inputId = "SI_customLogic_show_logic",
                label = "Select",
                choices = c(),  # Use '=' instead of '<-' for assigning the choices
                selectize = FALSE,
                size = 12
              )
            ),
            actionButton(
              inputId = "bttn_customLogic_delete_logic",
              label = "Delete",
              style = "float: right;"
            )
          )
        ),
        column(
          width = 8,
          verbatimTextOutput(outputId = "TO_customLogic_view_logic"),
          tags$style(
            type='text/css', 
            '#TO_customLogic_view_logic {white-space: pre-wrap;}'
          )
        )
      )
    )
  )