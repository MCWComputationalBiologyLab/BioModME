TAB_CUSTOM_LOGIC <- 
  tabItem(
    tabName = "TAB_CUSTOM_LOGIC",
    "This tab is experimental.  It is meant to inject custom if/else loops into
    the model.",
    br(),
    br(),
    box(
      width = 12,
      # fluidRow(
      #   column(
      #     width = 3,
      #     textInput(
      #       inputId = "TI_customLogic_if_condition",
      #       label = "Conditional Statement",
      #       value = "",
      #       placeholder = "species <= 0 || species_2 > 5"
      #     )
      #   ),
      #   column(
      #     width = 9, 
      #     textAreaInput(
      #       inputId = "TI_customLogic_if_statement",
      #       label = "Statement Trigger",
      #       value = "species = 0"
      #     )
      #   )
      # )
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
    )
  )