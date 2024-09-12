TAB_CUSTOM_LOGIC <- 
  tabItem(
    tabName = "TAB_CUSTOM_LOGIC",
    "This tab is experimental.  It is meant to inject custom if/else loops into
    the model.",
    fluidRow(
      column(
        width = 2,
        "If"
      ),
      column(
        width = 10,
        textInput(
          inputId = "TI_customLogic_if_statement",
          label = "Conditional Statement",
          value = "",
          placeholder = "species <= 0 || species_2 > 5"
        )
      )
    )
  )