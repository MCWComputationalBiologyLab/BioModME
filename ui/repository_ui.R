TAB_MODEL_REPOSITORY <-
  tabItem(
    tabName = "TAB_MODEL_REPOSITORY",
    fluidRow(
      column(
      width = 4,
      div(
        style = "float: left",
        selectInput(
          inputId = "SI_repos_base_choices",
          label = "Select Model To Load:",
          choices <- c(),
          selectize = FALSE,
          size = 12
        ),
        actionButton(
          inputId = "bttn_load_model_from_base_repo",
          label = "Load Model",
          style = "float: right;"
        )
      )
      ), 
      column(
        width = 8,
        verticalLayout(
          verbatimTextOutput(outputId = "TO_repos_model_name"),
          verbatimTextOutput(outputId = "TO_repos_model_description"),
          tags$style(
            type='text/css', 
            '#TO_repos_model_description {white-space: pre-wrap;}'
          )
        )
      )
    )
  )