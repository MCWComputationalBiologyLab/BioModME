TAB_IMPORT <- 
  tabItem(
    tabName = "TAB_IMPORT",
    "We currently support loading file types in an r data structure and SBML. 
    While SBML is a common storage language for models, we store model information
    in .rds format as that allows us to store application specific content.",
    div(
      id = "import_for_waiter",
      fluidRow(
        column(
          width = 12,
          titlePanel(
            title = "Import RDS File",
            fileInput(
              "file_input_load_rds",
              ".rds",
              placeholder = "Choose .rds File",
              multiple = FALSE,
              accept = c(".rds")
            )
          )
        )
      ),
      br(),
      br(),
      fluidRow(
        column(
          width = 12,
          titlePanel(
            title = "Import SBML File",
            panel_id = "test_id",
            fileInput(
              "file_input_load_sbml",
              "SBML",
              placeholder = " .xml",
              multiple = FALSE,
              accept = c(".xml")
            )
          )
        )
      )
    )
  )