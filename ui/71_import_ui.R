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
            "Our SBML importer was created from custom scripts. There are many 
            complexities and variations in SBML files. As such, we note that 
            our importer will fail on some files. We do our best to return 
            where these importers failed, looking to better improve it in the future.",
            br(),
            br(),
            fileInput(
              "file_input_load_sbml",
              "SBML",
              placeholder = " .xml",
              multiple = FALSE,
              accept = c(".xml")
            ),
            HTML("<h5>Current Known Limitations:</h3>
         <ul>
           <li>MathML parser cannot read expressions with root and degree (other keywords may be an issue)</li>
           <li>Models are rendered as unitless. We do not currently parse UnitDefinitions in SBML markup.</li>
           <li>While we can read function definitions and use those to build equations, we have not currently incorporated a converter to add the functions to the UI. As such, all functions will load as type 'CUSTOM'.</li>
         </ul>")
          ),
          
        )
      )
    )
  )