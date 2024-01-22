TAB_DEBUG <-   
  tabItem(
    tabName = "TAB_DEBUG",
    "This page is meant for developers to be able to quickly/easily see different
    variables and data structures of the application.  Allowing insight, and, 
    hopefully, ease of finding and narrowing down software bugs.",
    fluidRow(
      column(
        width = 3,
        h4(tags$b(tags$u("View"))),
        verticalLayout(
          radioGroupButtons(inputId = "debug_text_or_table",
                            label = "View by:",
                            choices = c("List View", "Table")),
          actionButton(inputId = "debug_view_variables",
                       label = "Species",
                       width = "50%"),
          actionButton(inputId = "debug_view_compartments",
                       label = "Compartments",
                       width = "50%"),
          actionButton(inputId = "debug_view_equations",
                       label = "Equations",
                       width = "50%"),
          actionButton(inputId = "debug_view_ids",
                       label = "ID Database",
                       width = "50%"),
          actionButton(inputId = "debug_view_IO",
                       label = "I/O",
                       width = "50%"),
          actionButton(inputId = "debug_view_parameters",
                       label = "Parameters",
                       width = "50%"),
          actionButton(inputId = "debug_view_differential_eqns",
                       label = "Diff Eqns",
                       width = "50%"),
          actionButton(inputId = "debug_view_custom_laws",
                       label = "Custom Laws",
                       width = "50%"),
          actionButton(inputId = "debug_view_custom_eqns",
                       label = "Custom Eqns",
                       width = "50%")
        )
      ),
      column(
        width = 9,
        pickerInput(
          inputId = "debug_filter_searchType",
          label = "Filter",
          choices = c()
        ),
        conditionalPanel(
          condition = "input.debug_text_or_table == 'List View'",
          verbatimTextOutput("debug_text_view")
        ),
        conditionalPanel(
          condition = "input.debug_text_or_table == 'Table'",
          rHandsontableOutput("debug_table_view")
        )
        
      )
    ),
    hr(),
    tags$h3("Reset all variables"),
    actionButton(
      inputId = "bttn_debug_reset_vars",
      label = "Reset Variables"
    ),
    actionButton(
      inputId = "bttn_debug_display_units",
      label = "Print Units"
    ),
    hr(), hr(),
    tags$h3("Reset Latex/Mathjax laws"),
    actionButton(
      inputId = "bttn_debug_reset_diff_mathjax",
      label = "Resolve display laws"
    ),
    hr(), hr(), hr(),
    fluidRow(
      column(
        width = 3,
        verticalLayout(
          pickerInput(
            inputId = "css_selector",
            label = "Select Skin",
            choices = c("Default",
                        "Night",
                        "RoyalBlue"),
            select = "Default"
          )
        )
      )
    )
)