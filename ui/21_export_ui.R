#This tab corresponds to the "Export" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_EXPORT <- 
  tabItem(
    tabName = "TAB_EXPORT",
    bsModal(
      id = "latex_popup",
      title = NULL,
      trigger = "latex_make_document",
      #size = "small",
      fluidRow(
        column(
          width = 6,
          awesomeCheckboxGroup(
            inputId = "latex_pages_to_add",
            label = "Pages To Add",
            choices = c(
              "Variable",
              "Equations",
              "Additional Equations",
              "Input/Output",
              "Parameter Table",
              "Differential Eqns"
            ),
            selected = c(
              "Variable",
              "Equations",
              "Additional Equations",
              "Input/Output",
              "Parameter Table",
              "Differential Eqns"
            )
          )
        ),
        column(
          width = 6,
          awesomeCheckboxGroup(
            inputId = "latex_additional_options",
            label = "Additional Addons",
            choices = c(#"Show Equation Types" = "show_eqn_type",
              "Show Equation Descriptions" =
                "show_eqn_description")
          )
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 12,
          align = "right",
          downloadButton(
            outputId = "export_latex_document",
            label = "Get Document")
        )
      )
    ), 
    tags$head(tags$style("#latex_popup .modal-footer{ display:none}")),
    fluidRow(
      column(
        width = 4,
        box(
          title = "Code",
          solidHeader = FALSE,
          collapsible = FALSE,
          closable = FALSE,
          width = 12,
          fluidRow(
            column(
              width = 12,
              textInput(
                inputId = "export_code_file_name",
                label = NULL,
                value = "",
                placeholder = "File Name (no extension)"
              )
            )
          ),
        downloadButton(
          outputId = "export_data_to_matlab_script",
          label = "MatLab"
        ),
        downloadButton(
          outputId = "export_data_to_R_script",
          label = "R"
        ),
        downloadButton(
          outputId = "export_data_to_julia_script",
          label = "Julia"
        )
      )
    ),
    column(
      width = 4,
      box(
        title = "Model File",
        solidHeader = FALSE,
        collapsible = FALSE,
        closable = FALSE,
        width = 12,
        fluidRow(
          column(
            width = 12,
            textInput(
              inputId = "export_model_file_name",
              label = NULL,
              value = "",
              placeholder = "File Name (no extension)"
            )
          )
        ),
        downloadButton(
          outputId = "export_save_data", 
          label = "Save Model"),
        downloadButton(
          outputId = "export_save_as_sbml",
          label = "SBML"
        )
      )
    ),
    column(
      width = 4,
      box(
        title = "Latex Document",
        solidHeader = FALSE,
        collapsible = FALSE,
        closable = FALSE,
        width = 12,
        fluidRow(
          column(
            width = 12,
            textInput(
              inputId = "export_eqns_file_name",
              label = NULL,
              value = "",
              placeholder = "File Name (no extension)"
            )
          )
        ), 
        actionButton(
          inputId = "latex_make_document",
          label = "Latex Download",
          icon = icon("download")
        )
      )
    )
  ),
  hr(),
  tabBox(
    width = 12,
    tabPanel("Species",
             DTOutput("table_species_export")
    ),
    tabPanel("Compartments",
             DTOutput("table_compartments_export")
    ),
    tabPanel("Parameters",
             DTOutput("table_parameters_export")
    ),
    tabPanel("Reactions",
             DTOutput("table_reactions_export")
    ),
    tabPanel("Differential Equations",
             downloadButton(
               outputId = "Dbttn_export_diffeqn_mathml",
               label = "MathML"
             ),
             DTOutput("table_differential_equation_export")
             )
  ) #end tabbox
)#end tabitem