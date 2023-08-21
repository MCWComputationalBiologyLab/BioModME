#This tab corresponds to the "Data Management" Tab of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_VAR_CREATE <- 
  tabItem(
    tabName = "TAB_VAR_CREATE",
    # Source Modals ------------------------------------------------------------
    # Compartment Modals
    source(file.path(".", "ui", "modal_compartment_add.R"), local = TRUE)$value,
    
    # Species Modals
    source(file.path(".", "ui", "modal_species_add.R"), local = TRUE)$value,
    source(file.path(".", "ui", "modal_species_delete.R"), local = TRUE)$value,
    
    # Reactions Modals
    source(file.path(".", "ui", "modal_reaction_add.R"), local = TRUE)$value,
    source(file.path(".", "ui", "modal_reaction_edit.R"), local = TRUE)$value,
    source(file.path(".", "ui", "modal_reaction_delete.R"), local = TRUE)$value,
    
    # Input/Ouput Modals
    source(file.path(".", "ui", "modal_IO_add.R"), local = TRUE)$value, 
    source(file.path(".", "ui", "modal_IO_delete.R"), local = TRUE)$value,

    jqui_sortable(
    div(
      # Info Box -----------------------------------------------------------------
      fluidRow(
        column(
          width = 12,
          box(
            id = "create_var_info_box",
            title = "Model Information",
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            textInput(
              inputId = "TI_model_name",
              label = "Model Name",
              value = "New Model"
            ),
            textAreaInput(
              inputId = "TAI_model_description",
              label = "Model Description",
              value = "No description given."
            )
            # h3("Naming Conventions"),
            # tags$div(
            #   tags$ul(
            #     tags$li("Do not start variable name with a number or 
            #             special character"),
            #     tags$li("Special Characters allowed are: \"_\" and \".\" 
            #             (eg my_var, my.var, my.weird_var)"),
            #     tags$li("Variables are case sensitive (ie Var1 is different 
            #             from var1)")
            #   )
            # )
          )
        )
      ),
      # Compartment Box --------------------------------------------------------
      fluidRow(
        column(
          width = 12,
          box(
            id = "createVar_compartment_box",
            width = 12,
            title = "Compartments",
            collapsible = TRUE,
            div(
              rHandsontableOutput("createVar_compartment_table"),
              fluidRow(
                column(
                  offset = 9,
                  width = 3,
                  align = "right",
                  actionBttn(
                    inputId = "createVar_add_compartment_button",
                    label = NULL,
                    style = "material-circle",
                    # color = "royal",
                    icon = icon("plus"),
                    size = "xs"
                  ),
                  actionBttn(
                    inputId = "createVar_remove_compartment_button",
                    label = NULL,
                    style = "material-circle",
                    # color = "success",
                    icon = icon("minus"),
                    size = "xs"
                  ) 
                )
              )
            )
          )
        )
      ),
      # Variable Box -------------------------------------------------------------
      fluidRow(
        column(
          width = 12,
          box(
            id = "createVar_variable_box",
            width = 12,
            title = "Species",
            collapsible = TRUE,
            # uiOutput("createVar_species_compartment_options"),
            shinyjs::hidden(
              div(id = "species_hide_in_single_compartment",
                  div(style = "background-color:#F9F9F9;
                       border: 1px solid #c5c5c5;
                       border-radius: 12px;
                       padding: 10px 10px 10px 10px;",
                      fluidRow(
                        column(
                          width = 3,
                          pickerInput(
                            inputId = "createVar_active_compartment",
                            label = "Active Compartment",
                            choices = c()
                          )
                        ),
                        column(
                          width = 3,
                          prettyCheckbox(
                            inputId = "createVar_show_active_compartment_only",
                            label = "Show Active Compartment Only",
                            value = TRUE
                          ),
                          prettyCheckbox(
                            inputId = "createVar_add_to_all_compartments",
                            label = "Add To All Compartments",
                            value = FALSE
                          )
                        )
                      ) 
                  ),
                  hr()
              )
            ),
            fluidRow(
              column(
                width = 12,
                actionButton(
                  inputId = "test_popup_table",
                  "Popout"
                ),
                div(
                  rHandsontableOutput("myVariables_DT"),
                  fluidRow(
                    column(
                      offset = 9,
                      width = 3,
                      align = "right",
                      hidden(
                        actionBttn(
                          inputId = "createVar_add_variable_to_all_button",
                          label = NULL,
                          style = "material-circle",
                          color = "primary",
                          icon = icon("plus"),
                          size = "xs"
                        )
                      ),
                      actionBttn(
                        inputId = "createVar_add_variable_button",
                        label = NULL,
                        style = "material-circle",
                        # color = "primary",
                        icon = icon("plus"),
                        size = "xs"
                      ),
                      actionBttn(
                        inputId = "species_del_open_modal",
                        label = NULL,
                        style = "material-circle",
                        # color = "danger",
                        icon = icon("minus"),
                        size = "xs"
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      # Reaction Box -----------------------------------------------------------
      fluidRow(
        column(
          width = 12,
          box(
            id = "createVar_equation_box",
            width = 12,
            title = "Reactions",
            collapsible = TRUE,
            fluidRow(
              column(
                width = 12,
                div(
                  rHandsontableOutput(outputId = "main_eqns_table"),
                  ## Eqn Display -----------------------------------------------
                  fluidRow(
                    column(
                      width = 12,
                      align = "right",
                      actionBttn(
                        inputId = "eqns_add_open_modal",
                        label = NULL,
                        style = "material-circle",
                        icon = icon("plus"),
                        size = "xs"
                      ),
                      actionBttn(
                        inputId = "eqns_delete_open_modal",
                        label = NULL,
                        style = "material-circle",
                        icon = icon("minus"),
                        size = "xs"
                      )
                    )
                  )
                )
              ) 
            ),
            tags$head(
              tags$style("#eqnbuilder_sidebar {min-height:480px")),
            tags$head(
              tags$style("#tabbox_equation_builder_box {min-height:480px")),
            tags$head(
              tags$style("#eqnCreate_addEqnToVector {margin-top: 15px")),
            tags$head(
              tags$style("#eqnCreate_showEquationBuilding {margin-top: 15px"))
          )
        )
      ),
      # Input/Output Box ---------------------------------------------------------
      fluidRow(
        column(
          width = 12,
          box(
            id = "createVar_input_output_box",
            width = 12,
            title = "Input/Output",
            collapsible = TRUE,
            collapsed = TRUE,
            fluidRow(
              column(
                width = 12, 
                rHandsontableOutput("createModel_IO_logs_table")
              )
            ),
            fluidRow(
              column(
                width = 12,
                align = "right",
                div(
                  actionBttn(
                    inputId = "io_add_open_modal",
                    label = NULL,
                    style = "material-circle",
                    icon = icon("plus"),
                    size = "xs"
                  ),
                  actionBttn(
                    inputId = "io_delete_open_modal",
                    label = NULL,
                    style = "material-circle",
                    icon = icon("minus"),
                    size = "xs"
                  )
                )
              )
            )
          )
        )
      ),
      # Parameter Box ------------------------------------------------------------
      fluidRow(
        column(
          width = 12,
          box(
            id = "createVar_parameter_box",
            width = 12,
            title = "Parameters",
            collapsible = TRUE,
            collapsed = TRUE,
            div(style = "background-color:#F9F9F9;
                       border: 1px solid #c5c5c5;
                       border-radius: 12px;
                       padding: 10px 10px 10px 10px;",
                fluidRow(
                  column(
                    width = 3,
                    pickerInput(
                      inputId = "parameters_filter_type",
                      label = "View By:",
                      choices = c("All",
                                  "Equation Parameters" = "Reaction",
                                  "Input/Output Parameters" = "Input/Output",
                                  "Compartment Volumes" = "Compartment")
                    )
                  )
                )
            ),
            hr(),
            fluidRow(
              column(
                width = 12,
                div(
                  rHandsontableOutput("parameters_DT")
                )
              )
            )
          )
        )
      ),
      # Differential Equations ---------------------------------------------------
      fluidRow(
        column(
          width = 12,
          box(
            id = "createVar_differential_equations_box",
            width = 12,
            title = "Differential Equations",
            collapsible = TRUE,
            collapsed = TRUE,
            conditionalPanel(
              condition = "!input.diffeq_render_as_mathjax",
              htmlOutput(outputId = "diffeq_display_diffEqs")
            ),
            conditionalPanel(
              condition = "input.diffeq_render_as_mathjax",
              uiOutput("diffeq_display_diffEqs_MathJax")
            ),
            hr(),
            fluidRow(
              column(
                width = 12,
                align = "right",
                actionButton(
                  inputId = "diffeq_generate_equations",
                  label = "Generate")
              )
            ),
            conditionalPanel(
              condition = "input.diffeq_custom_option",
              hr(),
              fluidRow(
                column(
                  width = 3,
                  pickerInput(
                    inputId = "diffeq_var_to_custom",
                    label = "Species For Customization",
                    choices = c()
                  )
                ),
                column(
                  width = 7,
                  textInput(
                    inputId = "diffeq_custom_eqn",
                    label = "Custom Equation",
                    value = "",
                    placeholder = "k_f1*A-k_r1*B"
                  )
                ),
                column(
                  width = 2,
                  div(style = "padding-top:28px",
                      actionButton(
                        inputId = "diffeq_custom_eqn_button",
                        label = "Customize")
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  "Note that any custom equation will disable that equation 
                    to be solved for by the computer. You can choose to allow 
                    equation to be generated by adding it below"
                )
              ),
              fluidRow(
                column(
                  width = 3,
                  pickerInput(
                    inputId = "diffeq_multi_custom_eqns",
                    label = "Custom Equations to Ignore",
                    choices = c(),
                    multiple = TRUE
                  )
                )
              )
            ),
            sidebar = boxSidebar(
              id = "diffeq_sidebar",
              icon = icon("bars", lib = "font-awesome"),
              width = 25,
              checkboxInput(
                inputId = "diffeq_render_as_mathjax",
                label = "As Mathjax",
                value = TRUE
              ),
              conditionalPanel(
                condition = "input.diffeq_render_as_mathjax",
                checkboxInput(
                  inputId = "diffeq_newline_diffeq",
                  label = "Newline Each Term",
                  value = FALSE
                )
              ),
              conditionalPanel(
                condition = "!input.diffeq_render_as_mathjax",
                checkboxInput(
                  inputId = "diffeq_option_simplify",
                  label = "Simplify Equations",
                  value = FALSE)
              ), 
              checkboxInput(
                inputId = "diffeq_custom_option",
                label = "Create Custom Equation",
                value = FALSE
              ),
              checkboxInput(
                inputId = "CBI_diffeq_clean_parenthesis",
                label = "Clean Parenthesis",
                value = TRUE
              ),
              checkboxInput(
                inputId = "CBI_diffeq_show_unit_types",
                label = "Show Unit Correctness",
                value = FALSE
              ),
              checkboxInput(
                inputId = "CBI_diffeq_pretty_equations",
                label = "Bracket Species",
                value = FALSE
              )
            )
          )
        )
      )
    ) # end div
    ),#end sortable
    
    tags$head(tags$style('#html_table_vars .box-header{ display: none}')),  
    tags$head(tags$style('#box1 .box-header{ display: none}')),
    tags$head(
      tags$style(".PE_variable_UI_table label {display: table-cell; 
                             text-align: center;
                             vertical-align: top; } 

.PE_variable_UI_table .form-group {display: table-cell;}")
    )
  )#end tabItem
