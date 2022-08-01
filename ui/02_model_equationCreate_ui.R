#This tab corresponds to the "Equation Creation" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 5, 2021
#  Last Update: January 5, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_Equation_Create <- 
  tabItem(
    tabName = "TAB_Equation_Create",
    # box(
    #   id = "create_eqn_info_box",
    #   title = "Info",
    #   collapsible = TRUE,
    #   width = 12,
    #   h3("How To Use")
    #   
    # ),
    fluidRow(
      column(
        width = 3,
  #-----------------------------------------------------------------------------
  
  # Equation Builder Main Sidebar
  
  #-----------------------------------------------------------------------------
        box(
          id = "eqnbuilder_sidebar",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          maximizable = TRUE,
          radioGroupButtons(
            inputId = "eqn_action",
            label = NULL,
            choices = c("New", 
                        "Edit", 
                        "Delete"),
            justified = TRUE,
            width = "100%"
              ),
          hr(),
          conditionalPanel(
            condition = "input.eqn_action == 'New'",
            pickerInput(
              inputId = "eqnCreate_type_of_equation",
              label = "Equation Type",
              choices = c("Chemical Reaction" = "chem_rxn",
                           "Enzyme Based Reaction" = "enzyme_rxn",
                           "Synthesis" = "syn",
                           "Degradation" = "deg",
                           "Custom Equation" = "rate_eqn",
                           "Time Dependent Equation" = "time_dependent"
                           )
            ),
            conditionalPanel(
              condition = "input.eqnCreate_type_of_equation == 'chem_rxn'",
              pickerInput(
                inputId = "eqn_chem_law",
                label = "Law",
                choices = c("Mass Action" = "MA",
                            "Regulated Mass Action" = "MAwR"
                )
              ),
              pickerInput(
                inputId = "eqn_chem_forward_or_both"
                ,label = "Reaction Direction"
                ,choices = c("Reversible" = "both_directions",
                             "Forward" = 'forward_only')
                ,choicesOpt = list(icon = c("glyphicon glyphicon-resize-horizontal",
                                            "glyphicon glyphicon-arrow-right"
                                            )
                                   )
              ),
              conditionalPanel(
                condition = "input.eqn_chem_law == 'MAwR'",
                hr(),
                prettyCheckbox(
                  inputId = "eqn_options_chem_modifier_forward"
                  ,label = "Add Forward Regulator(s)"
                  ,value = FALSE
                ),
                conditionalPanel(
                  condition = "input.eqn_options_chem_modifier_forward"
                  ,numericInput(inputId = "eqn_options_chem_num_forward_regulators"
                                ,label = "# of Forward Regulators"
                                ,value = 1
                                ,min = 1
                                ,step = 1)
                ),
                conditionalPanel(
                  condition = "input.eqn_chem_forward_or_both == 'both_directions'",
                  prettyCheckbox(
                    inputId = "eqn_options_chem_modifier_reverse"
                    ,label = "Add Reverse Regulator(s)"
                    ,value = FALSE
                  ),
                  conditionalPanel(
                    condition = "input.eqn_options_chem_modifier_reverse",
                    numericInput(inputId = "eqn_options_chem_num_reverse_regulators"
                                 ,label = "# of Reverse Regulators"
                                 ,value = 1
                                 ,min = 1
                                 ,step = 1)
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.eqnCreate_type_of_equation == 'enzyme_rxn'",
              pickerInput(
                inputId = "eqn_enzyme_law",
                label = "Law",
                choices = c("Michaelis Menten Kinetics" = "MM",
                            "Other" = "Other")
              ),
              hr(),
              prettyCheckbox(
                inputId = "eqn_options_enzyme_useVmax"
                ,label = "Use Vmax"
                ,value = FALSE
              )
            ),
            conditionalPanel(
              condition = "input.eqnCreate_type_of_equation == 'syn'",
              pickerInput(
                inputId = "eqn_syn_law",
                label = "Law",
                choices = c("Rate" = "rate",
                            "By Factor" = "byFactor")
              )
            ),
            conditionalPanel(
              condition = "input.eqnCreate_type_of_equation == 'deg'",
              pickerInput(
                inputId = "eqn_deg_law",
                label = "Law",
                choices = c("Rate" = "rate",
                            "By Enzyme" = "byEnzyme")
              ),
              hr(),
              prettyCheckbox(
                inputId = "eqn_deg_to_products",
                label = "Degrades to species",
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.eqn_deg_to_products",
                numericInput(
                  inputId = "eqn_deg_num_products",
                  label = "Number of Species",
                  value = 1,
                  min = 1,
                  step = 1
                )
              ),
              conditionalPanel(
                condition = "input.eqn_deg_law == 'byEnzyme'",
                hr(),
                prettyCheckbox(
                  inputId = "eqn_deg_use_Vmax",
                  label = "Use Vmax",
                  value = FALSE
                ),
                
              )
            )
          ),
          conditionalPanel(
            condition = "input.eqn_action == 'Edit'",
            prettyRadioButtons(
              inputId = "eqnCreate_edit_reg_or_custom",
              label = "Select Type",
              choices = c("Equations", "Custom"),
              inline = TRUE
            ),
            hr(),
            conditionalPanel(
              condition = "input.eqnCreate_edit_reg_or_custom == 'Equations'",
              pickerInput(
                inputId = "eqnCreate_edit_select_equation",
                label = "Select Equation Number to Edit",
                choices = ""
              )
            ),
            conditionalPanel(
              condition = "input.eqnCreate_edit_reg_or_custom == 'Custom'",
              pickerInput(
                inputId = "eqnCreate_edit_select_equation_custom",
                label = "Select Custom Equation to Edit",
                choices = ""
              )
            ),
            hr(),
            uiOutput("eqnCreate_renderingUIcomponents")
          ),
          conditionalPanel(
            condition = "input.eqn_action == 'Delete'",
            prettyRadioButtons(
              inputId = "eqnCreate_delete_eqn_type",
              label = "Type",
              choices = c("Equation", "Custom")
            )
          )
        )
      ),
  #-----------------------------------------------------------------------------
  
  # Equation Builder Tabbox
  
  #-----------------------------------------------------------------------------
      column(
        width = 9,
        tabBox(
          width = 12,
          id = "tabbox_equation_builder",
          tabPanel(
            "Equation",
            conditionalPanel(
              condition = "input.eqn_action == 'New'",
              conditionalPanel(
                condition = "input.eqnCreate_type_of_equation=='chem_rxn'",
                fluidRow(
                  column(
                    width = 3, 
                    numericInput(inputId = "eqnCreate_num_of_eqn_LHS"
                                 ,label = "Number of Reactants"
                                 ,value = 1
                                 ,min = 1
                                 ,step = 1)
                  ),
                  column(
                    width = 3,
                    numericInput(inputId = "eqnCreate_num_of_eqn_RHS"
                                 ,label = "Number of Products"
                                 ,value = 1
                                 ,min = 1
                                 ,step = 1)
                  )
                ),
                hr(),
                uiOutput("eqnCreate_equationBuilder_chem"),
                tags$head(tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              ")))
              ),
              conditionalPanel(
                condition = "input.eqnCreate_type_of_equation == 'enzyme_rxn'",
                uiOutput("eqnCreate_equationBuilder_enzyme")
              ),
              conditionalPanel(
                condition = "input.eqnCreate_type_of_equation == 'syn'",
                uiOutput("eqnCreate_equationBuilder_synthesis")
              ),
              conditionalPanel(
                condition = "input.eqnCreate_type_of_equation == 'deg'",
                uiOutput("eqnCreate_equationBuilder_degradation")
              ),
              conditionalPanel(
                condition = "input.eqnCreate_type_of_equation == 'simp_diff'",
                uiOutput("eqnCreate_equationBuilder_simp_diff")
              ),
              conditionalPanel(
                condition = "input.eqnCreate_type_of_equation == 'rate_eqn'",
                uiOutput("eqnCreate_equationBuilder_custom_rate")
              ),
              conditionalPanel(
                condition = "input.eqnCreate_type_of_equation == 'time_dependent'",
                uiOutput("eqnCreate_equationBuilder_time_equation")
              ),
              hr(),
              fluidRow(
                column(
                  width = 10,
                  uiOutput("eqnCreate_showEquationBuilding")
                ),
                column(
                  width = 2,
                  align = "right",
                  div(style = "padding-top:6px",
                      actionButton(
                        inputId = "eqnCreate_addEqnToVector"
                        ,label = "Add Equation"
                        ,width = "145px"))
                )
              )
            ),
            conditionalPanel(
              condition = "input.eqn_action == 'Edit'", 
              conditionalPanel(
                condition = "input.eqnCreate_type_of_equation_edit == 'chem_rxn'",
                uiOutput("eqnCreate_equationBuilder_chem_edit")
              ),
              conditionalPanel(
                condition = "input.eqnCreate_type_of_equation_edit == 'enzyme_rxn'",
                uiOutput("eqnCreate_equationBuilder_enzyme_edit")
              ),
              conditionalPanel(
                condition = "input.eqnCreate_type_of_equation_edit == 'syn'",
                uiOutput("eqnCreate_equationBuilder_synthesis_edit")
              ),
              conditionalPanel(
                condition = "input.eqnCreate_type_of_equation_edit == 'deg'",
                uiOutput("eqnCreate_equationBuilder_degradation_edit")
              ),
              conditionalPanel(
                condition = "input.eqnCreate_type_of_equation_edit == 'rate_eqn'",
                uiOutput("eqnCreate_equationBuilder_custom_rate_edit")
              ),
              conditionalPanel(
                condition = "input.eqnCreate_type_of_equation_edit == 'time_dependent'",
                uiOutput("eqnCreate_equationBuilder_time_equation_edit")
              ),
              hr(),
              fluidRow(
                column(
                  width = 8,
                  uiOutput("build_equation_edit")
                ),
                column(
                  width = 4,
                  align = "right",
                  div(
                    style = "display: inline-block;
                             vertical-align:top;
                             padding-top:25px;
                             padding-left:-35px",
                    actionButton(inputId = "createEqn_store_edit_button",
                                 label = "Edit")
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.eqn_action == 'Delete'",
              fluidRow(
                column(
                  width = 6,
                  conditionalPanel(
                    condition = "input.eqnCreate_delete_eqn_type == 'Equation'",
                    pickerInput(
                      inputId = "eqnCreate_delete_equation",
                      label = "Select Equation Number to Delete",
                      choices = ""
                    )
                  ),
                  conditionalPanel(
                    condition = "input.eqnCreate_delete_eqn_type == 'Custom'",
                    pickerInput(
                      inputId = "eqnCreate_delete_equation_custom",
                      label = "Select Custom Equation Number to Delete",
                      choices = ""
                    )
                  )
                ),
                column(
                  width = 2,
                  conditionalPanel(
                    condition = "input.eqnCreate_delete_eqn_type == 'Equation'",
                  div
                  (style = "display: inline-block;
                            vertical-align:top;
                            padding-top:28px;
                            padding-left:-35px",
                    actionButton(
                      inputId = "createEqn_delete_equation_button",
                      label = "Delete")
                    )
                  ), 
                  conditionalPanel(
                    condition = "input.eqnCreate_delete_eqn_type == 'Custom'",
                    div
                    (style = "display: inline-block;
                            vertical-align:top;
                            padding-top:28px;
                            padding-left:-35px",
                      actionButton(
                        inputId = "createEqn_delete_custom_equation_button",
                        label = "Delete")
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            "Info",
            # conditionalPanel(
            #   condition = "input.eqnCreate_type_of_equation == 'chem_rxn'",
            #   conditionalPanel(
            #     condition = "input.eqn_chem_law == 'MA'",
            #     uiOutput("mathjax_MA")
            #   ),
            #   conditionalPanel(
            #     condition = "input.eqn_chem_law == 'MAwR'",
            #     uiOutput("mathjax_MA_with_regulators")
            #   ),
            # ),
            # conditionalPanel(
            #   condition = "input.eqnCreate_type_of_equation =='enzyme_rxn'",
            #   uiOutput("enzyme_MM")
            #   )
            "Coming Soon..."
          )
        )
      )
    ),
  
  #-----------------------------------------------------------------------------
  
  # Bottom box of equation page displaying eqns
  
  #-----------------------------------------------------------------------------
  fluidRow(
    column(
      width = 12,
      tabBox(
        id = "eqns_tabbox",
        width = 12,
        tabPanel("Equations",
                 htmlOutput(outputId = "eqnCreate_showEquations")),
        tabPanel(
          "Custom",
          htmlOutput(outputId = "eqnCreate_showAdditionalEquations")
        ),
        tabPanel(
          "Equation Descriptions",
          radioGroupButtons(
            inputId = "eqnCreate_EqnDescriptionDisplayType",
            "View Type",
            choices = c("Single View" = "single",
                        "Flow View" = "flow"),
            checkIcon = list(yes = icon("ok",
                                        lib = "glyphicon"))
          ),
          conditionalPanel(
            condition = "input.eqnCreate_EqnDescriptionDisplayType == 'single'",
            pickerInput(
              "eqnCreate_selectEqnForDescription",
              "Select Equation",
              choices = c()
            ),
            uiOutput("eqnCreate_eqnDescription"),
            actionButton(
              inputId = "eqnCreate_storeEqnDescription",
              label = "Store Description",
              width = "145px"
            )
          ),
          conditionalPanel(
            condition = "input.eqnCreate_EqnDescriptionDisplayType == 'flow'",
            uiOutput("eqnCreate_eqnDescriptionFlow"))
        )
     ) #end tabbox
    ) #end column
  ), #end fluidRow
  tags$head(tags$style("#eqnbuilder_sidebar {min-height:480px")),
  tags$head(tags$style("#tabbox_equation_builder_box {min-height:480px")),
  tags$head(tags$style("#eqnCreate_addEqnToVector {margin-top: 15px")),
  tags$head(tags$style("#eqnCreate_showEquationBuilding {margin-top: 15px"))
)#end TabItem


