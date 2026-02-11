shinyBS::bsModal(
  id = "modal_create_equations",
  title = NULL,
  trigger = "eqns_add_open_modal",
  size = "large",
  fluidRow(
    column(
      width = 3,
      pickerInput(
        inputId = "eqnCreate_active_compartment",
        label = "Active Compartment",
        choices = c()
      )
    )
  ),
  fluidRow(
    column(
      width = 3,
# Main Sidebar -----------------------------------------------------------------
      box(
        id = "eqnbuilder_sidebar",
        solidHeader = FALSE,
        width = 12,
        collapsible = FALSE,
        pickerInput(
          inputId = "eqnCreate_type_of_equation",
          label = "Reaction Type",
          choices = c("All" = "All",
                      "Chemical Reaction" = "chemical_reaction",
                      "Enzyme Based Reaction" = "enzyme_reaction",
                      "Custom Reaction" = "custom_reaction",
                      "Time Dependent Equation" = "time_dependent"
          )
        ),
        pickerInput(
          inputId = "eqnCreate_reaction_law",
          label = "Reaction Law",
          choices = c()
        ),
        
## Mass Action -----------------------------------------------------------------
        conditionalPanel(
          condition = "input.eqnCreate_reaction_law == 'mass_action'",
          pickerInput(
            inputId = "PI_mass_action_reverisble_option",
            label = "Reversability?",
            choices = c("Reversible" = "both_directions",
                        "Irreversible" = "forward_only"),
            choicesOpt = list(icon = c("glyphicon glyphicon-resize-horizontal",
                                       "glyphicon glyphicon-arrow-right"))
          )
        ),
## Mass Action w Regulation ----------------------------------------------------
        conditionalPanel(
          condition = "input.eqnCreate_reaction_law == 'mass_action_w_reg'",
          pickerInput(
            inputId = "reaction_mass_action_wReg_reverisble",
            label = "Reversability?",
            choices = c("Reversible" = "both_directions",
                        "Irreversible" = 'forward_only'),
            choicesOpt = 
              list(
                icon = c("glyphicon glyphicon-resize-horizontal",
                         "glyphicon glyphicon-arrow-right"
                )
              )
          ),
          hr(),
          prettyCheckbox(
            inputId = "CB_MAwR_chem_modifier_forward",
            label = "Add Forward Regulator(s)",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.CB_MAwR_chem_modifier_forward",
            numericInput(
              inputId = "NI_MAwR_n_forward_regulators",
              label = "# of Forward Regulators",
              value = 1,
              min = 1,
              step = 1)
          ),
          conditionalPanel(
            condition = "input.reaction_mass_action_wReg_reverisble == 
                                                          'both_directions'",
            prettyCheckbox(
              inputId = "CB_MAwR_chem_modifier_reverse",
              label = "Add Reverse Regulator(s)",
              value = FALSE
            ),
            conditionalPanel(
              condition = 
                "input.CB_MAwR_chem_modifier_reverse",
              numericInput(
                inputId = 
                  "NI_MAwR_n_reverse_regulators",
                label = "# of Reverse Regulators",
                value = 1,
                min = 1,
                step = 1)
            )
          )
        ),
## Synthesis -------------------------------------------------------------------
        conditionalPanel(
          condition = "input.eqnCreate_reaction_law == 'synthesis'",
          prettyCheckbox(
            inputId = "CB_synthesis_factor_checkbox",
            label = "Factor Driving Synthesis?",
            value = FALSE
          )
        ),

## Degradation By Rate ---------------------------------------------------------
        conditionalPanel(
          condition = "input.eqnCreate_reaction_law == 'degradation_rate'",
          prettyCheckbox(
            inputId = "CB_degradation_rate_toProducts",
            label = "Degrade Into Products?",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.CB_degradation_rate_toProducts",
            numericInput(
              inputId = "NI_degradation_rate_num_products",
              label = "Number of Products",
              value = 1,
              min = 1,
              step = 1
            )
          )
        ),
## Degradation By Enzyme -------------------------------------------------------
        conditionalPanel(
          condition = "input.eqnCreate_reaction_law == 'degradation_by_enzyme'",
          prettyCheckbox(
            inputId = "CB_degradation_enzyme_toProducts",
            label = "Degrade Into Products?",
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.CB_degradation_enzyme_toProducts",
            numericInput(
              inputId = "NI_degradation_enzyme_num_products",
              label = "Number of Products",
              value = 1,
              min = 1,
              step = 1
            )
          ),
          hr(),
          prettyCheckbox(
            inputId = "CB_degradation_enzyme_useVmax",
            label = "Use Vmax",
            value = FALSE
          )
        ),
## Michaelis Menten ------------------------------------------------------------
        conditionalPanel(
          condition = "input.eqnCreate_reaction_law == 'michaelis_menten'",
          prettyCheckbox(
            inputId = "CB_michaelis_menten_useVmax",
            label = "Use Vmax",
            value = FALSE
          )
        ),
## Add Custom Law --------------------------------------------------------------
        conditionalPanel(
          condition = "input.eqnCreate_reaction_law == 'create_custom'",
          # textInput(
          #   inputId = "TI_CC_law_name",
          #   label = "Law Name", 
          #   value = "",
          #   placeholder = "ChemLaw2"
          # ),
          # textAreaInput(
          #   inputId = "TI_CC_law_description",
          #   label = "Description",
          #   value = "",
          #   placeholder = "This law describes the interaction of ...."
          # )
        )
      )
    ),
# Equation Builder Tabbox ------------------------------------------------------
    column(
      width = 9,
      tabBox(
        width = 12,
        # solidHeader = FALSE,
        # collapsible = FALSE,
        id = "tabbox_equation_builder",
        tabPanel(
          "Build", 
          conditionalPanel(
            condition = 
              "input.eqnCreate_reaction_law == 'mass_action'",
            fluidRow(
              column(
                width = 3, 
                numericInput(
                  inputId = "NI_mass_action_num_reactants",
                  label = "Number of Reactants",
                  value = 1,
                  min = 1,
                  step = 1)
              ), 
              column(
                width = 3,
                numericInput(
                  inputId = "NI_mass_action_num_products",
                  label = "Number of Products",
                  value = 1,
                  min = 1,
                  step = 1
                )
              )
            ),
            hr(),
            uiOutput("equationBuilder_mass_action"),
            tags$head(tags$style(HTML("
                          .shiny-split-layout > div {
                            overflow: visible;
                          }
                          ")))
          ),
          conditionalPanel(
            condition = 
              "input.eqnCreate_reaction_law == 'mass_action_w_reg'",
            fluidRow(
              column(
                width = 3, 
                numericInput(
                  inputId = "NI_mass_action_wReg_num_reactants",
                  label = "Number of Reactants",
                  value = 1,
                  min = 1,
                  step = 1)
              ), 
              column(
                width = 3,
                numericInput(
                  inputId = "NI_mass_action_wReg_num_products",
                  label = "Number of Products",
                  value = 1,
                  min = 1,
                  step = 1)
              )
            ),
            hr(),
            uiOutput("equationBuilder_mass_action_w_regulation"),
            tags$head(tags$style(HTML("
                          .shiny-split-layout > div {
                            overflow: visible;
                          }
                          ")))
          ),
          conditionalPanel(
            condition = 
              "input.eqnCreate_reaction_law == 'michaelis_menten'",
            uiOutput("equationBuilder_michaelis_menten")
          ),
          conditionalPanel(
            condition = 
              "input.eqnCreate_reaction_law == 'synthesis'",
            uiOutput("equationBuilder_synthesis")
          ),
          conditionalPanel(
            condition = 
              "input.eqnCreate_reaction_law == 'degradation_rate'",
            uiOutput("equationBuilder_degradation_rate")
          ),
          conditionalPanel(
            condition = 
              "input.eqnCreate_reaction_law == 'degradation_by_enzyme'",
            uiOutput("equationBuilder_degradation_by_enzyme")
          ),
          conditionalPanel(
            condition = 
              "input.eqnCreate_reaction_law == 'create_custom'",
            uiOutput("equationBuilder_create_custom_reaction")
          ),
          conditionalPanel(
            condition = 
              "input?.eqnCreate_reaction_law?.startsWith('user_custom_law_')",
            uiOutput("equationBuilder_user_custom_reaction")
          ),
          conditionalPanel(
            condition = 
              "input.eqnCreate_type_of_equation == 'rate_eqn'",
            uiOutput("eqnCreate_equationBuilder_custom_rate")
          ),
          conditionalPanel(
            condition = 
              "input.eqnCreate_type_of_equation == 
                                                          'time_dependent'",
            uiOutput("eqnCreate_equationBuilder_time_equation")
          ),
          hrTitle("Reaction Form"),
          fluidRow(
            column(
              width = 12,
              uiOutput("eqnCreate_showEquationBuilding")
            )
            # column(
            #   width = 2,
            #   align = "right",
            #   div(style = "padding-top:6px",
            #       )
            # )
          )      
        ),
        tabPanel(
          "Description",
          fluidRow(
            column(
              width = 12,
              h6("Below is a box to add a description of the reaction occuring.
                 If no information is entered, an autogenerated description of
                 the reaction is built using the reaction information."),
             textAreaInput(
               inputId = "TAI_reaction_description_add",
               label = "Reaction Description",
               value = "",
               placeholder = "Enter your reaction description here."
             )
            )
          )
        )
        
        # tabPanel(
        #   "Info",
        #   # conditionalPanel(
        #   #   condition = 
        #   #"input.eqnCreate_type_of_equation == 'chem_rxn'",
        #   #   conditionalPanel(
        #   #     condition = "input.eqn_chem_law == 'MA'",
        #   #     uiOutput("mathjax_MA")
        #   #   ),
        #   #   conditionalPanel(
        #   #     condition = "input.eqn_chem_law == 'MAwR'",
        #   #     uiOutput("mathjax_MA_with_regulators")
        #   #   ),
        #   # ),
        #   # conditionalPanel(
        #   #   condition = 
        #   #"input.eqnCreate_type_of_equation =='enzyme_rxn'",
        #   #   uiOutput("enzyme_MM")
        #   #   )
        #   "Coming Soon..."
        # )
      )
    )
    
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      checkboxInput(
        inputId = "checkbox_modal_keep_active_add_eqn",
        label = "Close on Add",
        value = FALSE
      )
    ),
    column(
      width = 6,
      align = "right",
      actionButton(
        inputId = "eqnCreate_addEqnToVector",
        label = "Add Equation",
        width = "auto")
    )
  )
)