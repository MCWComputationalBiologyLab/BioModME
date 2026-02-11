shinyBS::bsModal(
  id = "modal_edit_equations",
  title = NULL,
  trigger = "eqns_edit_open_modal",
  size = "large",
  fluidRow(
    column(
      width = 12,
      "To edit equations, select the equation you wish to edit. Then change
         the variables or parameter values. Note that you cannot change the 
         compartment with edit."
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 12, 
      box(
        width = 12,
        solidHeader = FALSE,
        collapsible = FALSE,
        fluidRow(
          column(
            width = 3,
            pickerInput(
              inputId = "eqnCreate_edit_select_equation",
              label = "Select Equation Number to Edit",
              choices = ""
            )
          ),
          column(
            width = 9,
            uiOutput("build_equation_edit")
          )
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 3,
        div(
          style = "background-color: #F9F9F9;
                   border: 1px solid #c5c5c5;
                   border-radius: 12px;
                   padding: 10px 10px 10px 10px",
          uiOutput("eqnCreate_edit_rendering_sidebar")
        )
    ),
    column(
      width = 9,
      div(
        style = "background-color:#F9F9F9;
                 border: 1px solid #c5c5c5;
                 border-radius: 12px;
                 padding: 10px 10px 10px 10px;",
        uiOutput("eqnCreate_edit_rending_mainbar"),
        conditionalPanel(
          condition = 
            "input.eqnCreate_reaction_law_edit == 'mass_action'",
          
          hr(),
          uiOutput("equationBuilder_mass_action_edit"),
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_reaction_law_edit == 'mass_action_w_reg'",
          fluidRow(
            column(
              width = 3, 
              numericInput(
                inputId = "NI_mass_action_wReg_num_reactants_edit",
                label = "Number of Reactants",
                value = 1,
                min = 1,
                step = 1)
            ), 
            column(
              width = 3,
              numericInput(
                inputId = "NI_mass_action_wReg_num_products_edit",
                label = "Number of Products",
                value = 1,
                min = 1,
                step = 1)
            )
          ),
          hr(),
          uiOutput("equationBuilder_mass_action_w_regulation_edit")
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_reaction_law_edit == 'michaelis_menten'",
          uiOutput("equationBuilder_michaelis_menten_edit")
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_reaction_law_edit == 'synthesis'",
          uiOutput("equationBuilder_synthesis_edit")
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_reaction_law_edit == 'degradation_rate'",
          uiOutput("equationBuilder_degradation_rate_edit")
        ),
        conditionalPanel(
          condition = 
            "input.eqnCreate_reaction_law_edit == 'degradation_by_enzyme'",
          uiOutput("equationBuilder_degradation_by_enzyme_edit")
        ),
        conditionalPanel(
          condition =
            "input.eqnCreate_type_of_equation_edit == 'rate_eqn'",
          uiOutput("eqnCreate_equationBuilder_custom_rate_edit")
        ),
        conditionalPanel(
          condition =
            "input.eqnCreate_type_of_equation_edit == 'time_dependent'",
          uiOutput("eqnCreate_equationBuilder_time_equation_edit")
        )
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 12,
      align = "right",
      div(
        actionButton("modal_editEqn_edit_button",
                     "Confirm Edit")
      )
    )
  )
)