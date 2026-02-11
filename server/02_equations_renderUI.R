# This script holds the renderUIs for the equation building suite
# w.load.MA.vars <- Waiter$new(id = "eqnCreate_equationBuilder_chem")
# w.load.MA.vars$show()
# w.load.MA.vars$hide()

output$equationBuilder_mass_action <- renderUI({
  number.reactants <- as.numeric(input$NI_mass_action_num_reactants)
  number.products  <- as.numeric(input$NI_mass_action_num_products)
  
  div(
    fluidRow(
      column(
        style = "border-right: 1px solid #e5e5e5; padding-right:20px",
        width = 4,
        lapply(seq(number.reactants), function(i){
          div(
            HTML(paste0("<b>Reactant ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("NI_MA_r_stoichiometry_", as.character(i)),
                label = NULL,
                value = 1,
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("PI_MA_reactant_", as.character(i)),
                label = NULL,
                choices = sort(rv.SPECIES$df.by.compartment$Name),
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith",
                                        dropupAuto = FALSE)
              ),
              cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "border-right: 1px solid #e5e5e5; 
                 padding-right: 20px; 
                 padding-left: 20px;",
        width = 4,
        lapply(seq(number.products), function(i){
          div(
            HTML(paste0("<b>Product ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("NI_MA_p_stoichiometry_", as.character(i)),
                label = NULL,
                value = 1,
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("PI_MA_product_", as.character(i)),
                label = NULL,
                choices = sort(rv.SPECIES$df.by.compartment$Name),
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith",
                                        dropupAuto = FALSE)
              ),
              cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "padding-left: 20px; padding-right: 0px",
        width = 3,
        textInput(
          inputId = "TI_mass_action_forward_k",
          label = "Forward Rate Constant",
          value = paste0("k_f", 
                         as.character(rv.REACTIONS$reaction.id.counter + 1))
        ),
        tags$head(tags$style("#TI_mass_action_forward_k {margin-top: -7px;}")),
        conditionalPanel(
          condition = 
            "input.PI_mass_action_reverisble_option == 'both_directions'",
          textInput(
            inputId = "TI_mass_action_reverse_k",
            label = "Reverse Rate Constant",
            value = paste0("k_r", 
                           as.character(rv.REACTIONS$reaction.id.counter + 1))
          ),
          tags$head(tags$style("#TI_mass_action_reverse_k {margin-top: -7px;}"))
        )
      ), #end column
      column(
        style = "padding-left: 0px",
        width = 1,
        textInput(
          inputId = "TI_mass_action_forward_k_value",
          label = "Value",
          value = 0
        ),
        conditionalPanel(
          condition = 
            "input.PI_mass_action_reverisble_option == 'both_directions'",
          textInput(
            inputId = "TI_mass_action_reverse_k_value",
            label = "Value",
            value = 0)
          )
        ),
      tags$head(
        tags$style("#TI_mass_action_forward_k_value {margin-top: -7px;}")),
      tags$head(
        tags$style("#TI_mass_action_reverse_k_value {margin-top: -7px;}"))
    ) #end fluidRow`
  )
})

output$equationBuilder_mass_action_w_regulation <- renderUI({
  number.reactants <- as.numeric(input$NI_mass_action_wReg_num_reactants)
  number.products  <- as.numeric(input$NI_mass_action_wReg_num_products)
  
  n.forward.regulators <- as.numeric(input$NI_MAwR_n_forward_regulators)
  n.reverse.regulators <- as.numeric(input$NI_MAwR_n_reverse_regulators)
  
  #Sys.sleep(0.5) 
  div(
    fluidRow(
      column(
        style = "border-right: 1px solid #e5e5e5; padding-right:20px",
        width = 4,
        lapply(seq(number.reactants), function(i){
          div(
            HTML(paste0("<b>Reactant ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("NI_MAwR_r_stoichiometry_", as.character(i)),
                label = NULL,
                value = 1,
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("PI_MAwR_reactant_", as.character(i)),
                label = NULL,
                choices = sort(rv.SPECIES$df.by.compartment$Name),
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith",
                                        dropupAuto = FALSE)
              ),
              cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "border-right: 1px solid #e5e5e5; 
               padding-right: 20px; 
               padding-left: 20px;",
        width = 4,
        lapply(seq(number.products), function(i){
          div(
            HTML(paste0("<b>Product ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("NI_MAwR_p_stoichiometry_", as.character(i)),
                label = NULL,
                value = 1,
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("PI_MAwR_product_", as.character(i)),
                label = NULL,
                choices = sort(rv.SPECIES$df.by.compartment$Name),
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith",
                                        dropupAuto = FALSE)
              ),
              cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "padding-left: 20px; padding-right: 0px",
        width = 3,
        conditionalPanel(
          condition = "!input.CB_MAwR_chem_modifier_forward",
          textInput(
            inputId = "TI_MAwR_forward_k",
            label = "Forward Rate Constant",
            value = paste0("k_f", 
                           as.character(rv.REACTIONS$reaction.id.counter + 1)
                           )
           ),
          tags$head(
            tags$style(
              "#TI_MAwR_forward_k {margin-top: -7px;}"))
        ),
        conditionalPanel(
          condition = 
            "input.reaction_mass_action_wReg_reverisble == 'both_directions' && 
             !input.CB_MAwR_chem_modifier_reverse",
          textInput(
            inputId = "TI_MAwR_reverse_k",
            label = "Reverse Rate Constant",
            value = paste0("k_r", 
                           as.character(rv.REACTIONS$reaction.id.counter + 1)
                           )
          ),
          tags$head(tags$style("#TI_MAwR_reverse_k {margin-top: -7px;}"))
        )
      ), #end column
      column(
        style = "padding-left: 0px",
        width = 1,
        conditionalPanel(
          condition = "!input.CB_MAwR_chem_modifier_forward",
          textInput(
            inputId = "TI_MAwR_forward_k_value",
            label = "Value",
            value = 0
          )
        ),
        conditionalPanel(
          condition = 
            "input.reaction_mass_action_wReg_reverisble == 'both_directions' && 
             !input.CB_MAwR_chem_modifier_reverse",
          textInput(
            inputId = "TI_MAwR_reverse_k_value",
            label = "Value",
            value = 0)
        )
      ),
      tags$head(
        tags$style("#TI_MAwR_forward_k_value {margin-top: -7px;}")),
      tags$head(
        tags$style("#TI_MAwR_reverse_k_value {margin-top: -7px;}"))
    ), #end fluidRow`
    conditionalPanel(
      condition = "input.CB_MAwR_chem_modifier_forward || 
                   input.CB_MAwR_chem_modifier_reverse",
      hr()
    ),
    fluidRow(
      column(
        width = 3,
        conditionalPanel(
          condition = "input.CB_MAwR_chem_modifier_forward",
          lapply(seq(n.forward.regulators), function(i){
            pickerInput(
              inputId = paste0("PI_MAwR_forward_regulator_", as.character(i)),
              label = paste0("Forward Regulator ", as.character(i)),
              choices = sort(c(rv.SPECIES$df.by.compartment$Name,
                               rv.PARAMETERS$parameters.names)),
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith"))
          })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.CB_MAwR_chem_modifier_forward",
          lapply(seq(n.forward.regulators), function(i){
            textInput(
              inputId = paste0("TI_MAwR_forward_regulator_RC_", 
                               as.character(i)),
              label = "Rate Constant",
              value = paste0("k_f", 
                             as.character(rv.REACTIONS$reaction.id.counter + 1),
                             ".", 
                             as.character(i)
              )
            )
          })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.CB_MAwR_chem_modifier_forward",
          lapply(seq(n.forward.regulators), function(i){
            textInput(
              inputId = paste0("TI_MAwR_forward_regulator_RC_value_",
                               as.character(i)),
              label = "Value",
              value = 0
            )
          })
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        conditionalPanel(
          condition = "input.CB_MAwR_chem_modifier_reverse",
          lapply(seq(n.reverse.regulators), function(i){
            pickerInput(
              inputId = paste0("PI_MAwR_reverse_regulator_", as.character(i)),
              label = paste0("Reverse Regulator ", as.character(i)),
              choices = sort(c(rv.SPECIES$df.by.compartment$Name,
                               rv.PARAMETERS$parameters.names)),
              options = pickerOptions(liveSearch = TRUE
                                      ,liveSearchStyle = "startsWith")
            )
          })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.CB_MAwR_chem_modifier_reverse",
          lapply(seq(n.reverse.regulators), function(i){
            textInput(
              inputId = paste0("TI_MAwR_reverse_regulator_RC_", 
                               as.character(i)),
              label = "Rate Constant",
              value = paste0("k_r",
                             as.character(rv.REACTIONS$reaction.id.counter + 1),
                             ".",
                             as.character(i))
            )
          })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.CB_MAwR_chem_modifier_reverse",
          lapply(seq(n.reverse.regulators), function(i){
            textInput(
              inputId = paste0("TI_MAwR_reverse_regulator_RC_value_",
                               as.character(i)),
              label = "Value",
              value = 0
            )
          })
        )
      )
    )
  )#end div
})

output$equationBuilder_exponential_growth <- renderUI({
  # Count existing exponential growth reactions to generate unique parameter names
  n.existing <- length(rv.REACTIONS$exponentialGrowth)
  param.suffix <- if (n.existing > 0) paste0("_", n.existing + 1) else ""
  
  div(
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId = "PI_exp_growth_species",
          label   = "Growing Species",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      )
    ),
    fluidRow(
      column(
        width = 4,
        textInput(
          inputId = "TI_exp_growth_mu",
          label = "Growth Rate Parameter (mu)",
          value = paste0("mu", param.suffix)
        )
      ),
      column(
        width = 3,
        numericInput(
          inputId = "NI_exp_growth_mu_value",
          label = "Value",
          value = 0.7,
          min = 0,
          step = 0.01
        )
      )
    )
  )
})

output$equationBuilder_logistic_competition <- renderUI({
  # Count existing logistic competition reactions to generate unique parameter names
  n.existing <- length(rv.REACTIONS$logisticCompetition)
  param.suffix <- if (n.existing > 0) paste0("_", n.existing + 1) else ""
  
  # Preserve current input values when switching panels
  # Check both possible input IDs (for both modes) - prioritize the one that matches current checkbox state
  checkbox.state <- if (!is.null(input$CB_log_comp_single_species)) input$CB_log_comp_single_species else FALSE
  if (checkbox.state) {
    # Single species mode - check _2 inputs first
    current.species.x <- if (!is.null(input$PI_log_comp_species_x_2)) input$PI_log_comp_species_x_2 
                        else if (!is.null(input$PI_log_comp_species_x)) input$PI_log_comp_species_x 
                        else NULL
    current.species.y <- if (!is.null(input$PI_log_comp_species_y_2)) input$PI_log_comp_species_y_2 
                        else if (!is.null(input$PI_log_comp_species_y)) input$PI_log_comp_species_y 
                        else NULL
  } else {
    # Both species mode - check regular inputs first
    current.species.x <- if (!is.null(input$PI_log_comp_species_x)) input$PI_log_comp_species_x 
                        else if (!is.null(input$PI_log_comp_species_x_2)) input$PI_log_comp_species_x_2 
                        else NULL
    current.species.y <- if (!is.null(input$PI_log_comp_species_y)) input$PI_log_comp_species_y 
                        else if (!is.null(input$PI_log_comp_species_y_2)) input$PI_log_comp_species_y_2 
                        else NULL
  }
  
  div(
    conditionalPanel(
      condition = "!input.CB_log_comp_single_species",
      # Both species compete (default)
      fluidRow(
        column(
          width = 4,
          pickerInput(
            inputId = "PI_log_comp_species_x",
            label   = "Species X",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.x,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        ),
        column(
          width = 4,
          pickerInput(
            inputId = "PI_log_comp_species_y",
            label   = "Species Y",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.y,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          textInput("TI_log_comp_r_x", "r_x", value = paste0("r_x", param.suffix))
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_r_x_value", "Value", value = 0.7, min = 0, step = 0.01)
        ),
        column(
          width = 3,
          textInput("TI_log_comp_r_y", "r_y", value = paste0("r_y", param.suffix))
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_r_y_value", "Value", value = 0.7, min = 0, step = 0.01)
        )
      ),
      fluidRow(
        column(
          width = 3,
          textInput("TI_log_comp_alpha_xy", "alpha_xy", value = paste0("alpha_xy", param.suffix))
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_alpha_xy_value", "Value", value = 0.1, min = 0, step = 0.01)
        ),
        column(
          width = 3,
          textInput("TI_log_comp_alpha_yx", "alpha_yx", value = paste0("alpha_yx", param.suffix))
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_alpha_yx_value", "Value", value = 0.1, min = 0, step = 0.01)
        )
      ),
      fluidRow(
        column(
          width = 3,
          textInput("TI_log_comp_Kc", "Kc (carrying capacity)", value = paste0("Kc", param.suffix))
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_Kc_value", "Value", value = 1, min = 0.0001, step = 0.1)
        )
      )
    ),
    conditionalPanel(
      condition = "input.CB_log_comp_single_species",
      # Only species X grows competitively, Y is just a competitor
      fluidRow(
        column(
          width = 4,
          pickerInput(
            inputId = "PI_log_comp_species_x_2",
            label   = "Species X (growing competitively)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.x,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        ),
        column(
          width = 4,
          pickerInput(
            inputId = "PI_log_comp_species_y_2",
            label   = "Species Y (competitor only)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.y,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          textInput("TI_log_comp_r_x", "r_x", value = paste0("r_x", param.suffix))
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_r_x_value", "Value", value = 0.7, min = 0, step = 0.01)
        ),
        column(
          width = 3,
          textInput("TI_log_comp_alpha_xy", "alpha_xy", value = paste0("alpha_xy", param.suffix))
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_alpha_xy_value", "Value", value = 0.1, min = 0, step = 0.01)
        )
      ),
      fluidRow(
        column(
          width = 3,
          textInput("TI_log_comp_Kc", "Kc (carrying capacity)", value = paste0("Kc", param.suffix))
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_Kc_value", "Value", value = 1, min = 0.0001, step = 0.1)
        )
      )
    )
  )
})

output$equationBuilder_monod_growth <- renderUI({
  # Count existing monod growth reactions to generate unique parameter names
  n.existing <- length(rv.REACTIONS$monodGrowth)
  param.suffix <- if (n.existing > 0) paste0("_", n.existing + 1) else ""
  
  div(
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId = "PI_monod_species",
          label   = "Growing Species (X)",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      ),
      column(
        width = 4,
        pickerInput(
          inputId = "PI_monod_substrate",
          label   = "Substrate (S)",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        textInput("TI_monod_mu_max", "mu_max", value = paste0("mu_max", param.suffix))
      ),
      column(
        width = 3,
        numericInput("NI_monod_mu_max_value", "Value", value = 0.7, min = 0, step = 0.01)
      ),
      column(
        width = 3,
        textInput("TI_monod_K_s", "K_s (half-saturation)", value = paste0("K_s", param.suffix))
      ),
      column(
        width = 3,
        numericInput("NI_monod_K_s_value", "Value", value = 0.5, min = 0.0001, step = 0.01)
      )
    )
  )
})

output$equationBuilder_competitive_monod <- renderUI({
  # Count existing competitive monod reactions to generate unique parameter names
  n.existing <- length(rv.REACTIONS$competitiveMonod)
  param.suffix <- if (n.existing > 0) paste0("_", n.existing + 1) else ""
  
  # Preserve current input values when switching panels
  checkbox.state <- if (!is.null(input$CB_comp_monod_single_species)) input$CB_comp_monod_single_species else FALSE
  if (checkbox.state) {
    # Single species mode - check _2 inputs first
    current.species.x <- if (!is.null(input$PI_comp_monod_species_x_2)) input$PI_comp_monod_species_x_2 
                        else if (!is.null(input$PI_comp_monod_species_x)) input$PI_comp_monod_species_x 
                        else NULL
    current.species.y <- if (!is.null(input$PI_comp_monod_species_y_2)) input$PI_comp_monod_species_y_2 
                        else if (!is.null(input$PI_comp_monod_species_y)) input$PI_comp_monod_species_y 
                        else NULL
    current.substrate <- if (!is.null(input$PI_comp_monod_substrate_2)) input$PI_comp_monod_substrate_2 
                        else if (!is.null(input$PI_comp_monod_substrate)) input$PI_comp_monod_substrate 
                        else NULL
  } else {
    # Both species mode - check regular inputs first
    current.species.x <- if (!is.null(input$PI_comp_monod_species_x)) input$PI_comp_monod_species_x 
                        else if (!is.null(input$PI_comp_monod_species_x_2)) input$PI_comp_monod_species_x_2 
                        else NULL
    current.species.y <- if (!is.null(input$PI_comp_monod_species_y)) input$PI_comp_monod_species_y 
                        else if (!is.null(input$PI_comp_monod_species_y_2)) input$PI_comp_monod_species_y_2 
                        else NULL
    current.substrate <- if (!is.null(input$PI_comp_monod_substrate)) input$PI_comp_monod_substrate 
                        else if (!is.null(input$PI_comp_monod_substrate_2)) input$PI_comp_monod_substrate_2 
                        else NULL
  }
  
  div(
    conditionalPanel(
      condition = "!input.CB_comp_monod_single_species",
      # Both species compete (default)
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "PI_comp_monod_species_x",
            label   = "Species X",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.x,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        ),
        column(
          width = 3,
          pickerInput(
            inputId = "PI_comp_monod_species_y",
            label   = "Species Y",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.y,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        ),
        column(
          width = 3,
          pickerInput(
            inputId = "PI_comp_monod_substrate",
            label   = "Substrate (S)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.substrate,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        )
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_mu_max_x", "mu_max_x", value = paste0("mu_max_x", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_mu_max_x_value", "Value", value = 0.7, min = 0, step = 0.01)),
        column(width = 3, textInput("TI_comp_monod_mu_max_y", "mu_max_y", value = paste0("mu_max_y", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_mu_max_y_value", "Value", value = 0.7, min = 0, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_K_s_x", "K_s_x", value = paste0("K_s_x", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_K_s_x_value", "Value", value = 0.5, min = 0.0001, step = 0.01)),
        column(width = 3, textInput("TI_comp_monod_K_s_y", "K_s_y", value = paste0("K_s_y", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_K_s_y_value", "Value", value = 0.5, min = 0.0001, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_alpha_xy", "alpha_xy", value = paste0("alpha_xy", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_alpha_xy_value", "Value", value = 0.1, min = 0, step = 0.01)),
        column(width = 3, textInput("TI_comp_monod_alpha_yx", "alpha_yx", value = paste0("alpha_yx", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_alpha_yx_value", "Value", value = 0.1, min = 0, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_Kc", "Kc (carrying capacity)", value = paste0("Kc", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_Kc_value", "Value", value = 1, min = 0.0001, step = 0.1)),
        column(width = 3, textInput("TI_comp_monod_Y_x", "Y_x (yield)", value = paste0("Y_x", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_Y_x_value", "Value", value = 0.5, min = 0.0001, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_Y_y", "Y_y (yield)", value = paste0("Y_y", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_Y_y_value", "Value", value = 0.5, min = 0.0001, step = 0.01))
      )
    ),
    conditionalPanel(
      condition = "input.CB_comp_monod_single_species",
      # Only species X grows competitively, Y is just a competitor
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "PI_comp_monod_species_x_2",
            label   = "Species X (growing competitively)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.x,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        ),
        column(
          width = 3,
          pickerInput(
            inputId = "PI_comp_monod_species_y_2",
            label   = "Species Y (competitor only)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.y,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        ),
        column(
          width = 3,
          pickerInput(
            inputId = "PI_comp_monod_substrate_2",
            label   = "Substrate (S)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.substrate,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        )
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_mu_max_x", "mu_max_x", value = paste0("mu_max_x", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_mu_max_x_value", "Value", value = 0.7, min = 0, step = 0.01)),
        column(width = 3, textInput("TI_comp_monod_K_s_x", "K_s_x", value = paste0("K_s_x", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_K_s_x_value", "Value", value = 0.5, min = 0.0001, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_alpha_xy", "alpha_xy", value = paste0("alpha_xy", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_alpha_xy_value", "Value", value = 0.1, min = 0, step = 0.01)),
        column(width = 3, textInput("TI_comp_monod_Kc", "Kc (carrying capacity)", value = paste0("Kc", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_Kc_value", "Value", value = 1, min = 0.0001, step = 0.1))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_Y_x", "Y_x (yield)", value = paste0("Y_x", param.suffix))),
        column(width = 3, numericInput("NI_comp_monod_Y_x_value", "Value", value = 0.5, min = 0.0001, step = 0.01))
      )
    )
  )
})

output$equationBuilder_competitive_monod_edit <- renderUI({
  # Preserve current input values when switching panels
  checkbox.state.edit <- if (!is.null(input$CB_comp_monod_single_species_edit)) input$CB_comp_monod_single_species_edit else FALSE
  if (checkbox.state.edit) {
    # Single species mode - check _2 inputs first
    current.species.x.edit <- if (!is.null(input$PI_comp_monod_species_x_edit_2)) input$PI_comp_monod_species_x_edit_2 
                              else if (!is.null(input$PI_comp_monod_species_x_edit)) input$PI_comp_monod_species_x_edit 
                              else NULL
    current.species.y.edit <- if (!is.null(input$PI_comp_monod_species_y_edit_2)) input$PI_comp_monod_species_y_edit_2 
                              else if (!is.null(input$PI_comp_monod_species_y_edit)) input$PI_comp_monod_species_y_edit 
                              else NULL
    current.substrate.edit <- if (!is.null(input$PI_comp_monod_substrate_edit_2)) input$PI_comp_monod_substrate_edit_2 
                              else if (!is.null(input$PI_comp_monod_substrate_edit)) input$PI_comp_monod_substrate_edit 
                              else NULL
  } else {
    # Both species mode - check regular inputs first
    current.species.x.edit <- if (!is.null(input$PI_comp_monod_species_x_edit)) input$PI_comp_monod_species_x_edit 
                              else if (!is.null(input$PI_comp_monod_species_x_edit_2)) input$PI_comp_monod_species_x_edit_2 
                              else NULL
    current.species.y.edit <- if (!is.null(input$PI_comp_monod_species_y_edit)) input$PI_comp_monod_species_y_edit 
                              else if (!is.null(input$PI_comp_monod_species_y_edit_2)) input$PI_comp_monod_species_y_edit_2 
                              else NULL
    current.substrate.edit <- if (!is.null(input$PI_comp_monod_substrate_edit)) input$PI_comp_monod_substrate_edit 
                              else if (!is.null(input$PI_comp_monod_substrate_edit_2)) input$PI_comp_monod_substrate_edit_2 
                              else NULL
  }
  
  div(
    conditionalPanel(
      condition = "!input.CB_comp_monod_single_species_edit",
      # Both species compete (default)
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "PI_comp_monod_species_x_edit",
            label   = "Species X",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.x.edit,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        ),
        column(
          width = 3,
          pickerInput(
            inputId = "PI_comp_monod_species_y_edit",
            label   = "Species Y",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.y.edit,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        ),
        column(
          width = 3,
          pickerInput(
            inputId = "PI_comp_monod_substrate_edit",
            label   = "Substrate (S)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.substrate.edit,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        )
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_mu_max_x_edit", "mu_max_x", value = "mu_max_x")),
        column(width = 3, numericInput("NI_comp_monod_mu_max_x_value_edit", "Value", value = 0.7, min = 0, step = 0.01)),
        column(width = 3, textInput("TI_comp_monod_mu_max_y_edit", "mu_max_y", value = "mu_max_y")),
        column(width = 3, numericInput("NI_comp_monod_mu_max_y_value_edit", "Value", value = 0.7, min = 0, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_K_s_x_edit", "K_s_x", value = "K_s_x")),
        column(width = 3, numericInput("NI_comp_monod_K_s_x_value_edit", "Value", value = 0.5, min = 0.0001, step = 0.01)),
        column(width = 3, textInput("TI_comp_monod_K_s_y_edit", "K_s_y", value = "K_s_y")),
        column(width = 3, numericInput("NI_comp_monod_K_s_y_value_edit", "Value", value = 0.5, min = 0.0001, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_alpha_xy_edit", "alpha_xy", value = "alpha_xy")),
        column(width = 3, numericInput("NI_comp_monod_alpha_xy_value_edit", "Value", value = 0.1, min = 0, step = 0.01)),
        column(width = 3, textInput("TI_comp_monod_alpha_yx_edit", "alpha_yx", value = "alpha_yx")),
        column(width = 3, numericInput("NI_comp_monod_alpha_yx_value_edit", "Value", value = 0.1, min = 0, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_Kc_edit", "Kc (carrying capacity)", value = "Kc")),
        column(width = 3, numericInput("NI_comp_monod_Kc_value_edit", "Value", value = 1, min = 0.0001, step = 0.1)),
        column(width = 3, textInput("TI_comp_monod_Y_x_edit", "Y_x (yield)", value = "Y_x")),
        column(width = 3, numericInput("NI_comp_monod_Y_x_value_edit", "Value", value = 0.5, min = 0.0001, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_Y_y_edit", "Y_y (yield)", value = "Y_y")),
        column(width = 3, numericInput("NI_comp_monod_Y_y_value_edit", "Value", value = 0.5, min = 0.0001, step = 0.01))
      )
    ),
    conditionalPanel(
      condition = "input.CB_comp_monod_single_species_edit",
      # Only species X grows competitively, Y is just a competitor
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "PI_comp_monod_species_x_edit_2",
            label   = "Species X (growing competitively)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.x.edit,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        ),
        column(
          width = 3,
          pickerInput(
            inputId = "PI_comp_monod_species_y_edit_2",
            label   = "Species Y (competitor only)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.y.edit,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        ),
        column(
          width = 3,
          pickerInput(
            inputId = "PI_comp_monod_substrate_edit_2",
            label   = "Substrate (S)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.substrate.edit,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        )
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_mu_max_x_edit", "mu_max_x", value = "mu_max_x")),
        column(width = 3, numericInput("NI_comp_monod_mu_max_x_value_edit", "Value", value = 0.7, min = 0, step = 0.01)),
        column(width = 3, textInput("TI_comp_monod_K_s_x_edit", "K_s_x", value = "K_s_x")),
        column(width = 3, numericInput("NI_comp_monod_K_s_x_value_edit", "Value", value = 0.5, min = 0.0001, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_alpha_xy_edit", "alpha_xy", value = "alpha_xy")),
        column(width = 3, numericInput("NI_comp_monod_alpha_xy_value_edit", "Value", value = 0.1, min = 0, step = 0.01)),
        column(width = 3, textInput("TI_comp_monod_Kc_edit", "Kc (carrying capacity)", value = "Kc")),
        column(width = 3, numericInput("NI_comp_monod_Kc_value_edit", "Value", value = 1, min = 0.0001, step = 0.1))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_Y_x_edit", "Y_x (yield)", value = "Y_x")),
        column(width = 3, numericInput("NI_comp_monod_Y_x_value_edit", "Value", value = 0.5, min = 0.0001, step = 0.01))
      )
    )
  )
})

output$equationBuilder_synthesis <- renderUI({
  
  div(
    conditionalPanel(
      condition = "!input.CB_synthesis_factor_checkbox",
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "PI_synthesis_rate_var",
            label   = "Species to synthesize",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith") 
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          textInput(
            inputId = "TI_synthesis_rate_RC",
            label = "Rate Constant",
            value = paste0("k_syn",
                           as.character(rv.REACTIONS$reaction.id.counter + 1))
            
          )
        ),
        column(
          width = 3, 
          textInput(
            inputId = "TI_synthesis_rate_RC_value",
            label = "Value",
            value = 1
          )
        )
      )
    ), 
    conditionalPanel(
      condition = "input.CB_synthesis_factor_checkbox",
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "PI_synthesis_byFactor_var",
            label   = "Species to synthesize",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith") 
          )
        ),
        column(
          width = 3, 
          pickerInput(
            inputId = "PI_synthesis_byFactor_factor",
            label = "Factor causing synthesis",
            choices = sort(rv.SPECIES$df.by.compartment$Name)
          )
        )
      ),
      fluidRow(
        column(
          width = 3, 
          textInput(
            inputId = "TI_synthesis_byFactor_RC",
            label = "Rate Constant",
            value = paste0("k_syn", 
                           as.character(rv.REACTIONS$reaction.id.counter + 1))
          )
        ),
        column(
          width = 3, 
          textInput(
            inputId = "TI_synthesis_byFactor_RC_value",
            label = "Value",
            value = 1
          )
        )
      )
    )
  )
})

output$equationBuilder_degradation_rate <- renderUI({
  # Count existing degradation reactions with krel to generate unique parameter names
  n.existing.with.krel <- 0
  if (length(rv.REACTIONS$degradation.by.rate) > 0) {
    for (i in seq_along(rv.REACTIONS$degradation.by.rate)) {
      degInfo <- rv.REACTIONS$degradation.by.rate[[i]]
      if ("krel" %in% names(degInfo) && !is.na(degInfo$krel) && degInfo$krel != "") {
        n.existing.with.krel <- n.existing.with.krel + 1
      }
    }
  }
  param.suffix <- if (n.existing.with.krel > 0) paste0("_", n.existing.with.krel + 1) else ""
  
  div(
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId = "PI_degradation_rate_species",
          label   = "Species to degrade",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith") 
        )
      ),
      column(
        width = 8,
        conditionalPanel(
          condition = "input.CB_degradation_rate_toProducts",
          fluidRow(
            column(
              width = 12,
              prettyCheckbox(
                inputId = "CB_degradation_rate_relative_formation",
                label = "Relative Formation",
                value = FALSE
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              lapply(seq(input$NI_degradation_rate_num_products), function(i){
                pickerInput(
                  inputId = paste0("PI_degradation_rate_product_", as.character(i)),
                  label = paste0("Product ", as.character(i)),
                  choices = sort(rv.SPECIES$df.by.compartment$Name),
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = "startsWith"))
              })
            ),
            column(
              width = 6,
              conditionalPanel(
                condition = "input.CB_degradation_rate_relative_formation",
                fluidRow(
                  column(
                    width = 12,
                    textInput(
                      inputId = "TI_degradation_rate_krel",
                      label = "krel (product yield fraction)",
                      value = paste0("krel", param.suffix)
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    numericInput(
                      inputId = "NI_degradation_rate_krel_value",
                      label = "Value (0-1)",
                      value = 0.1,
                      min = 0,
                      max = 1,
                      step = 0.01
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 8,
        splitLayout(
          textInput(
            inputId = "TI_degradation_rate_RC",
            label = "Rate Constant",
            value = paste0("k_d", 
                           as.character(rv.REACTIONS$reaction.id.counter + 1))
          ),
          textInput(
            inputId = "TI_degradation_rate_RC_value",
            label = "Value",
            value = 0
          ),
          div(
            style = "padding-top:38px; padding-left:15px;",
            checkboxInput(inputId = "CB_degradation_rate_conc_dependent",
                          label = "Concentration Dependent",
                          value = TRUE)
          )
        )
      )  
    )
  )
})

output$equationBuilder_degradation_by_enzyme <- renderUI({
  # Count existing enzyme degradation reactions with krel to generate unique parameter names
  n.existing.with.krel <- 0
  if (length(rv.REACTIONS$degradation.by.enzyme) > 0) {
    for (i in seq_along(rv.REACTIONS$degradation.by.enzyme)) {
      degInfo <- rv.REACTIONS$degradation.by.enzyme[[i]]
      if ("krel" %in% names(degInfo) && !is.na(degInfo$krel) && degInfo$krel != "") {
        n.existing.with.krel <- n.existing.with.krel + 1
      }
    }
  }
  param.suffix <- if (n.existing.with.krel > 0) paste0("_", n.existing.with.krel + 1) else ""
  
  div(
    fluidRow(
      column(
        width = 3,
        pickerInput(
          inputId = "PI_degradation_enzyme_species",
          label   = "Species to degrade",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith") 
        ),
        conditionalPanel(
          condition = "!input.CB_degradation_enzyme_useVmax",
          pickerInput(
            inputId = "PI_degradation_enzyme_enzyme",
            label = "Enzyme",
            choices = sort(rv.SPECIES$df.by.compartment$Name)
          )
        )
      ),
      column(
        width = 9,
        conditionalPanel(
          condition = "input.CB_degradation_enzyme_toProducts",
          fluidRow(
            column(
              width = 12,
              prettyCheckbox(
                inputId = "CB_degradation_enzyme_relative_formation",
                label = "Relative Formation",
                value = FALSE
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              lapply(seq(input$NI_degradation_enzyme_num_products), function(i){
                pickerInput(
                  inputId = paste0("PI_degradation_enzyme_product_", 
                                   as.character(i)),
                  label = paste0("Product ", as.character(i)),
                  choices = sort(rv.SPECIES$df.by.compartment$Name),
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = "startsWith"))
              })
            ),
            column(
              width = 6,
              conditionalPanel(
                condition = "input.CB_degradation_enzyme_relative_formation",
                fluidRow(
                  column(
                    width = 12,
                    textInput(
                      inputId = "TI_degradation_enzyme_krel",
                      label = "krel (product yield fraction)",
                      value = paste0("krel", param.suffix)
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    numericInput(
                      inputId = "NI_degradation_enzyme_krel_value",
                      label = "Value (0-1)",
                      value = 0.1,
                      min = 0,
                      max = 1,
                      step = 0.01
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    hr(),
    conditionalPanel(
      condition = "!input.CB_degradation_enzyme_useVmax",
      fluidRow(
        column(
          style = "padding-right: 0px;",
          width = 3,
          textInput(
            inputId = "TI_degradation_enzyme_kcat",
            label = "kcat",
            value = paste0("k_d", 
                           as.character(rv.REACTIONS$reaction.id.counter+1))
          )
        ),
        column(
          style = "padding-left: 0px;",
          width = 3,
          textInput(
            inputId = "TI_degradation_enzyme_kcat_value",
            label = "Value",
            value = 1
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.CB_degradation_enzyme_useVmax",
      fluidRow(
        column(
          style = "padding-right: 0px;",
          width = 3,
          textInput(
            inputId = "TI_degradation_enzyme_Vmax",
            label = "Vmax",
            value = paste0("Vmax_", 
                           as.character(rv.REACTIONS$reaction.id.counter+1))
          )
        ),
        column(
          style = "padding-left: 0px;",
          width = 3,
          textInput(
            inputId = "TI_degradation_enzyme_Vmax_value",
            label = "Value",
            value = 1
          )
        )
      )
    ),
    fluidRow(
      column(
        style = "padding-right: 0px;",
        width = 3,
        textInput(
          inputId = "TI_degradation_enzyme_Km",
          label = "Km",
          value = paste0("Km_", 
                         as.character(rv.REACTIONS$reaction.id.counter + 1))
        )
      ),
      column(
        style = "padding-left: 0px;",
        width = 3,
        textInput(
          inputId = "TI_degradation_enzyme_Km_value",
          label = "Value",
          value = 1
        )
      )
    )
  )
})

output$equationBuilder_michaelis_menten <- renderUI({
  
  km.name <- paste0("Km_", as.character(rv.REACTIONS$reaction.id.counter + 1))
  vmax.na <- paste0("Vmax_", as.character(rv.REACTIONS$reaction.id.counter + 1))
  kcat.na <- paste0("kcat_", as.character(rv.REACTIONS$reaction.id.counter + 1))
  
  if (input$CB_michaelis_menten_useVmax) {
    widgetList <- list(
      textInput("TI_michaelis_menten_Km",NULL , km.name),
      textInput("TI_michaelis_menten_vmax", NULL, vmax.na)
    )

    widgetList2 <- list(
      textInput("TI_michaelis_menten_Km_value", NULL, 1),
      textInput("TI_michaelis_menten_vmax_value", NULL, 1)
    )
    
    widgetLabels <- c("Km", "Vmax")
  } else {
    widgetList <- list(
      textInput("TI_michaelis_menten_Km", NULL, km.name),
      textInput("TI_michaelis_menten_kcat", NULL, kcat.na)
    )
    
    widgetList2 <- list(
      textInput("TI_michaelis_menten_Km_value", NULL, 1),
      textInput("TI_michaelis_menten_kcat_value", NULL, 1)
    )
    
    widgetLabels <- c("Km", "kcat")
  }
  
  div(
    fluidRow(
      column(
        width = 3,
        pickerInput(
          inputId = "PI_michaelis_menten_substrate",
          label = "Substrate",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(
            liveSearch = TRUE,
            liveSearchStyle = "startsWith",
            dropupAuto = FALSE
          )
        )
      ),
      column(
        width = 3,
        offset = 1,
        pickerInput(
          inputId = "PI_michaelis_menten_product",
          label = "Product",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(
            liveSearch = TRUE,
            liveSearchStyle = "startsWith",
            dropupAuto = FALSE
          )
        )
      ),
      column(
        width = 3, 
        offset = 1,
        conditionalPanel(
          condition = "!input.CB_michaelis_menten_useVmax",
          pickerInput(
            inputId = "PI_michaelis_menten_enzyme",
            label = "Enzyme",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        )
      )
    ),
    hrTitle("Parameters"),
    tableLayoutDualColumns(
      labels = widgetLabels,
      widgets = widgetList,
      widgets2 = widgetList2,
      headerLabels = c("Parameter", "Value"),
      firstColWidth = "15%"
    )
  )#end div
})

output$equationBuilder_user_custom_reaction <- renderUI({
  # Render UI for custom equations build by the user
  # Goes into the respective reactive variable and finds the reaction
  # information, rendering proper ui, such as reactants, parameters, etc
  
  # browser()
  # Find the custom law that is being used
  backend.name <- input$eqnCreate_reaction_law
  # custom.id    <- strsplit(backend.name, "_")[[1]][4]
  # Find id of equation in database
  law.id <- FindId(backend.name)
  # Find the reaction entry of this id
  # law.entry <- rv.CUSTOM.LAWS$cl.reaction[[custom.id]]
  law.entry <- rv.CUSTOM.LAWS$cl.reaction[[law.id]]
  
  has.reactants  <- FALSE
  has.products   <- FALSE
  has.modifiers  <- FALSE
  has.parameters <- FALSE
  
  # Unpack reaction information
  reactants  <- law.entry$Reactants
  products   <- law.entry$Products
  modifiers  <- law.entry$Modifiers
  parameters <- law.entry$Parameters
  # Process specie information
  if (isTruthy(reactants)) {
    reactants     <- strsplit(reactants, ", ")[[1]]
    has.reactants <- TRUE
  }
  
  if (isTruthy(products)) {
    products      <- strsplit(products, ", ")[[1]]
    has.products <- TRUE
  }
  
  if (isTruthy(modifiers)) {
    modifiers     <- strsplit(modifiers, ", ")[[1]]
    has.modifiers <- TRUE
  }
  
  if (isTruthy(parameters)) {
    parameters  <- strsplit(parameters, ", ")[[1]]
    has.parameters <- TRUE
  }
  
  
  # Build and Process UI
  div(
    # h2(paste0("This law is named ", law.entry$Law.Name)),
    fluidRow(
      column(
        width = 4,
        align = "center",
        if (has.reactants) {
          lapply(seq_along(reactants), function(i) {
            pickerInput(
              inputId = paste0("PI_CL_reactant_", as.character(i)),
              label = paste0("Reactant: ", reactants[i]),
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith",
                                      dropupAuto = FALSE)
            )
          })
        } else {
          "No Reactants"
        }
      ),
      column(
        width = 4,
        align = "center",
        if (has.products) {
          lapply(seq_along(products), function(i) {
            pickerInput(
              inputId = paste0("PI_CL_product_", as.character(i)),
              label = paste0("Product: ", products[i]),
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith",
                                      dropupAuto = FALSE)
            )
          })
        } else {
          "No Products"
        }
      ),
      column(
        width = 4,
        align = "center",
        if (has.modifiers) {
          lapply(seq_along(modifiers), function(i) {
            pickerInput(
              inputId = paste0("PI_CL_modifier_", as.character(i)),
              label = paste0("Modifer: ", modifiers[i]),
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith",
                                      dropupAuto = FALSE)
            )
          })
        } else {
          "No Modifiers"
        }
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 4, 
        lapply(seq_along(parameters), function(i) {
          textInput(
            inputId = paste0("PI_CL_parameter_", as.character(i)),
            label = paste0("Parameter: ", parameters[i]),
            value = "",
            placeholder = parameters[i]
          )
        })
      ),
      column(
        width = 4, 
        lapply(seq_along(parameters), function(i) {
          textInput(
            inputId = paste0("PI_CL_parameter_value_", as.character(i)),
            label = "Value",
            value = 0
          )
        })
      )
    )
  )
})

output$equationBuilder_create_custom_reaction <- renderUI({
  
  # div(
  #   fluidRow(
  #     column(
  #       width = 3,
  #       textInput(
  #         inputId = "PI_CC_reactants",
  #         label   = "Reactants",
  #         value = "",
  #         placeholder = "x1, x2"
  #       )
  #     ),
  #     column(
  #       width = 3,
  #       textInput(
  #         inputId = "PI_CC_products",
  #         label   = "Products",
  #         value = "",
  #         placeholder = "y1"
  #       )
  #     ),
  #     column(
  #       width = 3,
  #       textInput(
  #         inputId = "PI_CC_modifiers",
  #         label   = "Modifiers",
  #         value = "",
  #         placeholder = "mod1"
  #       )
  #     )
  #   ),
  #   hr(),
  #   fluidRow(
  #     column(
  #       width = 6, 
  #       textInput(
  #         inputId = "TI_CC_enter_rate_law",
  #         label = "Rate Law",
  #         value = "",
  #         placeholder = "x1*p1*x2^2/(mod*y1)"
  #       )
  #     )
  #   ),
  #   # fluidRow(
  #   #   column(
  #   #     width = 12,
  #   #     "Mathjax Place Holder",
  #   #     withMathJax(
  #   #       MJ_build_custom_rate_law()
  #   #     )
  #   #   )
  #   # ),
  #   hr(),
  #   fluidRow(
  #     column(
  #       width = 12,
  #       rHandsontableOutput("TO_CC_parameter_table")
  #     )
  #   )
  # )
})

# MJ_build_custom_rate_law <- reactive({
#   
#   # Find terms to convert to mathjax
#   a <- parse_string_expression(input$TI_CC_enter_rate_law)
#   
#   valid <- a$valid.terms
#   valid.mj <- sapply(valid, Var2MathJ)
#   
# })




output$eqnCreate_equationBuilder_custom_rate <- renderUI({
  
  div(
    "This is a custom equation entering. Please make sure things are spelled correctly as errors will occur if these equations are improper.",
    "Add any parameters in the 'Parameter Values' tab that would be used in this equation.",
    "These are often used for algebraic equations that need to be added to the model.",
    hr(),
    fluidRow(
      column(
        width = 4,
        textInput(
          inputId = "eqnCreate_custom_eqn_lhs",
          label = "Variable",
          value = "")
    ),
    column(
      width = 1,
      div(style = "padding-top:35px; padding-left: 12px;",
          "=")), 
    column(
      width = 7,
      textInput(
        inputId = "eqnCreate_custom_eqn_rhs",
        label = "Equation",
        value = ""
      )
    ))
  )
})

output$equationBuilder_logistic_competition_edit <- renderUI({
  # Preserve current input values when switching panels
  # Check both possible input IDs (for both modes) - prioritize the one that matches current checkbox state
  checkbox.state.edit <- if (!is.null(input$CB_log_comp_single_species_edit)) input$CB_log_comp_single_species_edit else FALSE
  if (checkbox.state.edit) {
    # Single species mode - check _2 inputs first
    current.species.x.edit <- if (!is.null(input$PI_log_comp_species_x_edit_2)) input$PI_log_comp_species_x_edit_2 
                              else if (!is.null(input$PI_log_comp_species_x_edit)) input$PI_log_comp_species_x_edit 
                              else NULL
    current.species.y.edit <- if (!is.null(input$PI_log_comp_species_y_edit_2)) input$PI_log_comp_species_y_edit_2 
                              else if (!is.null(input$PI_log_comp_species_y_edit)) input$PI_log_comp_species_y_edit 
                              else NULL
  } else {
    # Both species mode - check regular inputs first
    current.species.x.edit <- if (!is.null(input$PI_log_comp_species_x_edit)) input$PI_log_comp_species_x_edit 
                              else if (!is.null(input$PI_log_comp_species_x_edit_2)) input$PI_log_comp_species_x_edit_2 
                              else NULL
    current.species.y.edit <- if (!is.null(input$PI_log_comp_species_y_edit)) input$PI_log_comp_species_y_edit 
                              else if (!is.null(input$PI_log_comp_species_y_edit_2)) input$PI_log_comp_species_y_edit_2 
                              else NULL
  }
  
  div(
    conditionalPanel(
      condition = "!input.CB_log_comp_single_species_edit",
      # Both species compete (default)
      fluidRow(
        column(
          width = 4,
          pickerInput(
            inputId = "PI_log_comp_species_x_edit",
            label   = "Species X",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.x.edit,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        ),
        column(
          width = 4,
          pickerInput(
            inputId = "PI_log_comp_species_y_edit",
            label   = "Species Y",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.y.edit,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          textInput("TI_log_comp_r_x_edit", "r_x", value = "r_x")
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_r_x_value_edit", "Value", value = 0.7, min = 0, step = 0.01)
        ),
        column(
          width = 3,
          textInput("TI_log_comp_r_y_edit", "r_y", value = "r_y")
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_r_y_value_edit", "Value", value = 0.7, min = 0, step = 0.01)
        )
      ),
      fluidRow(
        column(
          width = 3,
          textInput("TI_log_comp_alpha_xy_edit", "alpha_xy", value = "alpha_xy")
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_alpha_xy_value_edit", "Value", value = 0.1, min = 0, step = 0.01)
        ),
        column(
          width = 3,
          textInput("TI_log_comp_alpha_yx_edit", "alpha_yx", value = "alpha_yx")
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_alpha_yx_value_edit", "Value", value = 0.1, min = 0, step = 0.01)
        )
      ),
      fluidRow(
        column(
          width = 3,
          textInput("TI_log_comp_Kc_edit", "Kc (carrying capacity)", value = "Kc")
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_Kc_value_edit", "Value", value = 1, min = 0.0001, step = 0.1)
        )
      )
    ),
    conditionalPanel(
      condition = "input.CB_log_comp_single_species_edit",
      # Only species X grows competitively, Y is just a competitor
      fluidRow(
        column(
          width = 4,
          pickerInput(
            inputId = "PI_log_comp_species_x_edit_2",
            label   = "Species X (growing competitively)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.x.edit,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        ),
        column(
          width = 4,
          pickerInput(
            inputId = "PI_log_comp_species_y_edit_2",
            label   = "Species Y (competitor only)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = current.species.y.edit,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          textInput("TI_log_comp_r_x_edit", "r_x", value = "r_x")
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_r_x_value_edit", "Value", value = 0.7, min = 0, step = 0.01)
        ),
        column(
          width = 3,
          textInput("TI_log_comp_alpha_xy_edit", "alpha_xy", value = "alpha_xy")
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_alpha_xy_value_edit", "Value", value = 0.1, min = 0, step = 0.01)
        )
      ),
      fluidRow(
        column(
          width = 3,
          textInput("TI_log_comp_Kc_edit", "Kc (carrying capacity)", value = "Kc")
        ),
        column(
          width = 3,
          numericInput("NI_log_comp_Kc_value_edit", "Value", value = 1, min = 0.0001, step = 0.1)
        )
      )
    )
  )
})

output$eqnCreate_equationBuilder_time_equation <- renderUI({
  
  div(
    "This is a custom equation entering. Please make sure things are spelled correctly as errors will occur if these equations are improper.",
    "There is a box below to add any parameters that could be used in your time dependent equations.",
    "Please note that 't' is used for time",
    hr(),
    fluidRow(
      column(
        width = 4,
        textInput(inputId = "eqnCreate_time_dependent_parameters",
                  label = "Parameters to add",
                  value = "")
        ),
      column(
        width = 4,
        align = "left",
        div(style = "padding-top: 30px;",
            actionButton(
              inputId = "eqnCreate_time_dependent_store_new_parameter",
              label = "Store Parameter"))
        
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 4,
        textInput(
          inputId = "eqnCreate_time_dependent_firstvar",
          label = "Time Dependent Variable",
          value = ""
        )
      ),
      column(width = 1,
             div(style = "padding-top: 38px; padding-left: 12px",
                 "=")
             ),
      column(
        width = 7,
        textInput(
          inputId = "eqnCreate_time_dependent_equation",
          label = "Equation",
          value = ""
        )
      )
    )
  )
})

output$equationBuilder_substrate_synthesis_competition <- renderUI({
  # Count existing substrate synthesis competition reactions to generate unique parameter names
  n.existing <- length(rv.REACTIONS$substrateSynthesisCompetition)
  param.suffix <- if (n.existing > 0) paste0("_", n.existing + 1) else ""
  
  div(
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId = "PI_sub_syn_comp_species",
          label   = "Species (X)",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      ),
      column(
        width = 4,
        pickerInput(
          inputId = "PI_sub_syn_comp_substrate",
          label   = "Substrate (S)",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      ),
      column(
        width = 4,
        pickerInput(
          inputId = "PI_sub_syn_comp_competitor",
          label   = "Competitor (Y) - Optional",
          choices = c("None" = "", sort(rv.SPECIES$df.by.compartment$Name)),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      )
    ),
    hr(),
    prettyCheckbox(
      inputId = "CB_sub_syn_comp_species_dependent",
      label = "Species-dependent synthesis (rate = k*S*X*(1-(X+alpha*Y)/Kc))",
      value = TRUE
    ),
    hr(),
    fluidRow(
      column(
        width = 3,
        textInput("TI_sub_syn_comp_k", "k (rate constant)", value = paste0("k_sub_syn", param.suffix))
      ),
      column(
        width = 3,
        numericInput("NI_sub_syn_comp_k_value", "Value", value = 0.1, min = 0, step = 0.01)
      ),
      column(
        width = 3,
        textInput("TI_sub_syn_comp_alpha", "alpha (competition coefficient)", value = paste0("alpha", param.suffix))
      ),
      column(
        width = 3,
        numericInput("NI_sub_syn_comp_alpha_value", "Value", value = 0.1, min = 0, step = 0.01)
      )
    ),
    fluidRow(
      column(
        width = 3,
        textInput("TI_sub_syn_comp_Kc", "Kc (carrying capacity)", value = paste0("Kc", param.suffix))
      ),
      column(
        width = 3,
        numericInput("NI_sub_syn_comp_Kc_value", "Value", value = 1, min = 0.0001, step = 0.1)
      )
    )
  )
})

output$equationBuilder_substrate_synthesis_competition_edit <- renderUI({
  # Get current reaction info
  eqn.id <- input$modal_editEqn_reaction_id
  if (is.null(eqn.id) || !eqn.id %in% names(rv.REACTIONS$substrateSynthesisCompetition)) {
    return(div("Error: Reaction not found"))
  }
  
  ssc.info <- rv.REACTIONS$substrateSynthesisCompetition[[eqn.id]]
  
  div(
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId = "PI_sub_syn_comp_species_edit",
          label   = "Species (X)",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          selected = ssc.info$Species,
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      ),
      column(
        width = 4,
        pickerInput(
          inputId = "PI_sub_syn_comp_substrate_edit",
          label   = "Substrate (S)",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          selected = ssc.info$Substrate,
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      ),
      column(
        width = 4,
        pickerInput(
          inputId = "PI_sub_syn_comp_competitor_edit",
          label   = "Competitor (Y) - Optional",
          choices = c("None" = "", sort(rv.SPECIES$df.by.compartment$Name)),
          selected = if ("Competitor" %in% names(ssc.info) && !is.na(ssc.info$Competitor)) ssc.info$Competitor else "",
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      )
    ),
    hr(),
    prettyCheckbox(
      inputId = "CB_sub_syn_comp_species_dependent_edit",
      label = "Species-dependent synthesis (rate = k*S*X*(1-(X+alpha*Y)/Kc))",
      value = if ("Species.Dependent" %in% names(ssc.info)) ssc.info$Species.Dependent else TRUE
    ),
    hr(),
    fluidRow(
      column(
        width = 3,
        textInput("TI_sub_syn_comp_k_edit", "k (rate constant)", value = ssc.info$k)
      ),
      column(
        width = 3,
        numericInput("NI_sub_syn_comp_k_value_edit", "Value", value = ssc.info$k.val, min = 0, step = 0.01)
      ),
      column(
        width = 3,
        textInput("TI_sub_syn_comp_alpha_edit", "alpha (competition coefficient)", value = ssc.info$alpha)
      ),
      column(
        width = 3,
        numericInput("NI_sub_syn_comp_alpha_value_edit", "Value", value = ssc.info$alpha.val, min = 0, step = 0.01)
      )
    ),
    fluidRow(
      column(
        width = 3,
        textInput("TI_sub_syn_comp_Kc_edit", "Kc (carrying capacity)", value = ssc.info$Kc)
      ),
      column(
        width = 3,
        numericInput("NI_sub_syn_comp_Kc_value_edit", "Value", value = ssc.info$Kc.val, min = 0.0001, step = 0.1)
      )
    )
  )
})

output$equationBuilder_predator_prey <- renderUI({
  # Count existing predator–prey reactions to generate unique parameter names
  n.existing <- length(rv.REACTIONS$predatorPrey)
  param.suffix <- if (n.existing > 0) paste0("_", n.existing + 1) else ""
  
  div(
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId = "PI_pred_prey_prey",
          label   = "Prey (X)",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      ),
      column(
        width = 4,
        pickerInput(
          inputId = "PI_pred_prey_predator",
          label   = "Predator (Y)",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 3,
        textInput("TI_pred_prey_r", "r (prey growth rate)", value = paste0("r", param.suffix))
      ),
      column(
        width = 3,
        numericInput("NI_pred_prey_r_value", "Value", value = 0.7, min = 0, step = 0.01)
      ),
      column(
        width = 3,
        textInput("TI_pred_prey_a", "a (attack rate in dX/dt)", value = paste0("a", param.suffix))
      ),
      column(
        width = 3,
        numericInput("NI_pred_prey_a_value", "Value", value = 0.01, min = 0, step = 0.0001)
      )
    ),
    fluidRow(
      column(
        width = 3,
        textInput("TI_pred_prey_b", "b (conversion rate in dY/dt)", value = paste0("b", param.suffix))
      ),
      column(
        width = 3,
        numericInput("NI_pred_prey_b_value", "Value", value = 0.01, min = 0, step = 0.0001)
      ),
      column(
        width = 3,
        textInput("TI_pred_prey_d", "d (predator death rate)", value = paste0("d", param.suffix))
      ),
      column(
        width = 3,
        numericInput("NI_pred_prey_d_value", "Value", value = 0.5, min = 0, step = 0.01)
      )
    )
  )
})

output$equationBuilder_predator_prey_edit <- renderUI({
  eqn.id <- input$modal_editEqn_reaction_id
  if (is.null(eqn.id) || !eqn.id %in% names(rv.REACTIONS$predatorPrey)) {
    return(div("Error: Predator–prey reaction not found"))
  }
  
  pp.info <- rv.REACTIONS$predatorPrey[[eqn.id]]
  
  div(
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId = "PI_pred_prey_prey_edit",
          label   = "Prey (X)",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          selected = pp.info$Prey,
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      ),
      column(
        width = 4,
        pickerInput(
          inputId = "PI_pred_prey_predator_edit",
          label   = "Predator (Y)",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          selected = pp.info$Predator,
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 3,
        textInput("TI_pred_prey_r_edit", "r (prey growth rate)", value = pp.info$r)
      ),
      column(
        width = 3,
        numericInput("NI_pred_prey_r_value_edit", "Value", value = pp.info$r.val, min = 0, step = 0.01)
      ),
      column(
        width = 3,
        textInput("TI_pred_prey_a_edit", "a (attack rate in dX/dt)", value = pp.info$a)
      ),
      column(
        width = 3,
        numericInput("NI_pred_prey_a_value_edit", "Value", value = pp.info$a.val, min = 0, step = 0.0001)
      )
    ),
    fluidRow(
      column(
        width = 3,
        textInput("TI_pred_prey_b_edit", "b (conversion rate in dY/dt)", value = pp.info$b)
      ),
      column(
        width = 3,
        numericInput("NI_pred_prey_b_value_edit", "Value", value = pp.info$b.val, min = 0, step = 0.0001)
      ),
      column(
        width = 3,
        textInput("TI_pred_prey_d_edit", "d (predator death rate)", value = pp.info$d)
      ),
      column(
        width = 3,
        numericInput("NI_pred_prey_d_value_edit", "Value", value = pp.info$d.val, min = 0, step = 0.01)
      )
    )
  )
})



