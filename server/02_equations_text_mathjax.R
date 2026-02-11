

equationMathJaxBuilder <- reactive({
  
  if (!isTruthy(input$eqnCreate_reaction_law)) {
    return("Reaction Law Not Loaded")
  }
  
  if (input$eqnCreate_reaction_law == "mass_action") {
    number.reactants <- as.numeric(input$NI_mass_action_num_reactants)
    number.products  <- as.numeric(input$NI_mass_action_num_products)

    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MA_r_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_reactant_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }

      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var))
      } else {
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var), " + ")
      }
    }
    
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MA_p_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_product_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var))
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var), " + ")
      }
    }
    
    if (input$PI_mass_action_reverisble_option == "both_directions") {
      arrow <- "<->"

      arrow <- paste0("\\ce{",
                      arrow, 
                      "[{", 
                      Var2MathJ(input$TI_mass_action_forward_k),
                      "\\text{, }",
                      Var2MathJ(input$TI_mass_action_reverse_k), 
                      "}]", 
                      "}")
    }
    else if (input$PI_mass_action_reverisble_option == "forward_only") {
      arrow = "->"

      arrow <- paste0("\\ce{",
                      arrow, 
                      "[{", 
                      Var2MathJ(input$TI_mass_action_forward_k), 
                      "}]",
                      "}")
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (input$eqnCreate_reaction_law == "mass_action_w_reg") {
    arrow <- "->"
    
    number.reactants <- as.numeric(input$NI_mass_action_wReg_num_reactants)
    number.products  <- as.numeric(input$NI_mass_action_wReg_num_products)
    
    has.f.reg <- input$CB_MAwR_chem_modifier_forward
    has.r.reg <- input$CB_MAwR_chem_modifier_reverse
    
    number_forward_regulators = as.numeric(input$NI_MAwR_n_forward_regulators)
    number_reverse_regulators = as.numeric(input$NI_MAwR_n_reverse_regulators)
    
    reversible <- input$reaction_mass_action_wReg_reverisble
    
    # Build Reactant Equation Side
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_r_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_reactant_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var))
      } else {
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var), " + ")
      }
    }
    
    # Build Product Equation Side
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_p_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_product_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var))
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var), " + ")
      }
    }
    
    modifiers <- c()
    parameters <- c()
    # Check For Forward Regulators
    if (has.f.reg) {
      #find regulators and add them together in form ([regulator/constant, 
      #regulator2/constant2, etc...])
      forwardModifiers <- c()
      for (i in seq(number_forward_regulators)) {
        regulator <-
          eval(parse(text = paste0(
            "input$PI_MAwR_forward_regulator_", as.character(i)
          )))
        rateConstant <-
          eval(parse(text = paste0(
            "input$TI_MAwR_forward_regulator_RC_", as.character(i)
          )))
        modifiers <- c(modifiers, Var2MathJ(regulator))
        parameters <- c(parameters, Var2MathJ(rateConstant))
      }
    } 
    else {
      # If no forward regulators, use kf
      parameters <- c(parameters, Var2MathJ(input$TI_MAwR_forward_k))
    }
    # Check If Reaction Is Reversible
    if (reversible == "both_directions") {
      arrow <- "<->"
      # Check if Reverse Regulator is used
      if (has.r.reg) {
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$PI_MAwR_reverse_regulator_", as.character(i)
            )))
          rateConstant <-
            eval(parse(text = paste0(
              "input$TI_MAwR_reverse_regulator_RC_", as.character(i)
            )))
          modifiers <- c(modifiers, Var2MathJ(regulator))
          parameters <- c(parameters, Var2MathJ(rateConstant))
        }
      }
      else {
        # If no regulators, use kr
        parameters <- c(parameters, Var2MathJ(input$TI_MAwR_reverse_k))
      }
    } 
    
    if (length(modifiers) > 0) {
      parameter.exp <- paste0(parameters, collapse = "\\text{, }")
      modifiers.exp <- paste0(modifiers, collapse = "\\text{, }")
      arrow <- 
        paste0(
          "\\ce{",
          arrow,
          "[{", parameter.exp, "}]",
          "[{", modifiers.exp, "}]",
          "}"
        )
    } else {
      parameter.exp <- paste0(parameters, collapse = "\\text{, }")
      arrow <- 
        paste0(
          "\\ce{",
          arrow,
          "[{", parameter.exp, "}]",
          "}"
        )
    }

    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
    
  }
  else if (input$eqnCreate_reaction_law == "synthesis") {
    
    if (input$CB_synthesis_factor_checkbox) {
      arrow  <- "->"
      var    <- Var2MathJ(input$PI_synthesis_byFactor_var)
      rc     <- Var2MathJ(input$TI_synthesis_byFactor_RC)
      factor <- Var2MathJ(input$PI_synthesis_byFactor_factor)
      type   <- "syn"
      textOut <- paste0("\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "[{", factor, "}]",
                        "}",
                        var
      )
    } else {
      arrow <- "->"
      var   <- Var2MathJ(input$PI_synthesis_rate_var)
      rc    <- Var2MathJ(input$TI_synthesis_rate_RC)
      type  <- "syn"
      textOut <- paste0("\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "}",
                        var
      )
    }
  } 
  else if (input$eqnCreate_reaction_law == "degradation_rate") {
    # Get products if they exist
    if (input$CB_degradation_rate_toProducts) {
      num.deg.products <- as.numeric(input$NI_degradation_rate_num_products)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$PI_degradation_rate_product_", 
                                         as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2MathJ(prod))
        } else {
          product <- paste0(product, Var2MathJ(prod), " + ")
        }
      }
    } else {
      product <- "\\bigotimes"
    }
    
    # Build Equations
    arrow <- "->"
    var   <- Var2MathJ(input$PI_degradation_rate_species)
    rc    <- Var2MathJ(input$TI_degradation_rate_RC)
    type  <- "deg"
    textOut <- paste0(var,
                      "\\ce{",
                      arrow,
                      "[{", rc, "}]",
                      "}",
                      product
    )
  }
  else if (input$eqnCreate_reaction_law == "degradation_by_enzyme") {
    # Get products if they exist
    if (input$CB_degradation_enzyme_toProducts) {
      num.deg.products <- as.numeric(input$NI_degradation_enzyme_num_products)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$PI_degradation_enzyme_product_", 
                                         as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2MathJ(prod))
        } else {
          product <- paste0(product, Var2MathJ(prod), " + ")
        }
      }
    } else {
      product <- "\\bigotimes"
    }
    
    # Build Equations
    arrow <- "->"
    var   <- Var2MathJ(input$PI_degradation_enzyme_species)
    Km    <- Var2MathJ(input$TI_degradation_enzyme_Km)
    type  <- "deg"
    
    if (input$CB_degradation_enzyme_useVmax) {
      Vmax <- Var2MathJ(input$TI_degradation_enzyme_Vmax)
      textOut <- paste0(var,
                        "\\ce{",
                        arrow,
                        "[{", Km, ",\\ ", Vmax, "}]",
                        "}",
                        product
      )
    } else {
      enz  <- Var2MathJ(input$PI_degradation_enzyme_enzyme)
      kcat <- Var2MathJ(input$TI_degradation_enzyme_kcat)
      textOut <- paste0(var,
                        "\\ce{",
                        arrow,
                        "[{", Km, ",\\ ", kcat, "}]",
                        "[{", enz, "}]",
                        "}",
                        product
      )
    }
  }
  else if (input$eqnCreate_reaction_law == "michaelis_menten") {
    substrate <- Var2MathJ(input$PI_michaelis_menten_substrate)
    product   <- Var2MathJ(input$PI_michaelis_menten_product)
    arrow     <- "->"
    enzyme    <- Var2MathJ(input$PI_michaelis_menten_enzyme)
    Km        <- Var2MathJ(input$TI_michaelis_menten_Km)
    
    if (!input$CB_michaelis_menten_useVmax) {
      kcat    <- Var2MathJ(input$TI_michaelis_menten_kcat)
      textOut <- paste0(substrate,
                        "\\ce{",
                        arrow,
                        "[{", Km , ",\\ ", kcat, "}]",
                        "[{", enzyme, "}]",
                        "}",
                        product)
    }
    else if (input$CB_michaelis_menten_useVmax) {
      Vmax = Var2MathJ(input$TI_michaelis_menten_vmax)
      textOut <- paste0(substrate, 
                        "\\ce{",
                        arrow,
                        "[{", Km, ",\\ ", Vmax, "}]",
                        "}",
                        product
      )
    }
  }
  else if (input$eqnCreate_reaction_law == "create_custom") {
    textOut <- ""
    
    # Grab Equation Information
    reactants <- ""
    products  <- ""
    modifiers <- ""
    parameters <- ""
    
    tryCatch(
      expr = {
        reactants <- trimws(strsplit(input$PI_CC_reactants, ",")[[1]], 
                            which = "both")
        products  <- trimws(strsplit(input$PI_CC_products,  ",")[[1]], 
                            which = "both")
        modifiers <- trimws(strsplit(input$PI_CC_modifiers, ",")[[1]], 
                            which = "both")
      }, 
      error = function(e) {
        reactants <- ""
        products  <- ""
        modifiers <- ""
      },
      warning = function(e) {
        reactants <- ""
        products  <- ""
        modifiers <- ""
      }
    )
    
    tryCatch( 
      expr = {
        par.table <- hot_to_r(input$TO_CC_parameter_table)
        parameters <- par.table %>% pull(Variables)
      }, 
      error = function(e) {
        parameters <- ""
      },
      warning = function(e) {
        parameters <- ""
      }
    )
    # Build Eqn
    eqn.builds <- BuildCustomEquationText(reactants,
                                          products,
                                          modifiers,
                                          parameters)
    
    textOut <- eqn.builds$mathjax
  }
  else if (startsWith(input$eqnCreate_reaction_law, "user_custom_law_")) {
    # Custom Made Equation Case
    
    # Find the custom law that is being used
    backend.name <- input$eqnCreate_reaction_law
    custom.id    <- strsplit(backend.name, "_")[[1]][4]
    
    # Find the reaction entry of this id
    law.entry <- rv.CUSTOM.LAWS$cl.reaction[[custom.id]]
    
    has.reactants  <- FALSE
    has.products   <- FALSE
    has.modifiers  <- FALSE
    has.parameters <- FALSE
    
    # Unpack reaction information
    eqn.reactants  <- law.entry$Reactants
    eqn.products   <- law.entry$Products
    eqn.modifiers  <- law.entry$Modifiers
    eqn.parameters <- law.entry$Parameters
    
    # Process specie information
    if (isTruthy(eqn.reactants)) {
      eqn.reactants <- strsplit(eqn.reactants, ", ")[[1]]
      n.reactants   <- length(eqn.reactants)
      has.reactants <- TRUE
    }
    
    if (isTruthy(eqn.products)) {
      eqn.products <- strsplit(eqn.products, ", ")[[1]]
      n.products   <- length(eqn.products)
      has.products <- TRUE
    }
    
    if (isTruthy(eqn.parameters)) {
      eqn.parameters  <- strsplit(eqn.parameters, ", ")[[1]]
      n.parameters    <- length(eqn.parameters)
      has.parameters  <- TRUE
    }
    
    if (isTruthy(eqn.modifiers)) {
      eqn.modifiers <- strsplit(eqn.modifiers, ", ")[[1]]
      n.modifiers   <- length(eqn.modifiers)
      has.modifiers <- TRUE
    }
    
    # FIND RENDERED UI VALUES
    reactants  <- ""
    products   <- ""
    modifiers  <- ""
    parameters <- ""
    
    if (has.reactants) {
      reactants <- c()
      for (i in seq(n.reactants)) {
        reactants <- c(reactants, 
                       eval(parse(text = paste0("input$PI_CL_reactant_", 
                                                as.character(i)))))
      }
    } 
    
    if (has.products) {
      products <- c()
      for (i in seq(n.products)) {
        products <- c(products, 
                       eval(parse(text = paste0("input$PI_CL_product_", 
                                                as.character(i)))))
      }
    } 
    
    if (has.modifiers) {
      modifiers <- c()
      for (i in seq(n.modifiers)) {
        modifiers <- c(modifiers, 
                      eval(parse(text = paste0("input$PI_CL_modifier_", 
                                               as.character(i)))))
      }
    }
    
    if (has.parameters) {
      parameters <- c()
      for (i in seq(n.parameters)) {
        parameters <- c(parameters, 
                       eval(parse(text = paste0("input$PI_CL_parameter_", 
                                                as.character(i)))))
      }
    }
    
    # Build Eqn
    eqn.builds <- BuildCustomEquationText(reactants,
                                          products,
                                          modifiers,
                                          parameters)
    
    textOut <- eqn.builds$mathjax
  }
  else if (input$eqnCreate_type_of_equation == "rate_eqn") {
    rate_left <- input$eqnCreate_custom_eqn_lhs
    rate_right <- input$eqnCreate_custom_eqn_rhs
    textOut <- paste0(rate_left, " = ", rate_right)
  }
  else if (input$eqnCreate_type_of_equation == "time_dependent") {
    TD_left <- input$eqnCreate_time_dependent_firstvar
    TD_right <- input$eqnCreate_time_dependent_equation
    textOut <- paste0(TD_left, "=", TD_right)
  }
  else{textOut <- "ERROR"}
  textOut <- paste0("$$", textOut, "$$")
  return(textOut)
})

equationLatexBuilder <- reactive({
  
  if (input$eqnCreate_reaction_law == "mass_action") {
    number.reactants <- as.numeric(input$NI_mass_action_num_reactants)
    number.products  <- as.numeric(input$NI_mass_action_num_products)
    
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MA_r_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_reactant_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, Var2Latex(var))
      } else {
        eqn_LHS <- paste0(eqn_LHS, Var2Latex(var), " + ")
      }
    }
    
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MA_p_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_product_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, Var2Latex(var))
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, Var2Latex(var), " + ")
      }
    }
    
    if (input$PI_mass_action_reverisble_option == "both_directions") {
      arrow <- "\\xrightleftharpoons"
      params <- c(Var2Latex(input$TI_mass_action_forward_k),
                  Var2Latex(input$TI_mass_action_reverse_k))
      params <- paste0(params, collapse = "\\text{, }")
      arrow <- paste0(arrow, 
                      "{", 
                      params, 
                      "}")
    }
    else if (input$PI_mass_action_reverisble_option == "forward_only") {
      arrow <- "\\xrightarrow"
      
      arrow <- paste0(arrow, 
                      "{", 
                      Var2Latex(input$TI_mass_action_forward_k), 
                      "}")
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  
  }
  else if (input$eqnCreate_reaction_law == "mass_action_w_reg") {
    arrow <- "\\xrightarrow"
    
    number.reactants <- as.numeric(input$NI_mass_action_wReg_num_reactants)
    number.products  <- as.numeric(input$NI_mass_action_wReg_num_products)
    
    has.f.reg <- input$CB_MAwR_chem_modifier_forward
    has.r.reg <- input$CB_MAwR_chem_modifier_reverse
    
    number_forward_regulators = as.numeric(input$NI_MAwR_n_forward_regulators)
    number_reverse_regulators = as.numeric(input$NI_MAwR_n_reverse_regulators)
    
    reversible <- input$reaction_mass_action_wReg_reverisble
    
    # Build Reactant Equation Side
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_r_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_reactant_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, Var2Latex(var))
      } else {
        eqn_LHS <- paste0(eqn_LHS, Var2Latex(var), " + ")
      }
    }
    
    # Build Product Equation Side
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_p_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_product_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, Var2Latex(var))
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, Var2Latex(var), " + ")
      }
    }
    
    parameters <- c()
    modifiers  <- c()
    # Check For Forward Regulators
    if (has.f.reg) {
      #find regulators and add them together in form ([regulator/constant, 
      #regulator2/constant2, etc...])
      for (i in seq(number_forward_regulators)) {
        regulator <-
          eval(parse(text = paste0(
            "input$PI_MAwR_forward_regulator_", as.character(i)
          )))
        rateConstant <-
          eval(parse(text = paste0(
            "input$TI_MAwR_forward_regulator_RC_", as.character(i)
          )))
        modifiers  <- c(modifiers, Var2Latex(regulator))
        parameters <- c(parameters, Var2Latex(rateConstant))
      }
    } 
    else {
      # If no forward regulators, use kf
      parameters <- c(parameters, Var2Latex(input$TI_MAwR_forward_k))
    }

    # Check If Reaction Is Reversible
    if (reversible == "both_directions") {
      arrow <- "\\xrightleftharpoons"
      # Check if Reverse Regulator is used
      if (has.r.reg) {
        for (i in seq(number_reverse_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$PI_MAwR_reverse_regulator_", as.character(i)
            )))
          rateConstant <-
            eval(parse(text = paste0(
              "input$TI_MAwR_reverse_regulator_RC_", as.character(i)
            )))
          modifiers  <- c(modifiers, Var2Latex(regulator))
          parameters <- c(parameters, Var2Latex(rateConstant))
        }
      }
      else {
        # If no regulators, use kr
        parameters <- c(parameters, Var2Latex(input$TI_MAwR_reverse_k))
      }
    } 
    if (length(modifiers) > 0) {
      parameter.exp <- paste0(parameters, collapse = "\\text{, }")
      modifiers.exp <- paste0(modifiers, collapse = "\\text{, }")
      arrow <- 
        paste0(
          arrow,
          "[", modifiers.exp, "]",
          "{", parameter.exp, "}"
        )
    } else {
      parameter.exp <- paste0(parameters, collapse = "\\text{, }")
      arrow <- 
        paste0(
          arrow,
          "{", parameter.exp, "}"
        )
    }
    
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (input$eqnCreate_reaction_law == "synthesis") {
    if (input$CB_synthesis_factor_checkbox) {
      arrow  <- "\\xrightarrow"
      var    <- Var2Latex(input$PI_synthesis_byFactor_var)
      rc     <- Var2Latex(input$TI_synthesis_byFactor_RC)
      factor <- Var2Latex(input$PI_synthesis_byFactor_factor)
      type   <- "syn"
      textOut <- paste0(arrow,
                        "[", factor, "]",
                        "{", rc, "}",
                        var
      )
    } else {
      arrow  <- "\\xrightarrow"
      var   <- Var2Latex(input$PI_synthesis_rate_var)
      rc    <- Var2Latex(input$TI_synthesis_rate_RC)
      type  <- "syn"
      textOut <- paste0(arrow,
                        "{", rc, "}",
                        var
      )
    }
  } 
  else if (input$eqnCreate_reaction_law == "degradation_rate") {
    # Get products if they exist
    if (input$CB_degradation_rate_toProducts) {
      num.deg.products <- as.numeric(input$NI_degradation_rate_num_products)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$PI_degradation_rate_product_", 
                                         as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2Latex(prod))
        } else {
          product <- paste0(product, Var2Latex(prod), " + ")
        }
      }
    } else {
      product <- "\\bigotimes"
    }
    
    # Build Equations
    arrow  <- "\\xrightarrow"
    var   <- Var2Latex(input$PI_degradation_rate_species)
    rc    <- Var2Latex(input$TI_degradation_rate_RC)
    type  <- "deg"
    textOut <- paste0(var,
                      arrow,
                      "{", rc, "}",
                      product
    )
  } 
  else if (input$eqnCreate_reaction_law == "degradation_by_enzyme") {
    # Get products if they exist
    if (input$CB_degradation_enzyme_toProducts) {
      num.deg.products <- as.numeric(input$NI_degradation_enzyme_num_products)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$PI_degradation_enzyme_product_", 
                                         as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2Latex(prod))
        } else {
          product <- paste0(product, Var2Latex(prod), " + ")
        }
      }
    } else {
      product <- "\\bigotimes"
    }
    
    # Build Equations
    arrow  <- "\\xrightarrow"
    var   <- Var2Latex(input$PI_degradation_enzyme_species)
    Km    <- Var2Latex(input$TI_degradation_enzyme_Km)
    type  <- "deg"
    
    if (input$CB_degradation_enzyme_useVmax) {
      Vmax <- Var2Latex(input$TI_degradation_enzyme_Vmax)
      textOut <- paste0(var,
                        arrow,
                        "{", Km, "\\text{, }", Vmax, "}",
                        product
      )
    } else {
      enz  <- Var2Latex(input$PI_degradation_enzyme_enzyme)
      kcat <- Var2Latex(input$TI_degradation_enzyme_kcat)
      textOut <- paste0(var,
                        arrow,
                        "[", enz, "]",
                        "{", Km, "\\text{, }", kcat, "}",
                        product
      )
    }
  } 
  else if (input$eqnCreate_reaction_law == "michaelis_menten") {
    substrate <- Var2Latex(input$PI_michaelis_menten_substrate)
    product   <- Var2Latex(input$PI_michaelis_menten_product)
    arrow     <- "\\xrightarrow"
    enzyme    <- Var2Latex(input$PI_michaelis_menten_enzyme)
    Km        <- Var2Latex(input$TI_michaelis_menten_Km)
    
    if (!input$CB_michaelis_menten_useVmax) {
      kcat    <- Var2Latex(input$TI_michaelis_menten_kcat)
      textOut <- paste0(substrate,
                        arrow,
                        "[", enzyme, "]",
                        "{", Km, "\\text{, }", kcat, "} ",
                        product)
    }
    else if (input$CB_michaelis_menten_useVmax) {
      Vmax <- Var2Latex(input$TI_michaelis_menten_vmax)
      textOut <- paste0(substrate, 
                        arrow,
                        "{", Vmax, "\\text{, }", Km, "}",
                        product
      )
    }
  }
  else {
    textOut <- "ERROR"
  }
 
  return(textOut)
})

# Build Text Equation for UI viewer --------------------------------------------
equationBuilder <- reactive({
  
  if (input$eqnCreate_reaction_law == "mass_action") {
    number.reactants <- as.numeric(input$NI_mass_action_num_reactants)
    number.products  <- as.numeric(input$NI_mass_action_num_products)
    
    kf <- input$TI_mass_action_forward_k
    kr <- input$TI_mass_action_reverse_k
    
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MA_r_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_reactant_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, var)
      } else {
        eqn_LHS <- paste0(eqn_LHS, var, " + ")
      }
    }
    
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MA_p_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_product_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, var)
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, var, " + ")
      }
    }
    
    if (input$PI_mass_action_reverisble_option == "both_directions") {
      arrow <- paste0("<->",
                      "(", kf, ", ", kr, ") "
      )
    } else if (input$PI_mass_action_reverisble_option == "forward_only") {
      arrow <- paste0("->",
                      "(", kf, ") "
      )
      
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  } 
  else if (input$eqnCreate_reaction_law == "mass_action_w_reg") {
    arrow <- "->"
    
    number.reactants <- as.numeric(input$NI_mass_action_wReg_num_reactants)
    number.products  <- as.numeric(input$NI_mass_action_wReg_num_products)
    
    has.f.reg <- input$CB_MAwR_chem_modifier_forward
    has.r.reg <- input$CB_MAwR_chem_modifier_reverse
    
    number_forward_regulators = as.numeric(input$NI_MAwR_n_forward_regulators)
    number_reverse_regulators = as.numeric(input$NI_MAwR_n_reverse_regulators)
    
    reversible <- input$reaction_mass_action_wReg_reverisble
    
    # Build Reactant Equation Side
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_r_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_reactant_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, var)
      } else {
        eqn_LHS <- paste0(eqn_LHS, var, " + ")
      }
    }
    
    # Build Product Equation Side
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_p_stoichiometry_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_product_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, var)
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, var, " + ")
      }
    }
    
    # Check For Forward Regulators
    parameters <- c()
    modifiers  <- c()
    if (has.f.reg) {
      #find regulators and add them together in form ([regulator/constant, 
      #regulator2/constant2, etc...])
      forwardModifiers <- c()
      for (i in seq(number_forward_regulators)) {
        regulator <-
          eval(parse(text = paste0(
            "input$PI_MAwR_forward_regulator_", as.character(i)
          )))
        rateConstant <-
          eval(parse(text = paste0(
            "input$TI_MAwR_forward_regulator_RC_", as.character(i)
          )))
        modifiers <- c(modifiers, regulator)
        parameters <- c(parameters, rateConstant)
        # modifierExpression <- paste0(regulator,
        #                              ":",
        #                              rateConstant)
        # forwardModifiers <- c(forwardModifiers, modifierExpression)
      }
      # forwardModifiers <- paste(forwardModifiers, collapse = ", ")
    } 
    else {
      # If no forward regulators, use kf
      parameters <- c(parameters, input$TI_MAwR_forward_k)
      # forwardModifiers <- input$TI_MAwR_forward_k
    }
    # forwardModifiers <- paste0("[",
    #                            forwardModifiers,
    #                            "]")
    # Check If Reaction Is Reversible
    if (reversible == "both_directions") {
      arrow <- "<->"
      # Check if Reverse Regulator is used
      if (has.r.reg) {
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$PI_MAwR_reverse_regulator_", as.character(i)
            )))
          rateConstant <-
            eval(parse(text = paste0(
              "input$TI_MAwR_reverse_regulator_RC_", as.character(i)
            )))
          
          modifiers <- c(modifiers, regulator)
          parameters <- c(parameters, rateConstant)
          
          # modifierExpression <- paste0(regulator,
          #                              ":",
          #                              rateConstant)
          # reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        # reverseModifiers <- paste(reverseModifiers, collapse = ", ")
      }
      else {
        # If no regulators, use kr
        parameters <- c(parameters, input$TI_MAwR_reverse_k)
      }
      # reverseModifiers <- paste0("[", 
      #                            reverseModifiers, 
      #                            "]")
    } 
    else {
      # reverseModifiers <- ""
    }
    
    if (length(modifiers) > 0) {
      arrow <- paste0(" [", paste0(modifiers, collapse =", "), "]",
                      arrow,
                      "(", paste0(parameters, collapse = ", "), ") ")
    } else {
      arrow <- paste0(arrow,
                      "(", paste0(parameters, collapse = ", "), ") ")
    }
    
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (input$eqnCreate_reaction_law == "synthesis") {
    if (input$CB_synthesis_factor_checkbox) {
      arrow  <- "-->"
      var    <- input$PI_synthesis_byFactor_var
      rc     <- input$TI_synthesis_byFactor_RC
      factor <- input$PI_synthesis_byFactor_factor
      textOut <- paste0(factor,
                        " ",
                        arrow,
                        "(", rc, ")",
                        " ",
                        var
      )
    } else {
      arrow <- "-->"
      var   <- input$PI_synthesis_rate_var
      rc    <- input$TI_synthesis_rate_RC
      textOut <- paste0(arrow,
                        "(", rc, ")",
                        " ",
                        var
      )
    }
  }
  else if (input$eqnCreate_reaction_law == "degradation_rate") {
    num.deg.products <- as.numeric(input$NI_degradation_rate_num_products)
    product <- ""
    if (input$CB_degradation_rate_toProducts) {
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$PI_degradation_rate_product_", 
                                         as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, prod)
        } else {
          product <- paste0(product, prod, " + ")
        }
      }
    }
    
    # Build Equations
    arrow  <- "-->"
    var   <- input$PI_degradation_rate_species
    rc    <- input$TI_degradation_rate_RC
    textOut <- paste0(var,
                      arrow,
                      "(", rc, ") ",
                      product
    )
  }
  else if (input$eqnCreate_reaction_law == "degradation_by_enzyme") {
    # Get products if they exist
    if (input$CB_degradation_enzyme_toProducts) {
      num.deg.products <- as.numeric(input$NI_degradation_enzyme_num_products)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$PI_degradation_enzyme_product_", 
                                         as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2MathJ(prod))
        } else {
          product <- paste0(product, Var2MathJ(prod), " + ")
        }
      }
    } else {
      product <- ""
    }
    
    # Build Equations
    arrow <- "-->"
    var   <- input$PI_degradation_enzyme_species
    Km    <- input$TI_degradation_enzyme_Km
    
    if (input$CB_degradation_enzyme_useVmax) {
      Vmax <- input$TI_degradation_enzyme_Vmax
      textOut <- paste0(var,
                        " ",
                        arrow,
                        "(", Km, ", ", Vmax, ") ",
                        product
      )
    } else {
      enz  <- input$PI_degradation_enzyme_enzyme
      kcat <- input$TI_degradation_enzyme_kcat
      textOut <- paste0(var,
                        " [", enz, "]",
                        arrow,
                        "(", Km, ", ", kcat, ") ",
                        product
      )
    }
  } 
  else if (input$eqnCreate_reaction_law == "michaelis_menten") {
    substrate <- input$PI_michaelis_menten_substrate
    product   <- input$PI_michaelis_menten_product
    arrow     <- "-->"
    enzyme    <- input$PI_michaelis_menten_enzyme
    Km        <- input$TI_michaelis_menten_Km
    
    if (!input$CB_michaelis_menten_useVmax) {
      kcat    <- input$TI_michaelis_menten_kcat
      textOut <- paste0(substrate,
                        " [", enzyme, "]",
                        arrow,
                        "(", Km , ", ", kcat, ") ",
                        product)
    }
    else if (input$CB_michaelis_menten_useVmax) {
      Vmax <- input$TI_michaelis_menten_vmax
      textOut <- paste0(substrate,
                        " ",
                        arrow,
                        "(", Km , ", ", Vmax, ") ",
                        product
      )
    }
  }
  else {
    textOut <- NA
  }
  return(textOut)
})


# Edit Reactions ---------------------------------------------------------------
## Equation Builder Text Builder -----------------------------------------------
equationBuilder_edit <- reactive({
  
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- rv.REACTIONS$reactions[[eqn.num]]
  eqn.reaction.law     <- eqn.row$Reaction.Law
  
  if (eqn.reaction.law == "mass_action") {
    number.reactants <- as.numeric(input$NI_mass_action_num_reactants_edit)
    number.products  <- as.numeric(input$NI_mass_action_num_products_edit)
    
    kf <- input$TI_mass_action_forward_k_edit
    kr <- input$TI_mass_action_reverse_k_edit
    
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MA_r_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_reactant_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, var)
      } else {
        eqn_LHS <- paste0(eqn_LHS, var, " + ")
      }
    }
    
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MA_p_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_product_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, var)
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, var, " + ")
      }
    }
    
    if (input$PI_mass_action_reverisble_option_edit == "both_directions") {
      arrow <- paste0("<->",
                      "(", kf, ", ", kr, ") "
      )
    } else if (input$PI_mass_action_reverisble_option_edit == "forward_only") {
      arrow <- paste0("->",
                      "(", kf, ") "
      )
      
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (eqn.reaction.law == "mass_action_w_reg") {
    arrow <- "->"
    
    number.reactants <- 
      as.numeric(input$NI_mass_action_wReg_num_reactants_edit)
    number.products  <- 
      as.numeric(input$NI_mass_action_wReg_num_products_edit)
    
    has.f.reg <- input$CB_MAwR_chem_modifier_forward_edit
    has.r.reg <- input$CB_MAwR_chem_modifier_reverse_edit
    
    number_forward_regulators <-
      as.numeric(input$NI_MAwR_n_forward_regulators_edit)
    number_reverse_regulators <-
      as.numeric(input$NI_MAwR_n_reverse_regulators_edit)
    
    reversible <- input$reaction_mass_action_wReg_reverisble_edit
    
    # Build Reactant Equation Side
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_r_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_reactant_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, var)
      } else {
        eqn_LHS <- paste0(eqn_LHS, var, " + ")
      }
    }
    
    # Build Product Equation Side
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_p_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_product_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, var)
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, var, " + ")
      }
    }
    
    parameters <- c()
    modifiers  <- c()
    # Check For Forward Regulators
    if (has.f.reg) {
      #find regulators and add them together in form ([regulator/constant, 
      #regulator2/constant2, etc...])
      forwardModifiers <- c()
      for (i in seq(number_forward_regulators)) {
        regulator <-
          eval(parse(text = paste0(
            "input$PI_MAwR_forward_regulator_edit_", as.character(i)
          )))
        rateConstant <-
          eval(parse(text = paste0(
            "input$TI_MAwR_forward_regulator_RC_edit_", as.character(i)
          )))
        modifiers <- c(modifiers, regulator)
        parameters <- c(parameters, rateConstant)
      }
    } 
    else {
      # If no forward regulators, use kf
      parameters <- c(parameters, input$TI_MAwR_forward_k)
    }
    # Check If Reaction Is Reversible
    if (reversible == "both_directions") {
      arrow <- "<->"
      # Check if Reverse Regulator is used
      if (has.r.reg) {
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$PI_MAwR_reverse_regulator_edit_", as.character(i)
            )))
          rateConstant <-
            eval(parse(text = paste0(
              "input$TI_MAwR_reverse_regulator_RC_edit_", as.character(i)
            )))
          modifiers <- c(modifiers, regulator)
          parameters <- c(parameters, rateConstant)
        }
      }
      else {
        # If no regulators, use kr
        parameters <- c(parameters, input$TI_MAwR_reverse_k)
      }
    } 
    
    if (length(modifiers) > 0) {
      arrow <- paste0(" [", paste0(modifiers, collapse =", "), "]",
                      arrow,
                      "(", paste0(parameters, collapse = ", "), ") ")
    } else {
      arrow <- paste0(arrow,
                      "(", paste0(parameters, collapse = ", "), ") ")
    }
    
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (eqn.reaction.law == "synthesis") {
    if (input$CB_synthesis_factor_checkbox_edit) {
      arrow  <- "-->"
      var    <- input$PI_synthesis_byFactor_var_edit
      rc     <- input$TI_synthesis_byFactor_RC_edit
      factor <- input$PI_synthesis_byFactor_factor_edit
      textOut <- paste0(factor,
                        " ",
                        arrow,
                        "(", rc, ")",
                        " ",
                        var
      )
    } else {
      arrow <- "-->"
      var   <- input$PI_synthesis_rate_var_edit
      rc    <- input$TI_synthesis_rate_RC_edit
      textOut <- paste0(arrow,
                        "(", rc, ")",
                        " ",
                        var
      )
    }
  }
  else if (eqn.reaction.law == "degradation_rate") {
    num.deg.products <- as.numeric(input$NI_degradation_rate_num_products_edit)
    product <- ""
    if (input$CB_degradation_rate_toProducts_edit) {
      for (i in seq(num.deg.products)) {
        prod <- eval(
          parse(text = paste0("input$PI_degradation_rate_product_edit_", 
                              as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, prod)
        } else {
          product <- paste0(product, prod, " + ")
        }
      }
    }
    
    # Build Equations
    arrow  <- "-->"
    var   <- input$PI_degradation_rate_species_edit
    rc    <- input$TI_degradation_rate_RC_edit
    textOut <- paste0(var,
                      arrow,
                      "(", rc, ") ",
                      product
    )
  }
  else if (eqn.reaction.law == "degradation_by_enzyme") {
    # Get products if they exist
    if (input$CB_degradation_enzyme_toProducts_edit) {
      num.deg.products <- 
        as.numeric(input$NI_degradation_enzyme_num_products_edit)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(
          parse(text = paste0("input$PI_degradation_enzyme_product_edit_", 
                              as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2MathJ(prod))
        } else {
          product <- paste0(product, Var2MathJ(prod), " + ")
        }
      }
    } else {
      product <- ""
    }
    
    # Build Equations
    arrow <- "-->"
    var   <- input$PI_degradation_enzyme_species_edit
    Km    <- input$TI_degradation_enzyme_Km_edit
    
    if (input$CB_degradation_enzyme_useVmax_edit) {
      Vmax <- input$TI_degradation_enzyme_Vmax
      textOut <- paste0(var,
                        " ",
                        arrow,
                        "(", Km, ", ", Vmax, ") ",
                        product
      )
    } else {
      enz  <- input$PI_degradation_enzyme_enzyme_edit
      kcat <- input$TI_degradation_enzyme_kcat_edit
      textOut <- paste0(var,
                        " [", enz, "]",
                        arrow,
                        "(", Km, ", ", kcat, ") ",
                        product
      )
    }
  } 
  else if (eqn.reaction.law == "michaelis_menten") {
    substrate <- input$PI_michaelis_menten_substrate_edit
    product   <- input$PI_michaelis_menten_product_edit
    arrow     <- "-->"
    enzyme    <- input$PI_michaelis_menten_enzyme_edit
    Km        <- input$TI_michaelis_menten_Km_edit
    
    if (!input$CB_michaelis_menten_useVmax_edit) {
      kcat    <- input$TI_michaelis_menten_kcat_edit
      textOut <- paste0(substrate,
                        " [", enzyme, "]",
                        arrow,
                        "(", Km , ", ", kcat, ") ",
                        product)
    }
    else if (input$CB_michaelis_menten_useVmax_edit) {
      Vmax <- input$TI_michaelis_menten_vmax_edit
      textOut <- paste0(substrate,
                        " ",
                        arrow,
                        "(", Km , ", ", Vmax, ") ",
                        product
      )
    }
  }
  
  # else if (input$eqnCreate_type_of_equation_edit == "rate_eqn") {
  #   rate_left <- input$eqnCreate_rate_firstvar
  #   rate_right <- input$eqnCreate_rate_equation
  #   textOut <- paste0(rate_left, " = ", rate_right)
  # }
  # else if (input$eqnCreate_type_of_equation_edit == "time_dependent")
  # {
  #   TD_left <- input$eqnCreate_time_dependent_firstvar
  #   TD_right <- input$eqnCreate_time_dependent_equation
  #   textOut <- paste0(TD_left, "=", TD_right)
  # }
  else{textOut <- "ERROR"}
  return(textOut)
})

## Edit Latex Equation Builder ------------------------------------------------
equationLatexBuilder_edit <- reactive({
  
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- rv.REACTIONS$reactions[[eqn.num]]
  eqn.reaction.law     <- eqn.row$Reaction.Law
  
  if (eqn.reaction.law == "mass_action") {
    number.reactants <- as.numeric(input$NI_mass_action_num_reactants_edit)
    number.products  <- as.numeric(input$NI_mass_action_num_products_edit)
    
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MA_r_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_reactant_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, Var2Latex(var))
      } else {
        eqn_LHS <- paste0(eqn_LHS, Var2Latex(var), " + ")
      }
    }
    
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MA_p_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_product_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, Var2Latex(var))
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, Var2Latex(var), " + ")
      }
    }
    
    if (input$PI_mass_action_reverisble_option_edit == "both_directions") {
      arrow <- "\\xrightleftharpoons"
      
      arrow <- paste0(arrow, 
                      "[", 
                      Var2Latex(input$TI_mass_action_forward_k_edit), 
                      "]", 
                      "{", 
                      Var2Latex(input$TI_mass_action_reverse_k_edit), 
                      "}")
    }
    else if (input$PI_mass_action_reverisble_option_edit == "forward_only") {
      arrow <- "\\xrightarrow"
      
      arrow <- paste0(arrow, 
                      "[", 
                      Var2Latex(input$TI_mass_action_forward_k_edit), 
                      "]")
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
    
  }
  else if (eqn.reaction.law == "mass_action_w_reg") {
    arrow <- "\\xrightarrow"
    
    number.reactants <- as.numeric(input$NI_mass_action_wReg_num_reactants_edit)
    number.products  <- as.numeric(input$NI_mass_action_wReg_num_products_edit)
    
    has.f.reg <- input$CB_MAwR_chem_modifier_forward
    has.r.reg <- input$CB_MAwR_chem_modifier_reverse
    
    number_forward_regulators <-
      as.numeric(input$NI_MAwR_n_forward_regulators_edit)
    number_reverse_regulators <- 
      as.numeric(input$NI_MAwR_n_reverse_regulators_edit)
    
    reversible <- input$reaction_mass_action_wReg_reverisble_edit
    
    # Build Reactant Equation Side
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_r_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_reactant_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, Var2Latex(var))
      } else {
        eqn_LHS <- paste0(eqn_LHS, Var2Latex(var), " + ")
      }
    }
    
    # Build Product Equation Side
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_p_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_product_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, Var2Latex(var))
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, Var2Latex(var), " + ")
      }
    }
    
    # Check For Forward Regulators
    
    if (has.f.reg) {
      #find regulators and add them together in form ([regulator/constant, 
      #regulator2/constant2, etc...])
      forwardModifiers <- c()
      for (i in seq(number_forward_regulators)) {
        regulator <-
          eval(parse(text = paste0(
            "input$PI_MAwR_forward_regulator_edit_", as.character(i)
          )))
        rateConstant <-
          eval(parse(text = paste0(
            "input$TI_MAwR_forward_regulator_RC_edit_", as.character(i)
          )))
        modifierExpression <- paste0("(",
                                     Var2Latex(regulator),
                                     ":",
                                     Var2Latex(rateConstant),
                                     ")")
        forwardModifiers <-
          c(forwardModifiers, modifierExpression)
      }
      forwardModifiers <- paste(forwardModifiers, collapse = ", ")
    } 
    else {
      # If no forward regulators, use kf
      forwardModifiers <- Var2Latex(input$TI_MAwR_forward_k_edit)
    }
    forwardModifiers <- paste0("[",
                               forwardModifiers,
                               "]")
    # Check If Reaction Is Reversible
    if (reversible == "both_directions") {
      arrow <- "\\xrightleftharpoons"
      # Check if Reverse Regulator is used
      if (has.r.reg) {
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$PI_MAwR_reverse_regulator_edit_", as.character(i)
            )))
          rateConstant <-
            eval(parse(text = paste0(
              "input$TI_MAwR_reverse_regulator_RC_edit_", as.character(i)
            )))
          modifierExpression <- paste0("(",
                                       Var2Latex(regulator),
                                       ":",
                                       Var2Latex(rateConstant),
                                       ")")
          reverseModifiers <-
            c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ", ")
      }
      else {
        # If no regulators, use kr
        reverseModifiers <- Var2Latex(input$TI_MAwR_reverse_k_edit)
      }
      reverseModifiers <- paste0("{", 
                                 reverseModifiers, 
                                 "}")
    } 
    else {
      reverseModifiers <- ""
    }
    
    arrow <- paste0(arrow,
                    forwardModifiers,
                    reverseModifiers
    )
    
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
    
  }
  else if (eqn.reaction.law == "synthesis") {
    if (input$CB_synthesis_factor_checkbox_edit) {
      arrow  <- "\\xrightarrow"
      var    <- Var2Latex(input$PI_synthesis_byFactor_var_edit)
      rc     <- Var2Latex(input$TI_synthesis_byFactor_RC_edit)
      factor <- Var2Latex(input$PI_synthesis_byFactor_factor_edit)
      type   <- "syn"
      textOut <- paste0(arrow,
                        "[", factor, "]",
                        "{", rc, "}",
                        var
      )
    } else {
      arrow  <- "\\xrightarrow"
      var   <- Var2Latex(input$PI_synthesis_rate_var_edit)
      rc    <- Var2Latex(input$TI_synthesis_rate_RC_edit)
      type  <- "syn"
      textOut <- paste0(arrow,
                        "[", rc, "]",
                        "{", type, "}",
                        var
      )
    }
  } 
  else if (eqn.reaction.law == "degradation_rate") {
    # Get products if they exist
    if (input$CB_degradation_rate_toProducts_edit) {
      num.deg.products <- 
        as.numeric(input$NI_degradation_rate_num_products_edit)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(
          parse(text = paste0("input$PI_degradation_rate_product_edit_", 
                              as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2Latex(prod))
        } else {
          product <- paste0(product, Var2Latex(prod), " + ")
        }
      }
    } else {
      product <- "\\bigotimes"
    }
    
    # Build Equations
    arrow  <- "\\xrightarrow"
    var   <- Var2Latex(input$PI_degradation_rate_species_edit)
    rc    <- Var2Latex(input$TI_degradation_rate_RC_edit)
    type  <- "deg"
    textOut <- paste0(var,
                      arrow,
                      "[", rc, "]",
                      "{", type, "}",
                      product
    )
  } 
  else if (eqn.reaction.law == "degradation_by_enzyme") {
    # Get products if they exist
    if (input$CB_degradation_enzyme_toProducts_edit) {
      num.deg.products <- 
        as.numeric(input$NI_degradation_enzyme_num_products_edit)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(
          parse(text = paste0("input$PI_degradation_enzyme_product_edit_", 
                              as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2Latex(prod))
        } else {
          product <- paste0(product, Var2Latex(prod), " + ")
        }
      }
    } else {
      product <- "\\bigotimes"
    }
    
    # Build Equations
    arrow  <- "\\xrightarrow"
    var   <- Var2Latex(input$PI_degradation_enzyme_species_edit)
    Km    <- Var2Latex(input$TI_degradation_enzyme_Km_edit)
    type  <- "deg"
    
    if (input$CB_degradation_enzyme_useVmax_edit) {
      Vmax <- Var2Latex(input$TI_degradation_enzyme_Vmax_edit)
      textOut <- paste0(var,
                        arrow,
                        "[", Km, ", ", Vmax, "]",
                        "{", type, "} ",
                        product
      )
    } else {
      enz  <- Var2Latex(input$PI_degradation_enzyme_enzyme_edit)
      kcat <- Var2Latex(input$TI_degradation_enzyme_kcat_edit)
      textOut <- paste0(var,
                        arrow,
                        "[", Km, ", ", kcat, ", ", enz, "]",
                        "{", type, "}",
                        product
      )
    }
  } 
  else if (eqn.reaction.law == "michaelis_menten") {
    substrate <- Var2Latex(input$PI_michaelis_menten_substrate_edit)
    product   <- Var2Latex(input$PI_michaelis_menten_product_edit)
    arrow     <- "\\xrightarrow"
    enzyme    <- Var2Latex(input$PI_michaelis_menten_enzyme_edit)
    Km        <- Var2Latex(input$TI_michaelis_menten_Km_edit)
    
    if (!input$CB_michaelis_menten_useVmax_edit) {
      kcat    <- Var2Latex(input$TI_michaelis_menten_kcat_edit)
      textOut <- paste0(substrate,
                        " + ",
                        enzyme, " ",
                        arrow,
                        "[", Km ,"]",
                        "{", kcat, "} ",
                        product)
    }
    else if (input$CB_michaelis_menten_useVmax_edit) {
      Vmax <- Var2Latex(input$TI_michaelis_menten_vmax_edit)
      textOut <- paste0(substrate, 
                        arrow,
                        "[", Vmax, "]",
                        "{", Km, "}",
                        product
      )
    }
  }
  else {
    textOut <- "ERROR"
  }
  
  return(textOut)
})

## Edit Mathjax Equation Builder -----------------------------------------------
equationBuilder_edit_mathJax <- reactive({
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- rv.REACTIONS$reactions[[eqn.num]]
  
  # Unpack Equation Information
  eqn.reaction.law     <- eqn.row$Reaction.Law    
  
  if (eqn.reaction.law == "mass_action") {
    number.reactants <- as.numeric(input$NI_mass_action_num_reactants_edit)
    number.products  <- as.numeric(input$NI_mass_action_num_products_edit)
    
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MA_r_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_reactant_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var))
      } else {
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var), " + ")
      }
    }
    
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MA_p_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MA_product_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var))
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var), " + ")
      }
    }
    
    if (input$PI_mass_action_reverisble_option_edit == "both_directions") {
      arrow <- "<->"
      
      arrow <- paste0("\\ce{",
                      arrow, 
                      "[{", 
                      Var2MathJ(input$TI_mass_action_forward_k_edit),
                      "\\text{, }",
                      Var2MathJ(input$TI_mass_action_reverse_k_edit), 
                      "}]", 
                      "}")
    }
    else if (input$PI_mass_action_reverisble_option_edit == "forward_only") {
      arrow = "->"
      
      arrow <- paste0("\\ce{",
                      arrow, 
                      "[{", 
                      Var2MathJ(input$TI_mass_action_forward_k_edit), 
                      "}]",
                      "}")
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (eqn.reaction.law == "mass_action_w_reg") {
    arrow <- "->"
    # browser()
    number.reactants <- as.numeric(input$NI_mass_action_wReg_num_reactants_edit)
    number.products  <- as.numeric(input$NI_mass_action_wReg_num_products_edit)
    
    has.f.reg <- input$CB_MAwR_chem_modifier_forward_edit
    has.r.reg <- input$CB_MAwR_chem_modifier_reverse_edit
    
    number_forward_regulators <- 
      as.numeric(input$NI_MAwR_n_forward_regulators_edit)
    number_reverse_regulators <- 
      as.numeric(input$NI_MAwR_n_reverse_regulators_edit)
    
    reversible <- input$reaction_mass_action_wReg_reverisble_edit
    
    # Build Reactant Equation Side
    eqn_LHS <- ""
    for (i in seq(number.reactants)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_r_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_reactant_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_LHS <- paste0(eqn_LHS, coef, "*")
        }
      } else {
        eqn_LHS <- ""
      }
      
      if (i == as.numeric(number.reactants)) {
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var))
      } else {
        eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var), " + ")
      }
    }
    
    # Build Product Equation Side
    eqn_RHS <- ""
    for (i in seq(number.products)) {
      coef <- eval(parse(text = paste0("input$NI_MAwR_p_stoichiometry_edit_", 
                                       as.character(i))))
      var <- eval(parse(text = paste0("input$PI_MAwR_product_edit_", 
                                      as.character(i))))
      if (!is.null(coef)) {
        if (coef != "1") {
          eqn_RHS <- paste0(eqn_RHS, coef, "*")
        }
      } else {
        eqn_RHS <- ""
      }
      
      if (i == as.numeric(number.products)) {
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var))
      }
      else{
        eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var), " + ")
      }
    }
    
    modifiers <- c()
    parameters <- c()
    # Check For Forward Regulators
    if (has.f.reg) {
      #find regulators and add them together in form ([regulator/constant, 
      #regulator2/constant2, etc...])
      for (i in seq(number_forward_regulators)) {
        regulator <-
          eval(parse(text = paste0(
            "input$PI_MAwR_forward_regulator_edit_", as.character(i)
          )))
        rateConstant <-
          eval(parse(text = paste0(
            "input$TI_MAwR_forward_regulator_RC_edit_", as.character(i)
          )))
        modifiers <- c(modifiers, Var2MathJ(regulator))
        parameters <- c(parameters, Var2MathJ(rateConstant))
      }
    } 
    else {
      # If no forward regulators, use kf
      parameters <- c(parameters, Var2MathJ(input$TI_MAwR_forward_k_edit))
    }
    # Check If Reaction Is Reversible
    if (reversible == "both_directions") {
      arrow <- "<->"
      # Check if Reverse Regulator is used
      if (has.r.reg) {
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$PI_MAwR_reverse_regulator_edit_", as.character(i)
            )))
          rateConstant <-
            eval(parse(text = paste0(
              "input$TI_MAwR_reverse_regulator_RC_edit_", as.character(i)
            )))
          modifiers  <- c(modifiers, Var2MathJ(regulator))
          parameters <- c(parameters, Var2MathJ(rateConstant))
        }
      }
      else {
        # If no regulators, use kr
        parameters <- c(parameters, Var2MathJ(input$TI_MAwR_reverse_k_edit))
      }
    } 

    if (length(modifiers) > 0) {
      parameter.exp <- paste0(parameters, collapse = "\\text{, }")
      modifiers.exp <- paste0(modifiers,  collapse = "\\text{, }")
      arrow <- 
        paste0(
          "\\ce{",
          arrow,
          "[{", parameter.exp, "}]",
          "[{", modifiers.exp, "}]",
          "}"
        )
    } else {
      parameter.exp <- paste0(parameters, collapse = "\\text{, }")
      arrow <- 
        paste0(
          "\\ce{",
          arrow,
          "[{", parameter.exp, "}]",
          "}"
        )
    }
    
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (eqn.reaction.law == "synthesis") {
    
    if (input$CB_synthesis_factor_checkbox_edit) {
      arrow  <- "->"
      var    <- Var2MathJ(input$PI_synthesis_byFactor_var_edit)
      rc     <- Var2MathJ(input$TI_synthesis_byFactor_RC_edit)
      factor <- Var2MathJ(input$PI_synthesis_byFactor_factor_edit)
      type   <- "syn"
      textOut <- paste0("\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "[{", factor, "}]",
                        "}",
                        var
      )
    } else {
      arrow <- "->"
      var   <- Var2MathJ(input$PI_synthesis_rate_var_edit)
      rc    <- Var2MathJ(input$TI_synthesis_rate_RC_edit)
      type  <- "syn"
      textOut <- paste0("\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "}",
                        var
      )
    }
  } 
  else if (eqn.reaction.law == "degradation_rate") {
    # Get products if they exist
    if (input$CB_degradation_rate_toProducts_edit) {
      num.deg.products <- 
        as.numeric(input$NI_degradation_rate_num_products_edit)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(
          parse(text = paste0("input$PI_degradation_rate_product_edit_", 
                              as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2MathJ(prod))
        } else {
          product <- paste0(product, Var2MathJ(prod), " + ")
        }
      }
    } else {
      product <- "\\bigotimes"
    }
    
    # Build Equations
    arrow <- "->"
    var   <- Var2MathJ(input$PI_degradation_rate_species_edit)
    rc    <- Var2MathJ(input$TI_degradation_rate_RC_edit)
    type  <- "deg"
    textOut <- paste0(var,
                      "\\ce{",
                      arrow,
                      "[{", rc, "}]",
                      "}",
                      product
    )
  }
  else if (eqn.reaction.law == "degradation_by_enzyme") {
    # Get products if they exist
    if (input$CB_degradation_enzyme_toProducts_edit) {
      num.deg.products <- 
        as.numeric(input$NI_degradation_enzyme_num_products_edit)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(
          parse(text = paste0("input$PI_degradation_enzyme_product_edit_", 
                              as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2MathJ(prod))
        } else {
          product <- paste0(product, Var2MathJ(prod), " + ")
        }
      }
    } else {
      product <- "\\bigotimes"
    }
    
    # Build Equations
    arrow <- "->"
    var   <- Var2MathJ(input$PI_degradation_enzyme_species_edit)
    Km    <- Var2MathJ(input$TI_degradation_enzyme_Km_edit)
    type  <- "deg"
    
    if (input$CB_degradation_enzyme_useVmax_edit) {
      Vmax <- Var2MathJ(input$TI_degradation_enzyme_Vmax_edit)
      textOut <- paste0(var,
                        "\\ce{",
                        arrow,
                        "[{", Km, ",\\ ", Vmax, "}]",
                        "}",
                        product
      )
    } else {
      enz  <- Var2MathJ(input$PI_degradation_enzyme_enzyme_edit)
      kcat <- Var2MathJ(input$TI_degradation_enzyme_kcat_edit)
      textOut <- paste0(var,
                        "\\ce{",
                        arrow,
                        "[{", Km, ",\\ ", kcat, "}]",
                        "[{", enz, "}]",
                        "}",
                        product
      )
    }
  }
  else if (eqn.reaction.law == "michaelis_menten") {
    substrate <- Var2MathJ(input$PI_michaelis_menten_substrate_edit)
    product   <- Var2MathJ(input$PI_michaelis_menten_product_edit)
    arrow     <- "->"
    enzyme    <- Var2MathJ(input$PI_michaelis_menten_enzyme_edit)
    Km        <- Var2MathJ(input$TI_michaelis_menten_Km_edit)
    
    if (!input$CB_michaelis_menten_useVmax_edit) {
      kcat    <- Var2MathJ(input$TI_michaelis_menten_kcat_edit)
      textOut <- paste0(substrate,
                        "\\ce{",
                        arrow,
                        "[{", Km , ",\\ ", kcat, "}]",
                        "[{", enzyme, "}]",
                        "}",
                        product)
    }
    else if (input$CB_michaelis_menten_useVmax_edit) {
      Vmax = Var2MathJ(input$TI_michaelis_menten_vmax_edit)
      textOut <- paste0(substrate, 
                        "\\ce{",
                        arrow,
                        "[{", Km, ",\\ ", Vmax, "}]",
                        "}",
                        product
      )
    }
  }
  else{textOut <- "ERROR"}
  textOut <- paste0("$$", textOut, "$$")
  return(textOut)
  
})


