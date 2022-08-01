Var2MathJ <- function(var = NULL){
  # Converts 
  # Args:
  #   var: variable to change to mathjax format converting subscripts properly
  #
  # Returns:
  #   var in latex readable form
  #
  # Ex: var = my_var -> var = my_{var} 
  

  latex.var = ""
  
  if (!is.null(var)) {
    split.var = strsplit(var, "")[[1]]
    has.underscore = FALSE

    for (i in seq(length(split.var))) {
      if (split.var[i] == "_" & !has.underscore) {
        has.underscore = TRUE
        latex.var = paste0(latex.var, split.var[i], "{")
      }else{
        latex.var = paste0(latex.var, split.var[i])
      }
    }
    if (has.underscore) {
      latex.var = paste0(latex.var, "}")
    }

  }
  
  return(latex.var)
}


equationBuilder_MathJax <- reactive({
  if (input$eqnCreate_type_of_equation == "chem_rxn") {
    number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
    number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
    number_forward_regulators = as.numeric(input$eqn_options_chem_num_forward_regulators)
    number_reverse_regulators = as.numeric(input$eqn_options_chem_num_reverse_regulators)
    
    eqn_LHS <- ""
    for (i in seq(number_LHS_equations)) {
      coef <- eval(parse(text = paste0("input$LHS_Coeff_", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_", as.character(i))))
      if (coef != "1") {eqn_LHS <- paste0(eqn_LHS, coef, "*")}
      if (i == as.numeric(number_LHS_equations)) {eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var))}
      else{eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var), " + ")}
    }
    
    eqn_RHS <- ""
    for (i in seq(number_RHS_equations)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_", as.character(i))))
      if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
      if (i == as.numeric(number_RHS_equations)) {eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var))}
      else{eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var), " + ")}
    }
    
    if (input$eqn_chem_forward_or_both == "both_directions") {
      arrow <- "<->"
      if (input$eqn_options_chem_modifier_forward && input$eqn_options_chem_modifier_reverse) {
        #find regulators and add them together in form ([regulator/constant, regulator2/constant2, etc...])
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":", 
                                       Var2MathJ(rateConstant),
                                       ")"
                                       )
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ", ")
        
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":", 
                                       Var2MathJ(rateConstant),
                                       ")"
                                       )
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ", ")
        
        arrow <- paste0("\\ce{",
                        arrow, 
                        "[{",
                        forwardModifiers,
                        "}]", 
                        "[{", 
                        reverseModifiers, 
                        "}]",
                        "}")
      }
      else if (input$eqn_options_chem_modifier_forward && !input$eqn_options_chem_modifier_reverse) {
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator), 
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")"
                                       )
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ", ")
        
        arrow <- paste0("\\ce{",
                        arrow, 
                        "[{",
                        forwardModifiers,
                        "}]",
                        "[{", 
                        Var2MathJ(input$eqn_chem_back_k),
                        "}]",
                        "}"
                        )
      }
      else if (!input$eqn_options_chem_modifier_forward && input$eqn_options_chem_modifier_reverse) {
        jPrint("Reverse Equation Build")
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")"
                                       )
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ",")
        arrow <- paste0( "\\ce{",
                         arrow, 
                         "[{", 
                         Var2MathJ(input$eqn_chem_forward_k),
                         "}]",
                         "[{", 
                         reverseModifiers, 
                         "}]",
                         "}"
                         )
      }
      else
      {
        arrow <- paste0("\\ce{",
                        arrow, 
                        "[{", 
                        Var2MathJ(input$eqn_chem_forward_k), 
                        "}]", 
                        "[{", 
                        Var2MathJ(input$eqn_chem_back_k), 
                        "}]",
                        "}")
      }
    }
    else if (input$eqn_chem_forward_or_both == "forward_only") {
      arrow = "->"
      if (input$eqn_options_chem_modifier_forward) {
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")")
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")
        arrow <- paste0("\\ce{",
                        arrow,
                        "[{",
                        forwardModifiers,
                        "}]",
                        "}")
      }
      else
      {
        arrow <- paste0("\\ce{",
                        arrow, 
                        "[{", 
                        Var2MathJ(input$eqn_chem_forward_k), 
                        "}]",
                        "}")
      }
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (input$eqnCreate_type_of_equation == "enzyme_rxn") {
    substrate <- Var2MathJ(input$eqn_enzyme_substrate)
    product   <- Var2MathJ(input$eqn_enzyme_product)
    arrow     <- "->"
    enzyme    <- Var2MathJ(input$eqn_enzyme_enzyme)
    Km        <- Var2MathJ(input$eqn_enzyme_Km)
    
    if (!input$eqn_options_enzyme_useVmax) {
      kcat    <- Var2MathJ(input$eqn_enzyme_kcat)
      textOut <- paste0(substrate,
                        " + ",
                        enzyme,
                        "\\ce{",
                        arrow,
                        "[{", Km ,"}]",
                        "[{", kcat, "}]",
                        "}",
                        product)
    }
    else if (input$eqn_options_enzyme_useVmax) {
      Vmax = input$eqn_enzyme_Vmax
      textOut <- paste0(substrate, 
                        "\\ce{",
                        arrow,
                        "[{", Vmax, "}]",
                        "[{", Km, "}]",
                        "}",
                        product
                        )
    }
  }
  else if (input$eqnCreate_type_of_equation == "syn") {
    
    if (input$eqn_syn_law == "rate") {
      arrow <- "->"
      var   <- Var2MathJ(input$eqn_syn_rate_var)
      rc    <- Var2MathJ(input$eqn_syn_rate_RC)
      type  <- "syn"
      textOut <- paste0("\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        "}",
                        var
                        )
    } else if (input$eqn_syn_law == "byFactor") {
      arrow  <- "->"
      var    <- Var2MathJ(input$eqn_syn_sby_var)
      rc     <- Var2MathJ(input$eqn_syn_sby_RC)
      factor <- Var2MathJ(input$eqn_syn_sby_factor)
      type   <- "syn"
      textOut <- paste0(factor,
                        "\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        "}",
                        var
      )
    }
  } 
  else if (input$eqnCreate_type_of_equation == "deg") {
    # Get products if they exist
    if (input$eqn_deg_to_products) {
      num.deg.products <- as.numeric(input$eqn_deg_num_products)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$eqn_deg_product_", as.character(i))))
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
    if (input$eqn_deg_law == "rate") {
      arrow <- "->"
      var   <- Var2MathJ(input$eqn_deg_var)
      rc    <- Var2MathJ(input$eqn_deg_rate_RC)
      type  <- "deg"
      textOut <- paste0(var,
                        "\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        "}",
                        product
      )
      
      
    } else if (input$eqn_deg_law == "byEnzyme") {
      arrow <- "->"
      var   <- Var2MathJ(input$eqn_deg_var)
      Km    <- Var2MathJ(input$eqn_deg_Km)
      type  <- "deg"
      
      if (input$eqn_deg_use_Vmax) {
        Vmax <- Var2MathJ(input$eqn_deg_Vmax)
        textOut <- paste0(var,
                          "\\ce{",
                          arrow,
                          "[{", Km, ",\\ ", Vmax, "}]",
                          "[{", type, "}]",
                          "}",
                          product
        )
      } else {
        enz  <- Var2MathJ(input$eqn_deg_enzyme)
        kcat <- Var2MathJ(input$eqn_deg_kcat)
        textOut <- paste0(var,
                          "\\ce{",
                          arrow,
                          "[{", Km, ",\\ ", kcat, ",\\ ", enz, "}]",
                          "[{", type, "}]",
                          "}",
                          product
        )
      }
    }
  }
  # else if (input$eqnCreate_type_of_equation == "simp_diff") {
  #   var_left = input$simp_diff_var1
  #   var_right = input$simp_diff_var2
  #   diff_coef <- input$simp_diff_PS_Var
  #   ifelse(input$simp_diff_wayOfDiffusion, symbol <- "-->", symbol <- "<-->")
  #   
  #   textOut <- paste0(var_left, " ", symbol, "(", diff_coef, ") ", var_right)
  # }
  else if (input$eqnCreate_type_of_equation == "rate_eqn")
  {
    rate_left <- input$eqnCreate_custom_eqn_lhs
    rate_right <- input$eqnCreate_custom_eqn_rhs
    textOut <- paste0(rate_left, " = ", rate_right)
  }
  else if (input$eqnCreate_type_of_equation == "time_dependent")
  {
    TD_left <- input$eqnCreate_time_dependent_firstvar
    TD_right <- input$eqnCreate_time_dependent_equation
    textOut <- paste0(TD_left, "=", TD_right)
  }
  else{textOut <- "ERROR"}
  textOut <- paste0("$$", textOut, "$$")
  return(textOut)
})

equationBuilder_edit_mathJax <- reactive({
  if (input$eqnCreate_type_of_equation_edit == "chem_rxn") {
    number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
    number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
    number_forward_regulators = as.numeric(input$eqn_options_chem_num_forward_regulators_edit)
    number_reverse_regulators = as.numeric(input$eqn_options_chem_num_reverse_regulators_edit)
    
    eqn_LHS <- ""
    for (i in seq(number_LHS_equations)) {
      coef <- eval(parse(text = paste0("input$LHS_Coeff_edit", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_edit", as.character(i))))
      if (coef != "1") {eqn_LHS <- paste0(eqn_LHS, coef, "*")}
      if (i == as.numeric(number_LHS_equations)) {eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var))}
      else{eqn_LHS <- paste0(eqn_LHS, Var2MathJ(var), " + ")}
    }
    
    eqn_RHS <- ""
    for (i in seq(number_RHS_equations)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_edit", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_edit", as.character(i))))
      if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
      if (i == as.numeric(number_RHS_equations)) {eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var))}
      else{eqn_RHS <- paste0(eqn_RHS, Var2MathJ(var), " + ")}
    }
    
    if (input$eqn_chem_forward_or_both_edit == "both_directions") {
      arrow <- "<->"
      if (input$eqn_options_chem_modifier_forward_edit && 
          input$eqn_options_chem_modifier_reverse_edit) {
        #find regulators and add them together in form ([regulator/constant, regulator2/constant2, etc...])
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_edit", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_edit", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":", 
                                       Var2MathJ(rateConstant),
                                       ")"
          )
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ", ")
        
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_edit", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_edit", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":", 
                                       Var2MathJ(rateConstant),
                                       ")"
          )
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ", ")
        
        arrow <- paste0("\\ce{",
                        arrow, 
                        "[{",
                        forwardModifiers,
                        "}]", 
                        "[{", 
                        reverseModifiers, 
                        "}]",
                        "}")
      }
      else if (input$eqn_options_chem_modifier_forward_edit 
               && !input$eqn_options_chem_modifier_reverse_edit) {
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_edit", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_edit", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator), 
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")"
          )
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ", ")
        
        arrow <- paste0("\\ce{",
                        arrow, 
                        "[{",
                        forwardModifiers,
                        "}]",
                        "[{", 
                        Var2MathJ(input$eqn_chem_back_k_edit),
                        "}]",
                        "}"
        )
      }
      else if (!input$eqn_options_chem_modifier_forward_edit && 
               input$eqn_options_chem_modifier_reverse_edit) {
        jPrint("Reverse Equation Build")
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_edit", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_edit", as.character(i))))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")"
          )
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ",")
        arrow <- paste0( "\\ce{",
                         arrow, 
                         "[{", 
                         Var2MathJ(input$eqn_chem_forward_k_edit),
                         "}]",
                         "[{", 
                         reverseModifiers, 
                         "}]",
                         "}"
        )
      } else {
        arrow <- paste0("\\ce{",
                        arrow, 
                        "[{", 
                        Var2MathJ(input$eqn_chem_forward_k_edit), 
                        "}]", 
                        "[{", 
                        Var2MathJ(input$eqn_chem_back_k_edit), 
                        "}]",
                        "}")
      }
    }
    else if (input$eqn_chem_forward_or_both_edit == "forward_only") {
      arrow = "->"
      if (input$eqn_options_chem_modifier_forward_edit) {
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <-
            eval(parse(text = paste0(
              "input$eqn_forward_regulator_edit", as.character(i)
            )))
          rateConstant <-
            eval(parse(
              text = paste0("input$eqn_forward_rateConstant_edit", as.character(i))
            ))
          modifierExpression <- paste0("(",
                                       Var2MathJ(regulator),
                                       ":",
                                       Var2MathJ(rateConstant),
                                       ")")
          forwardModifiers <-
            c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")
        arrow <- paste0("\\ce{",
                        arrow,
                        "[{",
                        forwardModifiers,
                        "}]",
                        "}")
      }
      else
      {
        arrow <- paste0("\\ce{",
                        arrow, 
                        "[{", 
                        Var2MathJ(input$eqn_chem_forward_k_edit), 
                        "}]",
                        "}")
      }
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (input$eqnCreate_type_of_equation_edit == "enzyme_rxn") {
    substrate <- Var2MathJ(input$eqn_enzyme_substrate_edit)
    product   <- Var2MathJ(input$eqn_enzyme_product_edit)
    arrow     <- "->"
    enzyme    <- Var2MathJ(input$eqn_enzyme_enzyme_edit)
    Km        <- Var2MathJ(input$eqn_enzyme_Km_edit)
    
    if (!input$eqn_options_enzyme_useVmax_edit) {
      kcat    <- Var2MathJ(input$eqn_enzyme_kcat_edit)
      textOut <- paste0(substrate,
                        " + ",
                        enzyme,
                        "\\ce{",
                        arrow,
                        "[{", Km ,"}]",
                        "[{", kcat, "}]",
                        "}",
                        product)
    }
    else if (input$eqn_options_enzyme_useVmax_edit) {
      Vmax = input$eqn_enzyme_Vmax_edit
      textOut <- paste0(substrate, 
                        "\\ce{",
                        arrow,
                        "[{", Vmax, "}]",
                        "[{", Km, "}]",
                        "}",
                        product
      )
    }
  }
  else if (input$eqnCreate_type_of_equation_edit == "syn") {
    
    if (input$eqn_syn_law_edit == "rate") {
      arrow <- "->"
      var   <- Var2MathJ(input$eqn_syn_rate_var_edit)
      rc    <- Var2MathJ(input$eqn_syn_rate_RC_edit)
      type  <- "syn"
      textOut <- paste0("\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        "}",
                        var
      )
    } else if (input$eqn_syn_law_edit == "byFactor") {
      arrow  <- "->"
      var    <- Var2MathJ(input$eqn_syn_sby_var_edit)
      rc     <- Var2MathJ(input$eqn_syn_sby_RC_edit)
      factor <- Var2MathJ(input$eqn_syn_sby_factor_edit)
      type   <- "syn"
      textOut <- paste0(factor,
                        "\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        "}",
                        var
      )
    }
  } 
  else if (input$eqnCreate_type_of_equation_edit == "deg") {
    # Get products if they exist
    if (input$eqn_deg_to_products_edit) {
      num.deg.products <- as.numeric(input$eqn_deg_num_products_edit)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$eqn_deg_product_edit", as.character(i))))
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
    if (input$eqn_deg_law_edit == "rate") {
      arrow <- "->"
      var   <- Var2MathJ(input$eqn_deg_var_edit)
      rc    <- Var2MathJ(input$eqn_deg_rate_RC_edit)
      type  <- "deg"
      textOut <- paste0(var,
                        "\\ce{",
                        arrow,
                        "[{", rc, "}]",
                        "[{", type, "}]",
                        "}",
                        product
      )
      
      
    } else if (input$eqn_deg_law_edit == "byEnzyme") {
      arrow <- "->"
      var   <- Var2MathJ(input$eqn_deg_var_edit)
      Km    <- Var2MathJ(input$eqn_deg_Km_edit)
      type  <- "deg"
      
      if (input$eqn_deg_use_Vmax_edit) {
        Vmax <- Var2MathJ(input$eqn_deg_Vmax_edit)
        textOut <- paste0(var,
                          "\\ce{",
                          arrow,
                          "[{", Km, ",\\ ", Vmax, "}]",
                          "[{", type, "}]",
                          "}",
                          product
        )
      } else {
        enz  <- Var2MathJ(input$eqn_deg_enzyme_edit)
        kcat <- Var2MathJ(input$eqn_deg_kcat_edit)
        textOut <- paste0(var,
                          "\\ce{",
                          arrow,
                          "[{", Km, ",\\ ", kcat, ",\\ ", enz, "}]",
                          "[{", type, "}]",
                          "}",
                          product
        )
      }
    }
  }
  # else if (input$eqnCreate_type_of_equation_edit == "rate_eqn")
  # {
  #   rate_left <- input$eqnCreate_rate_firstvar_edit
  #   rate_right <- input$eqnCreate_rate_equation_edit
  #   textOut <- paste0(rate_left, " = ", rate_right)
  # }
  # else if (input$eqnCreate_type_of_equation_edit == "time_dependent")
  # {
  #   TD_left <- input$eqnCreate_time_dependent_firstvar_edit
  #   TD_right <- input$eqnCreate_time_dependent_equation_edit
  #   textOut <- paste0(TD_left, "=", TD_right)
  # }
  else{textOut <- "ERROR"}
  textOut <- paste0("$$", textOut, "$$")
  return(textOut)
  
})

equationLatexBuilder <- reactive({
  if (input$eqnCreate_type_of_equation == "chem_rxn") {
    number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
    number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
    number_forward_regulators = as.numeric(input$eqn_options_chem_num_forward_regulators)
    number_reverse_regulators = as.numeric(input$eqn_options_chem_num_reverse_regulators)
    
    eqn_LHS <- ""
    for (i in seq(number_LHS_equations)) {
      coef <- eval(parse(text = paste0("input$LHS_Coeff_", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_", as.character(i))))
      if (coef != "1") {eqn_LHS <- paste0(eqn_LHS, coef, "*")}
      if (i == as.numeric(number_LHS_equations)) {eqn_LHS <- paste0(eqn_LHS, VarToLatexForm(var))}
      else{eqn_LHS <- paste0(eqn_LHS, VarToLatexForm(var), " + ")}
    }
    
    eqn_RHS <- ""
    for (i in seq(number_RHS_equations)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_", as.character(i))))
      if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
      if (i == as.numeric(number_RHS_equations)) {eqn_RHS <- paste0(eqn_RHS, VarToLatexForm(var))}
      else{eqn_RHS <- paste0(eqn_RHS, VarToLatexForm(var), " + ")}
    }
    
    if (input$eqn_chem_forward_or_both == "both_directions") {
      arrow <- "\\xrightleftharpoons"
      if (input$eqn_options_chem_modifier_forward && input$eqn_options_chem_modifier_reverse) {
        #find regulators and add them together in form ([regulator/constant, regulator2/constant2, etc...])
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       VarToLatexForm(regulator),
                                       ":", 
                                       VarToLatexForm(rateConstant),
                                       ")"
          )
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ", ")
        
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       VarToLatexForm(regulator),
                                       ":", 
                                       VarToLatexForm(rateConstant),
                                       ")"
          )
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ", ")
        
        arrow <- paste0(arrow, "[", reverseModifiers, "]", "{",forwardModifiers ,"}")
      }
      else if (input$eqn_options_chem_modifier_forward && !input$eqn_options_chem_modifier_reverse) {
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       VarToLatexForm(regulator), 
                                       ":",
                                       VarToLatexForm(rateConstant),
                                       ")"
          )
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ", ")
        
        arrow <- paste0(arrow, 
                        "[",
                        VarToLatexForm(input$eqn_chem_back_k),
                        "]",
                        "{", 
                        forwardModifiers,
                        "}"
        )
      }
      else if (!input$eqn_options_chem_modifier_forward && input$eqn_options_chem_modifier_reverse) {
        jPrint("Reverse Equation Build")
        reverseModifiers <- c()
        for (i in seq(number_reverse_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       VarToLatexForm(regulator),
                                       ":",
                                       VarToLatexForm(rateConstant),
                                       ")"
          )
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ",")
        arrow <- paste0( arrow, 
                         "[",
                         reverseModifiers,
                         "]",
                         "{", 
                         VarToLatexForm(input$eqn_chem_forward_k),
                         "}"
        )
      }
      else
      {
        arrow <- paste0(arrow, 
                        "[", 
                        VarToLatexForm(input$eqn_chem_back_k), 
                        "]", 
                        "{", 
                        VarToLatexForm(input$eqn_chem_forward_k), 
                        "}")
      }
    }
    else if (input$eqn_chem_forward_or_both == "forward_only") {
      arrow = "\\xrightarrow"
      if (input$eqn_options_chem_modifier_forward) {
        forwardModifiers <- c()
        for (i in seq(number_forward_regulators)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0("(",
                                       VarToLatexForm(regulator),
                                       ":",
                                       VarToLatexForm(rateConstant),
                                       ")")
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")
        arrow <- paste0(arrow,
                        "[]",
                        "{",
                        forwardModifiers,
                        "}")
      }
      else
      {
        arrow <- paste0(arrow, 
                        "[]",
                        "{", 
                        VarToLatexForm(input$eqn_chem_forward_k), 
                        "}")
      }
    }
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (input$eqnCreate_type_of_equation == "enzyme_rxn") {
    substrate <- VarToLatexForm(input$eqn_enzyme_substrate, mathMode = FALSE)
    product   <- VarToLatexForm(input$eqn_enzyme_product, mathMode = FALSE)
    arrow     <- "\\xrightarrow"
    enzyme    <- VarToLatexForm(input$eqn_enzyme_enzyme, mathMode = FALSE)
    Km        <- VarToLatexForm(input$eqn_enzyme_Km, mathMode = FALSE)
    type      <- "enz"
    
    if (!input$eqn_options_enzyme_useVmax) {
      kcat    <- VarToLatexForm(input$eqn_enzyme_kcat)
      textOut <- paste0(substrate,
                        " + ",
                        enzyme,
                        arrow,
                        "[",
                        type,
                        "]",
                        "{",
                        Km,
                        ",\\:",
                        kcat, 
                        "}",
                        product)
    }
    else if (input$eqn_options_enzyme_useVmax) {
      Vmax = input$eqn_enzyme_Vmax
      textOut <- paste0(substrate, 
                        arrow,
                        "[",
                        type,
                        "]",
                        "{",
                        Km,
                        "\\:",
                        Vmax, 
                        "}",
                        product
      )
    }
  }
  else if (input$eqnCreate_type_of_equation == "syn") {
    
    if (input$eqn_syn_law == "rate") {
      arrow <- "\\xrightarrow"
      var   <- VarToLatexForm(input$eqn_syn_rate_var)
      rc    <- VarToLatexForm(input$eqn_syn_rate_RC)
      type  <- "syn"
      textOut <- paste0(arrow,
                        "[",
                        type,
                        "]",
                        "{",
                        rc,
                        "}",
                        var
      )
    } else if (input$eqn_syn_law == "byFactor") {
      arrow  <- "\\xrightarrow"
      var    <- VarToLatexForm(input$eqn_syn_sby_var)
      rc     <- VarToLatexForm(input$eqn_syn_sby_RC)
      factor <- VarToLatexForm(input$eqn_syn_sby_factor)
      type   <- "syn"
      textOut <- paste0(factor,
                        arrow,
                        "[",
                        type,
                        "]",
                        "{",
                        rc,
                        "}",
                        var
      )
    }
  } else if (input$eqnCreate_type_of_equation == "deg") {
    
    if (input$eqn_deg_law == "rate") {
      arrow <- "\\xrightarrow"
      var   <- VarToLatexForm(input$eqn_deg_var)
      rc    <- VarToLatexForm(input$eqn_deg_rate_RC)
      type  <- "deg"
      textOut <- paste0(var,
                        arrow,
                        "[",
                        type,
                        "]",
                        "{",
                        rc,
                        "}",
                        "\\bigotimes"
      )
      
      
    } else if (input$eqn_deg_law == "byEnzyme") {
      arrow <- "\\xrightarrow"
      var   <- VarToLatexForm(input$eqn_deg_var)
      Km    <- VarToLatexForm(input$eqn_deg_Km)
      type  <- "deg"
      
      if (input$eqn_deg_use_Vmax) {
        Vmax <- VarToLatexForm(input$eqn_deg_Vmax)
        textOut <- paste0(var,
                          arrow,
                          "[",
                          type,
                          "]",
                          "{",
                          Km,
                          ",\\:",
                          Vmax,
                          "}",
                          "\\bigotimes"
        )
      } else {
        enz  <- VarToLatexForm(input$eqn_deg_enzyme)
        kcat <- VarToLatexForm(input$eqn_deg_kcat)
        textOut <- paste0(var,
                          arrow,
                          "[",
                          type,
                          "]",
                          "{",
                          Km,
                          ",\\:",
                          kcat,
                          ",\\:",
                          enz,
                          "}",
                          "\\bigotimes"
        )
      }
    }
  }
  # else if (input$eqnCreate_type_of_equation == "simp_diff") {
  #   var_left = input$simp_diff_var1
  #   var_right = input$simp_diff_var2
  #   diff_coef <- input$simp_diff_PS_Var
  #   ifelse(input$simp_diff_wayOfDiffusion, symbol <- "-->", symbol <- "<-->")
  #   
  #   textOut <- paste0(var_left, " ", symbol, "(", diff_coef, ") ", var_right)
  # }
  else if (input$eqnCreate_type_of_equation == "rate_eqn")
  {
    rate_left <- input$eqnCreate_rate_firstvar
    rate_right <- input$eqnCreate_rate_equation
    textOut <- paste0(rate_left, " = ", rate_right)
  }
  else if (input$eqnCreate_type_of_equation == "time_dependent")
  {
    TD_left <- input$eqnCreate_time_dependent_firstvar
    TD_right <- input$eqnCreate_time_dependent_equation
    textOut <- paste0(TD_left, "=", TD_right)
  }

  else{textOut <- "ERROR"}
  return(textOut)
})



