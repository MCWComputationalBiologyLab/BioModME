
waiter.eqns <- Waiter$new(id = "eqnCreate_showEquations",
                          html =  tagList(
                            div(
                              style = "color:black",
                              spin_whirly(),
                              hr(),
                              h4("Storing Equation...")
                            )
                          ),
                          color = transparent(0.7)
                          )

w.test <- Waiter$new(
  html =  tagList(
    div(
      style = "color:black",
      spin_whirly(),
      hr(),
      h4("Storing Equation...")
    )
  ),
  color = transparent(0.7)
)

CheckParametersForErrors <- function(paramsToCheck, 
                                     allSpeciesVar,
                                     allParamVariables, 
                                     onEdit = FALSE) {
  # Inputs: 
  #  @paramsToCheck - variable to be checked for conflicts
  #  @allParamVariables  - vector of parameter names
  #  @allSpeciesVar - vector of variable names
  #  @onEdit - boolean telling if this is an check on an equation edit
  # Outputs:
  #  @passed.test - boolean if parameter is good and should be stored.
  
  
  #Error Codes:
  # 0 - No Error
  # 1 - Variable name found in variable name vector
  # 2 - Variable name starts with number
  # 3 - Variable name contains special characters
  # 4 - Variable name starts with punctuation
  # 5 - Variable name found in parameter names
  # 6 - Variable name entered was all white space (no entered var)
  
  # Variables pass if error code of 5 is found but not 1,2,3,4,6
  
  
  # takes input of all parameters inputs for chem, enyzme, etc..only some will be active
  passed.test = TRUE #set true by default and change if error found
  for (var in paramsToCheck) {
    varCheck      <- variableCheck(var, allSpeciesVar, allParamVariables)
    pass.check    <- varCheck[[1]]
    error.message <- varCheck[[2]]
    error.code    <- varCheck[[3]]
    if (!pass.check) {
      if (error.code == 1 || 
          error.code == 2 || 
          error.code == 3 || 
          error.code == 4 ||
          error.code == 6) {
        # sends error and returns boolean to not store
        # errors on if parameter name == variable name, wrong punctuation, starts with number
        #   or contains special characters
        passed.test = FALSE
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = error.message,
          type = "error"
        )
        break
        # sends warning if parameter is already used, but returns store boolean
      } else if (error.code == 5) { 
        if (onEdit) {
          # Don't warning message on edit of equation
          # This is because often the parameters stay the same and its annoying
        } else {
          sendSweetAlert(
            session = session,
            title = "Warning !!!",
            text = error.message,
            type = "warning"
          )
        }
        
      }
    }
  }
  return(passed.test)
}

StoreParamsEqn <- function(parameterToAdd, pDescription = "") {
  
  #NEED TO ADD CHECK IF PARAM ALREADY EXISTS
  if (!(parameterToAdd %in% params$vars.all) &&
        !(parameterToAdd %in% params$rate.params)) {
    params$eqns.vars <- append(params$eqns.vars, parameterToAdd)
    params$eqns.vals <- append(params$eqns.vals, 0)
    params$eqns.comments <- append(params$eqns.comments, pDescription)
    
    params$vars.all <- append(params$vars.all, parameterToAdd)
    params$vals.all <- append(params$vals.all, 0)
    params$comments.all <- append(params$comments.all, pDescription)
    
    #add unique id
    ids <- GenerateId(id$id.var.seed, "parameter")
    unique.id <- ids[[2]]
    id$id.var.seed <- ids[[1]]
    idx.to.add <- nrow(id$id.parameters) + 1
    id$id.parameters[idx.to.add, ] <- c(unique.id, parameterToAdd)
    
    #add parameter to parameter table
    row.to.add <- c(parameterToAdd, 0, pDescription)
    if (nrow(params$param.table) == 0) {
      params$param.table[1,] <- row.to.add
    } else {
      params$param.table <- rbind(params$param.table, row.to.add)
    }
    loop$parameters <- params$param.table
  }
}

StoreParamsRate <- function(parameterToAdd) {
  
  if (!params$first.rate.eqn.stored) params$first.rate.eqn.stored = TRUE
  
  #NEED TO ADD CHECK IF PARAM ALREADY EXISTS
  if (!(parameterToAdd %in% params$vars.all)) {
    params$rate.eqn.vars <- append(params$rate.eqn.vars, parameterToAdd)
    params$rate.eqn.vals <- append(params$rate.eqn.vals, 0)
    params$rate.eqn.comments <- append(params$rate.eqn.comments, "")
    
    params$vars.all <- append(params$vars.all, parameterToAdd)
    params$vals.all <- append(params$vals.all, 0)
    params$comments.all <- append(params$comments.all, "")
    
    ids <- GenerateId(id$id.seed, "parameter")
    unique.id <- ids[[2]]
    id$id.seed <- ids[[1]]
    idx.to.add <- nrow(id$id.parameters) + 1
    id$id.parameters[idx.to.add, ] <- c(unique.id, parameterToAdd)
  }
  
  #add parameter to parameter table
  row.to.add <- c(parameterToAdd, 0, "")
  if (nrow(params$param.table) == 0) {
    params$param.table[1,] <- row.to.add
  } else {
    params$param.table <- rbind(params$param.table, row.to.add)
  }
  loop$parameters <- params$param.table
}

build_db_row <- function(eqn_type, RHS_coef, RHS_var, LHS_coef, LHS_var,arrow_type, kf, kr, description){
  row_out <- c(eqn_type, RHS_coef, RHS_var, LHS_coef, LHS_var,arrow_type, kf, kr, description)
}

BuildEquationSide <- function(coefUI, varUI, n) {
  # coefUI - strings of coef ui used to build equations ("2", "1" from input$LHS_coef)
  # varUI - strings of var used to build equations ("E2F", from input$LHS_Var_)
  # n - number of inputs on this side of the equation
  coefs <- vector()
  vars <- vector()
  
  for (i in seq(n)) { #find all coefficients and variables on left hand side of equation and add them to vectors
    coef <- eval(parse(text = paste0(coefUI, as.character(i))))
    var <- eval(parse(text = paste0(varUI, as.character(i))))
    coefs <- append(coefs, coef)
    vars <- append(vars, var)
  }
  coefs <- paste(coefs, collapse = " ") #paste vectors into space separated variables (ex k1 k2 k3)
  vars <- paste(vars, collapse = " ") #paste vectors into space separated variables
  
  out <- list("coefs" = coefs, "vars" = vars)
  return(out)
}

BuildRegulatorSide <- function(regUI, 
                               RC.UI, 
                               n, 
                               LHS.var, 
                               RHS.var,
                               ForwardReg) {
  # regUI - strings of regulators ui used to build equations
  # RC.UI - strings of rate constants used to build equations
  # n - number of inputs on this side of the equation
  # LHS.var - variables on the left (used for parameter description)
  # RHS.var - variables on the right
  # ForwardReg - True if forward regulator (used for description)
  regs     <- vector()
  RCs      <- vector()
  p.add    <- vector()
  d.add    <- vector()
  rc.d.add <- vector()
  
  for (i in seq(n)) { #find all coefficients and variables on left hand side of equation and add them to vectors
    reg   <- eval(parse(text = paste0(regUI, as.character(i))))
    rc    <- eval(parse(text = paste0(RC.UI, as.character(i))))
    regs  <- append(regs, reg)
    RCs   <- append(RCs, rc)
    p.add <- c(p.add, rc)
    
    if (ForwardReg) {
      rc.d  <- paste0("Rate constant for forward regulator, ",
                      reg,
                      ", on the reaction of ",
                      paste0(str_split(LHS.var, " ")[[1]], collapse = ", "),
                      " to ",
                      paste0(str_split(RHS.var, " ")[[1]], collapse = ", ")
                      )
    } else {
      rc.d  <- paste0("Rate constant for reverse regulator, ",
                      reg,
                      ", on the reaction of ",
                      paste0(str_split(LHS.var, " ")[[1]], collapse = ", "),
                      " to ",
                      paste0(str_split(RHS.var, " ")[[1]], collapse = ", ")
      )
    }
    rc.d.add <- append(rc.d.add, rc.d)
  }
  regs <- paste(regs, collapse = " ") #paste vectors into space separated variables (ex k1 k2 k3)
  RCs  <- paste(RCs, collapse = " ") #paste vectors into space separated variables
  
  
  
  out <- list("regulators"     = regs, 
              "rateConstants"  = RCs, 
              "P.to.add"       = p.add,
              "P.descriptions" = rc.d.add)
  return(out)
}

observeEvent(input$createVar_addVarToList, {
  updatePickerInput(session, "eqnCreate_recep", choices = sort(vars$species))
  updatePickerInput(session, "eqnCreate_lig", choices = sort(vars$species))

})

observeEvent(input$eqnCreate_recep, {
  updateTextInput(session, 
                  "eqnCreate_lig_recep_product", 
                  value = paste0(input$eqnCreate_recep, input$eqnCreate_lig))
})

observeEvent(input$eqnCreate_lig, {
  updateTextInput(session, 
                  "eqnCreate_lig_recep_product", 
                  value = paste0(input$eqnCreate_recep, input$eqnCreate_lig))
})


#-------------------------------------------------------------------------------

# Extract data and store equation elements into a df to solve ODEs from

#-------------------------------------------------------------------------------
observeEvent(input$eqnCreate_addEqnToVector, {
  #waiter.eqns$show()
  w.test$show()
  shinyjs::disable("eqnCreate_addEqnToVector")
  Sys.sleep(0.5)
  eqn_type           <- input$eqnCreate_type_of_equation
  p.add              <- c() # Parameter Variable Vector
  d.add              <- c() # Parameter Description Vector
  passed.error.check <- TRUE
  var.add            <- c() # Variables in model to add

  if (eqn_type == "chem_rxn") {
    #this will hold all the functions for chemical reactions:
    # Currently holds: Mass Action, Regulated Mass Action
    jPrint("chem_rxn")
    compartment = 1 #placeholder for compartments to be added in future
    
    n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS) #number of variables on RHS of equation
    n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS) #number of variables on LHS of equation
    
    if (input$eqn_chem_law == "MA") { # Mass Action
      jPrint("Mass Action")
      law = "MassAction"
      # Set regulators to null
      FM.bool <- FALSE
      FMs     <- NA
      FM.RC   <- NA
      RM.bool <- FALSE
      RMs     <- NA
      RM.RC   <- NA
      # Build left hand side of equation
      left     <- BuildEquationSide("input$LHS_Coeff_", "input$LHS_Var_", n.LHS)
      coef.LHS <- left["coefs"]
      var.LHS  <- left["vars"]
      
      # Build right hand side equation
      right    <- BuildEquationSide("input$RHS_Coeff_","input$RHS_Var_", n.RHS)
      coef.RHS <- right["coefs"]
      var.RHS  <- right["vars"]
      
      arrow <- input$eqn_chem_forward_or_both
      if (arrow == "both_directions") {
        jPrint("both directions")
          # Rate Constants
          kf    <- input$eqn_chem_forward_k
          kr    <- input$eqn_chem_back_k
          p.add <- c(p.add, kf, kr)
          
          kf.d <- paste0("Forward rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", "))
          kr.d <- paste0("Reverse rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", ")
          )
          d.add <- c(d.add, kf.d, kr.d)

      } else if (arrow == "forward_only") {
          kf    <- input$eqn_chem_forward_k
          kr    <- NA
          p.add <- c(p.add, kf)
          
          kf.d <- paste0("Forward rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", "))
          d.add <- c(d.add, kf.d)
      }
      eqn.description <- ""
      var.add <- paste(var.LHS, var.RHS)
      
    } else if (input$eqn_chem_law == 'MAwR') { # Mass Action w/ Regulation
      law = "RegulatedMA"
      n.f.reg = as.numeric(input$eqn_options_chem_num_forward_regulators) #number of regulators for forward reaction
      n.r.reg = as.numeric(input$eqn_options_chem_num_reverse_regulators) #number of regulators for reverse reaction
      
      # Build left hand side of equation
      left     <- BuildEquationSide("input$LHS_Coeff_", "input$LHS_Var_", n.LHS)
      coef.LHS <- left["coefs"]
      var.LHS  <- left["vars"]
      
      # Build right hand side equation
      right    <- BuildEquationSide("input$RHS_Coeff_","input$RHS_Var_", n.RHS)
      coef.RHS <- right["coefs"]
      var.RHS  <- right["vars"]
      
      arrow <- input$eqn_chem_forward_or_both
      if (arrow == "both_directions") {
        if (input$eqn_options_chem_modifier_forward) {
          kf      <- NA
          FM.bool <- TRUE

          f.regs <- BuildRegulatorSide("input$eqn_forward_regulator_", 
                                       "input$eqn_forward_rateConstant_", 
                                       n.f.reg,
                                       var.LHS,
                                       var.RHS,
                                       TRUE)
          FMs     <- f.regs["regulators"]
          FM.RC   <- f.regs["rateConstants"]
          p.add   <- c(p.add, f.regs["P.to.add"][[1]])
          jPrint("before extracting descriptions")
          d.add   <- c(d.add, f.regs["P.descriptions"][[1]])
          jPrint("After extracting descriptions")
          jPrint(d.add)
          FMs     <- paste(FMs, collapse = " ")
          FM.RC   <- paste(FM.RC, collapse = " ")
        } else {
          kf      <- input$eqn_chem_forward_k
          p.add   <- c(p.add, kf)
          FM.bool <- FALSE
          FMs     <- NA
          FM.RC   <- NA
          
          kf.d <- paste0("Reverse rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", ")
                        )
          d.add <- c(d.add, kf.d)
        }
        # Checks if regulator was used in reverse reaction, hence removing kr 
        # and updating the appropriate values for the regulator 
        if (input$eqn_options_chem_modifier_reverse) {
          kr      <- NA
          RM.bool <- TRUE 
   
          r.regs <- BuildRegulatorSide("input$eqn_reverse_regulator_", 
                                       "input$eqn_reverse_rateConstant_", 
                                       n.r.reg,
                                       var.LHS,
                                       var.RHS,
                                       FALSE)
          RMs     <- r.regs["regulators"]
          RM.RC   <- r.regs["rateConstants"]
          p.add   <- c(p.add, r.regs["P.to.add"][[1]])
          d.add   <- c(d.add, r.regs["P.descriptions"][[1]])
          RMs     <- paste(RMs, collapse = " ")
          RM.RC   <- paste(RM.RC, collapse = " ")
        }
        else{
          kr      <- input$eqn_chem_back_k
          RM.bool <- FALSE
          RMs     <- NA
          RM.RC   <- NA
          p.add   <- c(p.add, kr)
          
          kr.d <- paste0("Reverse rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", ")
                        )
          d.add <- c(d.add, kr.d)
        } 
      } else if (arrow == "forward_only") {
        
        # Set reverse regulator variables to NA
        kr <- NA
        RM.bool <- FALSE
        RMs <- NA
        RM.RC <- NA
        
        if (input$eqn_options_chem_modifier_forward) {
          kf      <- NA
          FM.bool <- TRUE
          
          f.regs <- BuildRegulatorSide("input$eqn_forward_regulator_", 
                                       "input$eqn_forward_rateConstant_", 
                                       n.f.reg,
                                       var.LHS,
                                       var.RHS,
                                       TRUE)
          FMs     <- f.regs["regulators"]
          FM.RC   <- f.regs["rateConstants"]
          p.add   <- c(p.add, f.regs["P.to.add"][[1]])
          d.add   <- c(d.add, f.regs["P.descriptions"][[1]])
          FMs     <- paste(FMs, collapse = " ")
          FM.RC   <- paste(FM.RC, collapse = " ")
        } else {
          kf <- input$eqn_chem_forward_k
          p.add <- c(p.add, kf)
          FM.bool <- FALSE
          FMs <- NA
          FM.RC <- NA
        }
      }

      eqn.description = ""
      to.add  <- c(var.LHS, var.RHS)
      to.add  <- to.add[!is.na(to.add)]
      var.add <- paste(to.add, collapse = " ")
    }
      
    # Add equation to df
    passed.error.check <- CheckParametersForErrors(p.add, 
                                                   vars$species, 
                                                   params$vars.all)
    
    if (passed.error.check) {
      jPrint("passed error check")
      # Store parameters to parameter vector
      for (i in seq(length(p.add))) {
        StoreParamsEqn(p.add[i], d.add[i])
      }
      jPrint("parameters stored")
      # Store up params and variables in equation
      
      # Generate eqn ID
      jPrint(id$id.eqn.seed)
      ID.gen <- GenerateId(id$id.eqn.seed, "eqn")
      jPrint(ID.gen)
      id$id.eqn.seed <- id$id.eqn.seed + 1
      ID <- ID.gen["id"]
      jPrint("ID Generated")
      #Build up Dataframe rows
      row.to.df.chem <- c(ID,
                          law,
                          coef.LHS, 
                          var.LHS, 
                          coef.RHS, 
                          var.RHS, 
                          arrow,
                          kf, 
                          kr,
                          FM.bool, 
                          FMs, 
                          FM.RC,
                          RM.bool, 
                          RMs, 
                          RM.RC
      )
      row.to.df.info <- c(ID,
                          eqn_type,
                          law,
                          var.add,
                          paste0(p.add, collapse = " "),
                          compartment,
                          eqn.description)
      
      eqns$eqn.info[eqns$n.eqns+1, ]      <- row.to.df.info
      eqns$eqn.chem[eqns$n.eqns.chem+1, ] <- row.to.df.chem
      #increment equation numbering
      eqns$n.eqns        <- eqns$n.eqns + 1
      eqns$n.eqns.chem   <- eqns$n.eqns.chem + 1
      eqns$n.eqns.no.del <- eqns$n.eqns.no.del + 1
      jPrint("Finished passed check 1")
    }
  }
  else if (eqn_type == "enzyme_rxn") {
    
    if (input$eqn_enzyme_law == "MM") {
      
      eqn.description <- ""
      compartment     <- 1
      law             <- "Michaelis Menten"
      
      substrate  <- input$eqn_enzyme_substrate
      product    <- input$eqn_enzyme_product
      Km         <- input$eqn_enzyme_Km
      arrow      <- "forward_only"
      p.add      <- c(Km)
      var.add    <- c(substrate, product)
      
      Km.d <- paste0("Michaelis Menten constant for the enzymatic conversion of ",
                               substrate,
                               " to ",
                               product
                               )
      d.add <- c(Km.d)
      
      if (!input$eqn_options_enzyme_useVmax) {
        kcat    <- input$eqn_enzyme_kcat
        enzyme  <-  input$eqn_enzyme_enzyme
        Vmax    <-  NA
        p.add   <- c(p.add, kcat)
        
        kcat.d <- paste0("Rate constant for the enzymatic conversion of ",
                         substrate,
                         " to ",
                         product
                         )
        d.add <- c(d.add, kcat.d)
        
      } else if (input$eqn_options_enzyme_useVmax) {
        Vmax   <- input$eqn_enzyme_Vmax
        kcat   <- NA
        enzyme <- NA
        p.add  <- c(p.add, Vmax)
        
        Vmax.d <- paste0("Maximum velocity for the enzymatic conversion of ",
                         substrate,
                         " to ",
                         product
                         )
        d.add <- c(d.add, Vmax.d)
      }
      
      passed.error.check <- CheckParametersForErrors(p.add, 
                                                     vars$species, 
                                                     params$vars.all)
      
      if (passed.error.check) {
        
        for (i in seq(length(p.add))) {
          StoreParamsEqn(p.add[i], pDescription = d.add[i])
        }
        
        # Generate eqn ID
        ID.gen <- GenerateId(id$id.eqn.seed, "eqn")
        id$id.eqn.seed <- id$id.eqn.seed + 1
        ID <- ID.gen["id"]
        
        row.to.df.info <- c(ID,
                            eqn_type,
                            law,
                            paste0(var.add, collapse = " "),
                            paste0(p.add, collapse = " "),
                            compartment,
                            eqn.description)
        
        row.to.df.enzyme <- c(ID,
                              law,
                              substrate,
                              product, 
                              enzyme,
                              kcat,
                              Km, 
                              Vmax)
        
        eqns$eqn.info[eqns$n.eqns+1, ]       <- row.to.df.info
        eqns$eqn.enzyme[eqns$n.eqns.enz+1, ] <- row.to.df.enzyme
        
        #increment equation numbering
        eqns$n.eqns        <- eqns$n.eqns + 1
        eqns$n.eqns.enz    <- eqns$n.eqns.enz + 1
        eqns$n.eqns.no.del <- eqns$n.eqns.no.del + 1
      }
    }
  }
  else if (eqn_type == "syn") {
    compartment <- 1
    
    if (input$eqn_syn_law == "rate") {
      
      eqn.d   <- ""
      var     <- input$eqn_syn_rate_var
      rc      <- input$eqn_syn_rate_RC
      p.add   <- c(p.add, rc)
      var.add <- c(var.add, var)
      
      rc.d    <- paste0("Synthesis rate constant for ", var)
      d.add   <- c(d.add, rc.d)
      
      factor  <- NA
    } else if (input$eqn_syn_law == "byFactor") {
      
      eqn.d   <- ""
      var     <- input$eqn_syn_sby_var
      rc      <- input$eqn_syn_sby_RC
      factor  <- input$eqn_syn_sby_factor
      p.add   <- c(p.add, rc)
      var.add <- c(var.add, var)
      
      rc.d    <- paste0("Synthesis rate constant of ", var, " by factor ", factor)
      d.add   <- c(d.add, rc.d)
      
    }
    passed.error.check <- CheckParametersForErrors(p.add, 
                                                   vars$species, 
                                                   params$vars.all)
    
    if (passed.error.check) {
      
      # Store parameters to parameter vector
      for (i in seq(length(p.add))) {
        StoreParamsEqn(p.add[i], d.add[i])
      }

      # Generate eqn ID
      ID.gen <- GenerateId(id$id.eqn.seed, "eqn")
      id$id.eqn.seed <- id$id.eqn.seed + 1
      ID <- ID.gen["id"]
      
      #Build up Dataframe rows
      row.to.df <- c(ID,
                     input$eqn_syn_law,
                     var,
                     rc, 
                     factor)
      
      row.to.df.info <- c(ID,
                          eqn_type,
                          input$eqn_syn_law,
                          paste0(var.add, collapse = " "),
                          paste0(p.add, collapse = " "),
                          compartment,
                          eqn.d)
      
      eqns$eqn.info[eqns$n.eqns+1, ]      <- row.to.df.info
      eqns$eqn.syn[eqns$n.eqns.syn+1, ]   <- row.to.df
      #increment equation numbering
      eqns$n.eqns        <- eqns$n.eqns + 1
      eqns$n.eqns.syn    <- eqns$n.eqns.syn + 1
      eqns$n.eqns.no.del <- eqns$n.eqns.no.del + 1
      
    }
  }
  else if (eqn_type == "deg") {
    
    compartment <- 1
    if (input$eqn_deg_to_products) {
      num.deg.products <- as.numeric(input$eqn_deg_num_products)
      product <- c()
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$eqn_deg_product_", as.character(i))))
        product <- c(product, prod)
      }
      var.add <- c(var.add, product)
      product <- paste0(product, collapse = " ")
    } else {
      product <- NA
    }
    
    if (input$eqn_deg_law == "rate") {
      
      eqn.d   <- ""
      var     <- input$eqn_deg_var
      rc      <- input$eqn_deg_rate_RC
      ConcDep <- input$eqn_deg_rate_conc_dependent
      p.add   <- c(p.add, rc)
      var.add <- c(var.add, var)
      
      rc.d    <- paste0("Degradation rate constant for ", var)
      d.add   <- c(d.add, rc.d)
      
      enz    <- NA
      Km     <- NA
      Vmax   <- NA
      kcat   <- NA
      
    } else if (input$eqn_deg_law == "byEnzyme") {
      
      eqn.d   <- ""
      ConcDep <- FALSE
      var     <- input$eqn_deg_var
      Km      <- input$eqn_deg_Km
      p.add   <- c(p.add, Km)
      var.add <- c(var.add, var)
      
      Km.d    <- paste0("Michelias Menten constant for degradation of ", var)
      d.add   <- c(d.add, Km.d)
      
      if (input$eqn_deg_use_Vmax) {
        Vmax  <- input$eqn_deg_Vmax
        p.add <- c(p.add, Vmax)
        rc    <- NA
        enz   <- NA
        
        Vmax.d  <- paste0("Maximum Velocity for degradation of ", var)
        d.add   <- c(d.add, Vmax.d)
        
      } else {
        rc    <- input$eqn_deg_kcat
        enz   <- input$eqn_deg_enzyme
        p.add <- c(p.add, rc)
        
        kcat.d <- paste0("Enzymatic degradation rate constant of ", var, " by  ", enz)
        d.add  <- c(d.add, kcat.d)
        Vmax <- NA
      }
    }
    passed.error.check <- CheckParametersForErrors(p.add, 
                                                   vars$species, 
                                                   params$vars.all)
    
    if (passed.error.check) {
      
      # Store parameters to parameter vector
      for (i in seq(length(p.add))) {
        StoreParamsEqn(p.add[i], d.add[i])
      }
      
      # Generate eqn ID
      ID.gen <- GenerateId(id$id.eqn.seed, "eqn")
      id$id.eqn.seed <- id$id.eqn.seed + 1
      ID <- ID.gen["id"]
      
      #Build up Dataframe rows
      row.to.df <- c(ID,
                     input$eqn_deg_law,
                     var,
                     ConcDep,
                     rc,
                     Km, 
                     enz,
                     Vmax,
                     product
                     )
      
      row.to.df.info <- c(ID,
                          eqn_type,
                          input$eqn_deg_law,
                          paste0(var.add, collapse = " "),
                          paste0(p.add, collapse = " "),
                          compartment,
                          eqn.d
                          )
      
      eqns$eqn.info[eqns$n.eqns+1, ]      <- row.to.df.info
      eqns$eqn.deg[eqns$n.eqns.deg+1, ]   <- row.to.df
      #increment equation numbering
      eqns$n.eqns        <- eqns$n.eqns + 1
      eqns$n.eqns.deg    <- eqns$n.eqns.deg + 1
      eqns$n.eqns.no.del <- eqns$n.eqns.no.del + 1
      
    }
  }
  else if (eqn_type == "simp_diff") {
    coef.LHS <- 1
    coef.RHS <- 1
    var.LHS = input$simp_diff_var1
    var.RHS = input$simp_diff_var2
    diff_coef <- input$simp_diff_PS_Var
    if (input$simp_diff_wayOfDiffusion) {
      arrow <- "forward_only"
      kf = diff_coef
      kr = NA
    }else{
      arrow <- "both_directions"
      kf = diff_coef
      kr = diff_coef
    }
    kcat = NA
    Vmax = NA 
    Km = NA 
    enzyme = NA
    FM.bool <- FALSE
    f_regulators_coef <- NA
    f_regulators_rateConstants <- NA
    RM.bool <- FALSE
    RMs <- NA
    RM.RC <- NA
    row_to_df <- c(eqn_type, coef.LHS, var.LHS, coef.RHS, var.RHS, arrow, kf, kr, 
                   kcat, Vmax, Km, enzyme,
                   FM.bool, f_regulators_coef, f_regulators_rateConstants,
                   RM.bool, RMs, RM.RC)    

    StoreParamsEqn(kf)
  }
  else if (eqn_type == "rate_eqn") {
    eqn.left   <- input$eqnCreate_custom_eqn_lhs
    eqn.right  <- input$eqnCreate_custom_eqn_rhs
    custom.eqn <- paste0(eqn.left, " = ", eqn.right)
    eqns$additional.eqns <- c(eqns$additional.eqns, custom.eqn)
  }
  else if (eqn_type == "time_dependent")
  {
    TD_left <- input$eqnCreate_time_dependent_firstvar
    TD_right <- input$eqnCreate_time_dependent_equation
    TD_eqn <- paste0(TD_left, "=", TD_right)
    eqns$additional.eqns <- c(eqns$additional.eqns, TD_eqn)
    params$parameters.based.on.other.values <- TD_left
  }

  if (passed.error.check) {
    jPrint("Storing equations to vector")
    if (eqn_type != "rate_eqn" && eqn_type != "time_dependent") {
      eqns$main <- append(eqns$main, equationBuilder())   #store selected variable to list of variables
      eqns$eqn.main.latex <- append(eqns$eqn.main.latex, equationLatexBuilder())
      eqns$eqn.main.mathjax <- append(eqns$eqn.main.mathjax, equationBuilder_MathJax())
      # ids <- GenerateId(id$id.seed, "eqn")
      # unique.id <- ids[[2]]
      # id$id.seed <- ids[[1]]
      # idx.to.add <- nrow(id$id.equations) + 1
      # id$id.equations[idx.to.add, ] <- c(unique.id, equationBuilder())
      # eqns$eqn.descriptions <- c(eqns$eqn.descriptions, "")
    }
  }
  
  #waiter.eqns$hide()
  w.test$hide()
  
  shinyjs::enable("eqnCreate_addEqnToVector")
  
  #solveForDiffEqs()
})

#-------------------------------------------------------------------------------

# Build Text Equation for User to See

#-------------------------------------------------------------------------------
equationBuilder <- reactive({
  if (input$eqnCreate_type_of_equation == "chem_rxn") {
    n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS)
    n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS)
    n.f.reg = as.numeric(input$eqn_options_chem_num_forward_regulators)
    n.r.reg = as.numeric(input$eqn_options_chem_num_reverse_regulators)

    eqn_LHS <- ""
    for (i in seq(n.LHS)) {
      coef <- eval(parse(text = paste0("input$LHS_Coeff_", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_", as.character(i))))
      if (coef != "1") {eqn_LHS <- paste0(eqn_LHS, coef, "*")}
      if (i == as.numeric(n.LHS)) {eqn_LHS <- paste0(eqn_LHS, var)}
      else{eqn_LHS <- paste0(eqn_LHS, var, " + ")}
    }

    eqn_RHS <- ""
    for (i in seq(n.RHS)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_", as.character(i))))
      if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
      if (i == as.numeric(n.RHS)) {eqn_RHS <- paste0(eqn_RHS, var)}
      else{eqn_RHS <- paste0(eqn_RHS, var, " + ")}
    }

    if (input$eqn_chem_forward_or_both == "both_directions") {
      arrow <- "<-->"
      if (input$eqn_options_chem_modifier_forward && input$eqn_options_chem_modifier_reverse) {
        #find regulators and add them together in form ([regulator/constant, regulator2/constant2, etc...])
        forwardModifiers <- c()
        for (i in seq(n.f.reg)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0(regulator, "/", rateConstant)
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")

        reverseModifiers <- c()
        for (i in seq(n.r.reg)) {
          regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_", as.character(i))))
          modifierExpression <- paste0(regulator, "/", rateConstant)
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ",")

        arrow <- paste0("([", reverseModifiers, "])", arrow, "([",forwardModifiers ,"])")
      }
      else if (input$eqn_options_chem_modifier_forward && !input$eqn_options_chem_modifier_reverse) {
        forwardModifiers <- c()
        for (i in seq(n.f.reg)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0(regulator, "/", rateConstant)
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")

        arrow <- paste0("(", input$eqn_chem_back_k, ")", arrow, "([",forwardModifiers ,"])")
      }
      else if (!input$eqn_options_chem_modifier_forward && input$eqn_options_chem_modifier_reverse) {
        reverseModifiers <- c()
        for (i in seq(n.r.reg)) {
          regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_", as.character(i))))
          modifierExpression <- paste0(regulator, "/", rateConstant)
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ",")
        arrow <- paste0("([", reverseModifiers, "])", arrow, "(", input$eqn_chem_forward_k, ")")
      }
      else
      {
        arrow <- paste0("(", input$eqn_chem_back_k, ")", arrow, "(", input$eqn_chem_forward_k, ")")
      }
    }
    else if (input$eqn_chem_forward_or_both == "forward_only") {
      arrow = "--->"
      if (input$eqn_options_chem_modifier_forward) {
        forwardModifiers <- c()
        for (i in seq(n.f.reg)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_", as.character(i))))
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_", as.character(i))))
          modifierExpression <- paste0(regulator, "/", rateConstant)
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")
        arrow <- paste0(arrow, "([",forwardModifiers ,"])")
      }
      else
      {
        arrow <- paste0(arrow, "(", input$eqn_chem_forward_k, ")")
      }
    }

    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (input$eqnCreate_type_of_equation == "enzyme_rxn") {
    substrate = input$eqn_enzyme_substrate
    product = input$eqn_enzyme_product
    arrow = "-->"
    enzyme = input$eqn_enzyme_enzyme
    Km = input$eqn_enzyme_Km

    if (!input$eqn_options_enzyme_useVmax) {
      kcat = input$eqn_enzyme_kcat
      textOut <- paste0(substrate," + ", enzyme,  " (", kcat, ")", arrow, "(", Km, ") ", product)
    }
    else if (input$eqn_options_enzyme_useVmax) {
      Vmax = input$eqn_enzyme_Vmax
      textOut <- paste0(substrate, " (", Vmax, ", Enzyme)", arrow, "(", Km, ") ", product)

    }
  }
  else if (input$eqnCreate_type_of_equation == "syn") {
    if (input$eqn_syn_law == "rate") {
      arrow <- "-->"
      var   <- input$eqn_syn_rate_var
      rc    <- input$eqn_syn_rate_RC
      type  <- "syn"
      textOut <- paste0(arrow,
                        "(", rc, ")",
                        var
      )
    } 
    else if (input$eqn_syn_law == "byFactor") {
      arrow  <- "-->"
      var    <- input$eqn_syn_sby_var
      rc     <- input$eqn_syn_sby_RC
      factor <- input$eqn_syn_sby_factor
      type   <- "syn"
      textOut <- paste0(factor,
                        arrow,
                        "(", rc, ")",
                        var
      )
    }
  }
  else if (input$eqnCreate_type_of_equation == "deg") {
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
      product <- ""
    }
    if (input$eqn_deg_law == "rate") {
      arrow <- "->"
      var   <- input$eqn_deg_var
      rc    <- input$eqn_deg_rate_RC
      type  <- "deg"
      textOut <- paste0(var,
                        arrow,
                        "(", rc, ")",
                        product
      )
      
    } else if (input$eqn_deg_law == "byEnzyme") {
      arrow <- "->"
      var   <- input$eqn_deg_var
      Km    <- input$eqn_deg_Km
      type  <- "deg"
      
      if (input$eqn_deg_use_Vmax) {
        Vmax <- input$eqn_deg_Vmax
        textOut <- paste0(var,
                          arrow,
                          "(", Km, ", ", Vmax, ")",
                          product
        )
      } else {
        enz  <- input$eqn_deg_enzyme
        kcat <- input$eqn_deg_kcat
        textOut <- paste0(var,
                          arrow,
                          "(", Km, ", ", kcat, ", ", enz, ")",
                          product
        )
      }
    }
  }
  else if (input$eqnCreate_type_of_equation == "simp_diff") {
    var_left = input$simp_diff_var1
    var_right = input$simp_diff_var2
    diff_coef <- input$simp_diff_PS_Var
    ifelse(input$simp_diff_wayOfDiffusion, symbol <- "-->", symbol <- "<-->")

    textOut <- paste0(var_left, " ", symbol, "(", diff_coef, ") ", var_right)
  }
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
  else if (input$eqnCreate_type_of_equation == "mass_bal") {
    textOut <- "MASS BAL"
  }
  else if (input$eqnCreate_type_of_equation == "lig_recep") {
    textOut <- paste0(input$eqnCreate_recep, "+", input$eqnCreate_stoch_coef, input$eqnCreate_lig, "=", input$eqnCreate_stoch_coef, input$eqnCreate_lig_recep_product)
  }
  else{textOut <- "ERROR"}
  return(textOut)
})


#-------------------------------------------------------------------------------

# Rate Equation Store Parameter/Time Dependent

#-------------------------------------------------------------------------------

# observeEvent(input$eqnCreate_rate_store_new_parameter, {
#   new_parameter <- input$eqnCreate_rate_new_parameter
#   params$rate.eqn.vars <- append(params$rate.eqn.vars, new_parameter)
#   updateTextInput(session
#                   ,"eqnCreate_rate_new_parameter"
#                   ,value = "")
# })

observeEvent(input$eqnCreate_time_dependent_store_new_parameter, {
  new_parameter <- input$eqnCreate_time_dependent_parameters
  params$time.dep.vars <- append(params$time.dep.vars, new_parameter)
  updateTextInput(session
                  ,"eqnCreate_time_dependent_parameters"
                  ,value = "")
})

#-------------------------------------------------------------------------------

# When Equation Add button pressed, store vars to respective places

#-------------------------------------------------------------------------------
observeEvent(input$eqnCreate_addEqnToVector, {
  eqn_type <- input$eqnCreate_type_of_equation
  
  #Add additional parameters in rate equation to proper rv
  if (eqn_type == "rate_eqn") {
    observe({print("rate eqn")})
    if (isTruthy(input$eqnCreate_rate_new_parameter)) { #if new parameters were entered (var1,var2,var3)
      observe({print("truthy statement")})
      num.param.to.add <- length(str_split(input$eqnCreate_rate_new_parameter, ","))
      parameters.to.add <- str_split(input$eqnCreate_rate_new_parameter, ",")
      for (i in seq(num.param.to.add)) {
        new.parameter <- gsub(" ", "", parameters.to.add[[i]], fixed = TRUE)
        phrase <- paste0("Added Param ", new.parameter)
        jPrint(phrase)
        #params$rate.eqn.vars <- append(params$rate.eqn.vars, new.parameter)
        StoreParamsRate(new.parameter)
      }
      
      #remove parameter and value and comment from paramter vectors 
      param.to.remove = input$eqnCreate_rate_firstvar
      params$rate.params <- append(params$rate.params, param.to.remove)
      #search named vector for this parameter and remove
      if (param.to.remove %in% params$eqns.vars) {
        idx.of.param = which(params$eqns.vars == param.to.remove)
        params$eqns.vars = params$eqns.vars[-idx.of.param]
        params$eqns.vals = params$eqns.vals[-idx.of.param]
        params$eqns.comments = params$eqns.comments[-idx.of.param]
        if (length(params$eqns.vars) == 0) {
           params$first.param.eqn.stored = FALSE
        }
      }
      #remove corresponding idices from list. 
      updateTextInput(session
                      ,"eqnCreate_rate_new_parameter"
                      ,value = "")
    }
    updateTextInput(session
                    ,"eqnCreate_rate_equation"
                    ,value = "")
  }

  #rate equation added in different part of code
  
  #reset text input to blank when variable entered
  eqn_type <- input$eqnCreate_type_of_equation
  n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS)
  n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS)
  nums <- c(n.RHS, n.LHS)
  out_list <- list(eqn_type, nums)

  updateNumericInput(session, "eqnCreate_num_of_eqn_LHS", value = 1)
  updateNumericInput(session, "eqnCreate_num_of_eqn_RHS", value = 1)
  
  updatePickerInput(session,'eqnCreate_edit_select_equation',choices = seq(length(eqns$main)))
  updateCheckboxInput(session,"eqn_options_chem_modifier_forward",value = FALSE)
  updateNumericInput(session, "eqn_options_chem_num_forward_regulators", value = 1)
  updateCheckboxInput(session,"eqn_options_chem_modifier_reverse",value = FALSE)
  updateNumericInput(session, "eqn_options_chem_num_reverse_regulators", value = 1)
  #updatePickerInput(session, "eqnCreate_rate_firstvar", choices = params$vars.all)

})

#-------------------------------------------------------------------------------

# Equation Text outputs

#-------------------------------------------------------------------------------

output$eqnCreate_showEquationBuilding <- renderUI({
  withMathJax(
    equationBuilder_MathJax()
  )
  })
output$test_mathjax_equations <- renderUI({
  if (length(eqns$main) == 0) {
    paste("No equations entered")
  } else {
    n_eqns = seq(length(eqns$main))
    eqns_to_display <- c()
    for (i in n_eqns) {
      new_eqn <- paste0("(",i, ") ", eqns$main[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br>")
  }
})
#output$eqnCreate_showEquations <- renderPrint({eqns$eqn.info})
output$eqnCreate_showEquations <- renderText({
  if (length(eqns$main) == 0) {
    paste("No equations entered")
  } else {
    n_eqns = seq(length(eqns$main))
    eqns_to_display <- c()
    for (i in n_eqns) {
      new_eqn <- paste0("(",i, ") ", eqns$main[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br>")
  }

})

output$eqnCreate_showAdditionalEquations <- renderText({
  if (length(eqns$additional.eqns) == 0) {
    "No additional equations entered"
  } else{
    eqns_to_display <- c()
    n_eqns = seq(length(eqns$additional.eqns))

    for (i in n_eqns) {
      new_eqn <- paste0("(",n_eqns[i], ") ", eqns$additional.eqns[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br>")
  }
})

#-------------------------------------------------------------------------------

# Removing last Equation from list

#-------------------------------------------------------------------------------
#when back button is pressed
observeEvent(input$createEqn_removeEqnFromList, {
  eqns$main <- eqns$main[-length(eqns$main)] #removes equanation from equation list
  
  #need to remove parameters
  param1 = eqns$eqn.info[nrow(eqns$eqn.info), 7] #kf
  param2 = eqns$eqn.info[nrow(eqns$eqn.info), 8] #kr
  if(!is.na(param1)){
    params$eqns.vars <- params$eqns.vars[-length(params$eqns.vars)]
    params$eqns.vals <- params$eqns.vals[-length(params$eqns.vals)]
    params$eqns.comments <- params$eqns.comments[-length(params$eqns.comments)]
  }
  if(!is.na(param2)){
    params$eqns.vars <- params$eqns.vars[-length(params$eqns.vars)]
    params$eqns.vals <- params$eqns.vals[-length(params$eqns.vals)]
    params$eqns.comments <- params$eqns.comments[-length(params$eqns.comments)]
  }
  
  #removes equation from its data matrix
  if(nrow(eqns$eqn.info)==1){ #if only row in matrix
    eqns$eqn.info <- eqns$eqn.info[-nrow(eqns$eqn.info), ] #remove equation info from data base
    eqns$first.run = TRUE #reset to be no equations
  }else{
    eqns$eqn.info <- eqns$eqn.info[-nrow(eqns$eqn.info), ] #remove equation info from data base
  }
  eqns$n.eqns<- eqns$n.eqns- 1
})

observeEvent(input$createEqn_removeFirstRate, {
  eqns$additional.eqns <- eqns$additional.eqns[-1]
})

#-------------------------------------------------------------------------------

# Delete Equation from Model

#-------------------------------------------------------------------------------
observeEvent(eqns$main, {
  updateSelectInput(session
                    ,"eqnCreate_delete_equation"
                    ,choices = as.character(seq(eqns$n.eqns)))
})


observeEvent(input$createEqn_delete_equation_button, {
  # Delete associated parameters used in this equation if they aren't used elsewhere
  eqn_to_delete <- as.numeric(input$eqnCreate_delete_equation)
  # Find parameters used in this equation
  eqn.row <- eqns$eqn.info[eqn_to_delete, 1:ncol(eqns$eqn.info)]
  # Eqn ID
  eqn.id <- eqn.row$ID[1]
  eqn.type <- eqn.row$EqnType[1]
  eqn.param <- eqn.row$RateConstants[1]

  # Find Matching Id in possible tables
  if (eqn.type == "chem_rxn") {
    for (i in 1:nrow(eqns$eqn.chem)) {
      id <- eqns$eqn.chem$ID[i]
      if (eqn.id == id){
        idx <- i
        break
      }
    }
    eqns$eqn.chem <- eqns$eqn.chem[-idx, 1:ncol(eqns$eqn.chem)] 
  } else if (eqn.type == "enzyme_rxn") {
    for (i in 1:nrow(eqns$eqn.enzyme)) {
      id <- eqns$eqn.enzyme$ID[i]
      if (eqn.id == id){
        idx <- i
        break
      }
    }
    eqns$eqn.enzyme <- eqns$eqn.enzyme[-idx, 1:ncol(eqns$eqn.enzyme)]
  } else if (eqn.type == "syn") {
    for (i in 1:nrow(eqns$eqn.syn)) {
      id <- eqns$eqn.syn$ID[i]
      if (eqn.id == id){
        idx <- i
        break
      }
    }
    eqns$eqn.syn <- eqns$eqn.syn[-idx, 1:ncol(eqns$eqn.syn)]
  } else if (eqn.type == "deg") {
    for (i in 1:nrow(eqns$eqn.deg)) {
      id <- eqns$eqn.deg$ID[i]
      if (eqn.id == id){
        idx <- i
        break
      }
    }
    eqns$eqn.deg <- eqns$eqn.deg[-idx, 1:ncol(eqns$eqn.deg)]
  }
  
  #remove equation from all sections
  eqns$eqn.info <- eqns$eqn.info[-eqn_to_delete, 1:ncol(eqns$eqn.info)] #delete equation from dataframe
  eqns$main <- eqns$main[-eqn_to_delete] #removes equation from equation list
  eqns$n.eqns <- eqns$n.eqns - 1
  eqns$eqn.descriptions <- eqns$eqn.descriptions[-eqn_to_delete]
  
  # #check to see if that parameter is used elsewhere and save it if it is
  #extract all possible parameters from eqn.info 
  p <- strsplit(eqn.param, " ")[[1]]
  p.remove <- c()
  p.save <- c()
  # Search eqns and IO for parameter
  for (param in p) {
    check1 <- ParameterSearchDF(param, eqns$eqn.info)
    check2 <- ParameterSearchDF(param, IO$input.info)
    check3 <- ParameterSearchDF(param, IO$output.info)
    if (check1 | check2 | check3) {
      p.save <- c(p.save, param)
    } else {
      p.remove <- c(p.remove, param)
    }
  }
  #if not, remove it
  for (var in p.remove) {
    DeleteParameters(var)
  }
  #if so, store in message of variables not removed
  if (length(p.save) > 0) {
    message.out <- paste0("The following parameter(s) were not deleted because they are used elsewhere: ",
                          paste0(p.save, collapse=", ")
    )
    session$sendCustomMessage(type = 'testmessage',
                              message = message.out)
  }
  my.choices <- paste0(seq(eqns$n.eqns), ") ", eqns$main)
  updatePickerInput(session,
                    "eqnCreate_selectEqnForDescription",
                    choices = my.choices)
})



#-------------------------------------------------------------------------------

# View Tab controlling the equations view

#-------------------------------------------------------------------------------
observeEvent(input$eqnCreate_addEqnToVector, {
  my.choices <- paste0(seq(eqns$n.eqns), ") ", eqns$main)
  updatePickerInput(session,
                    "eqnCreate_selectEqnForDescription",
                    choices = my.choices)
})

observeEvent(input$eqnCreate_storeEqnDescription, {
  #store current description to description vector 
  
  # find index
  idx = eqn.num <- as.numeric(str_split(input$eqnCreate_selectEqnForDescription, "\\)")[[1]][1])
  
  # find description
  text.to.store <- eval(parse(text = paste0("input$eqnDescription_", as.character(idx))))
  jPrint(text.to.store)
  jPrint(paste0("input$eqnDescription_", as.character(idx)))
  
  # store it to descriptions
  eqns$eqn.descriptions[idx] <- text.to.store
})

output$eqnCreate_eqnDescription <- renderUI({
  req(eqns$n.eqns > 0)
  eqn.num <- as.numeric(str_split(input$eqnCreate_selectEqnForDescription, "\\)")[[1]][1])
  #eqn.num = as.numeric(input$eqnCreate_selectEqnForDescription)
  
  textAreaInput(inputId = paste0("eqnDescription_", eqn.num),
                label = paste0("Description of \"", eqns$main[eqn.num], "\""),
                value = eqns$eqn.descriptions[eqn.num], 
                width = NULL,
                height = '200px')
})

output$eqnCreate_eqnDescriptionFlow <- renderUI({
  req(eqns$n.eqns > 0)
  #n.eqns <- length(eqns$main)
  n.eqns <- eqns$n.eqns
  
  lapply(seq(n.eqns), function(i){
    textAreaInput(inputId = paste0("eqnDescriptionFlow_", i),
                  label = paste0(i,") Description of \"", eqns$main[i], "\""),
                  value = eqns$eqn.descriptions[i], 
                  width = NULL,
                  height = '200px')
  })
})

observeEvent(input$view_eqns_debug, {
  jPrint(eqns$eqn.info)
  jPrint(eqns$eqn.chem)
  jPrint(eqns$eqn.enzyme)
  jPrint(eqns$eqn.syn)
  jPrint(eqns$eqn.deg)
  jPrint(eqns$eqn.main.latex)
})

observeEvent(input$refresh_text_eqns, {
  # Run new functions to rewrite RV storing text eqns
  jPrint(eqns$main)
  eqns$main <- ReCalcTextEqns(eqns$eqn.info,
                              eqns$eqn.chem,
                              eqns$eqn.enzyme,
                              eqns$eqn.syn,
                              eqns$eqn.deg,
                              eqns$main)
  jPrint("After Rewrite")
  jPrint(eqns$main)
})

#--------------------------Random----------------------------------------------

