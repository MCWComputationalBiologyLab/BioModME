
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
      h4("Storing Reaction...")
    )
  ),
  color = transparent(0.7)
)

CheckParametersForErrors <- function(parameter, 
                                     speciesList,
                                     parameterList,
                                     compartmentList,
                                     allowRepeatParams = TRUE,
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
  repeated.parameters <- TRUE

  varCheck      <- parameterCheck(parameter, 
                                  speciesList,
                                  parameterList,
                                  compartmentList,
                                  allowRepeatParams)
  # Inputs: 
  #  @parameter - new parameter entry to check (whole list entry)
  #  @currentVarList - species RV (rv.SPECIES$species)
  #  @parameterList  - parameter RV(rv.PARAMETERS$parameters)
  #  @compartmentList - compartment RV (rv.COMPARTMENT$compartments)
  
  pass.check    <- varCheck[[1]]
  error.message <- varCheck[[2]]
  error.code    <- varCheck[[3]]
  repeat.param  <- varCheck[[4]]
  
  if (repeat.param) {repeated.parameters <- TRUE}
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
      
      # sends warning if parameter is already used, but returns store boolean
    } else if (error.code == 5) { 
      if (onEdit) {
        # Don't warning message on edit of equation
        # This is because often the parameters stay the same and its annoying
      } else {
        passed.test = FALSE
        sendSweetAlert(
          session = session,
          title = "Warning !!!",
          text = error.message,
          type = "warning"
        )
      }
    }
  }
  
  out <- list(passed.test, repeated.parameters)
  return(out)
}

# CheckParametersForErrors <- function(paramsToCheck, 
#                                      allSpeciesVar,
#                                      allParamVariables,
#                                      allowRepeatParams = FALSE,
#                                      onEdit = FALSE) {
#   # Inputs: 
#   #  @paramsToCheck - variable to be checked for conflicts
#   #  @allParamVariables  - vector of parameter names
#   #  @allSpeciesVar - vector of variable names
#   #  @onEdit - boolean telling if this is an check on an equation edit
#   # Outputs:
#   #  @passed.test - boolean if parameter is good and should be stored.
#   
#   
#   #Error Codes:
#   # 0 - No Error
#   # 1 - Variable name found in variable name vector
#   # 2 - Variable name starts with number
#   # 3 - Variable name contains special characters
#   # 4 - Variable name starts with punctuation
#   # 5 - Variable name found in parameter names
#   # 6 - Variable name entered was all white space (no entered var)
#   
#   # Variables pass if error code of 5 is found but not 1,2,3,4,6
#   
#   # takes input of all parameters inputs for chem, enyzme, etc..only some will be active
#   passed.test = TRUE #set true by default and change if error found
#   repeated.parameters <- TRUE
#   for (var in paramsToCheck) {
#     varCheck      <- parameterCheck(parameter, 
#                                     speciesList,
#                                     parameterList,
#                                     compartmentList,
#                                     allowRepeatParams)
#     # Inputs: 
#     #  @parameter - new parameter entry to check (whole list entry)
#     #  @currentVarList - species RV (rv.SPECIES$species)
#     #  @parameterList  - parameter RV(rv.PARAMETERS$parameters)
#     #  @compartmentList - compartment RV (rv.COMPARTMENT$compartments)
#     
#     pass.check    <- varCheck[[1]]
#     error.message <- varCheck[[2]]
#     error.code    <- varCheck[[3]]
#     repeat.param  <- varCheck[[4]]
#     
#     if (repeat.param) {repeated.parameters <- TRUE}
#     if (!pass.check) {
#       if (error.code == 1 || 
#           error.code == 2 || 
#           error.code == 3 || 
#           error.code == 4 ||
#           error.code == 6) {
#         # sends error and returns boolean to not store
#         # errors on if parameter name == variable name, wrong punctuation, starts with number
#         #   or contains special characters
#         passed.test = FALSE
#         sendSweetAlert(
#           session = session,
#           title = "Error...",
#           text = error.message,
#           type = "error"
#         )
#         break
#         # sends warning if parameter is already used, but returns store boolean
#       } else if (error.code == 5) { 
#         if (onEdit) {
#           # Don't warning message on edit of equation
#           # This is because often the parameters stay the same and its annoying
#         } else {
#           passed.test = FALSE
#           sendSweetAlert(
#             session = session,
#             title = "Warning !!!",
#             text = error.message,
#             type = "warning"
#           )
#         }
#       }
#     }
#   }
#   out <- list(passed.test, repeated.parameters)
#   return(out)
# }



BuildParameters <- function(pToAdd,
                            pAll,
                            idSeed,
                            pValue = 0, 
                            pDescription = "", 
                            pUnit = "pH",
                            pUnitD = "num <div> time",
                            pBaseUnit = "BASE",
                            pBaseValue = 0,
                            pLocation = "reactionType",
                            pLocationNote = "") {
  
  if (!(pToAdd %in% pAll)) {
    # Generate Parameter ID
    ids <- GenerateId(idSeed, "parameter")
    id <- ids$id

    # Add Parameter to Parameter List
    nPar <- length(pAll)
    pAll[nPar + 1] <- pToAdd
    p.list.entry <- list(Name = pToAdd,
                         ID = id,
                         Value = pValue,
                         Unit = pUnit,
                         UnitD = pUnitD,
                         BaseUnit = pBaseUnit,
                         BaseValue = pBaseValue,
                         Description = pDescription,
                         Type = pLocation,
                         TypeNote = pLocationNote)
    
    # Assign List Name
    names(p.list.entry) <- pToAdd
    
    # Add Row to Parameter Table
    row.to.add <- c(pToAdd, 
                    pValue, 
                    pUnit, 
                    pDescription)
    passed.check <- TRUE
  } else {
    passed.check <- FALSE
    p.list.entry <- NULL
    row.to.add <- NULL
  }
  
  out <- list(passed = passed.check,
              par.id = id,
              par.all = pAll,
              p.entry = p.list.entry,
              row.for.datatable = row.to.add)
}

StoreParameters <- function(BuildParmetersOutput) {
  
  # Unpack Output
  passed    <- BuildParmetersOutput$passed
  par.id    <- BuildParmetersOutput$par.id
  par.all   <- BuildParmetersOutput$par.all
  p.entry   <- BuildParmetersOutput$p.entry
  row.2.add <- BuildParmetersOutput$row.for.datatable
  
  nPar      <- length(par.all)

  names(p.entry) <- c("Name", 
                      "ID", 
                      "Value", 
                      "Unit",
                      "UnitDescription",
                      "BaseUnit",
                      "BaseValue",
                      "Description", 
                      "Type",
                      "Type.Note")

  # Store Params to List
  rv.PARAMETERS$parameters[[nPar]] <- p.entry
  names(rv.PARAMETERS$parameters)[nPar] <- par.id
  # Add to Parameter Ids
  rv.ID$id.param.seed <- rv.ID$id.param.seed + 1
  rv.ID$id.df[nrow(rv.ID$id.df) + 1,] <- c(par.id, p.entry$Name)
  
  # Rewrite the loop parameter table
  # rv.PLOT.LOOP$loop.parameters <- rv.PARAMETERS$parameters.df %>% 
  #   select("Name", "Value", "Unit", "Description")
}

StoreParamsEqn <- function(pToAdd, 
                           pValue = 0, 
                           pDescription = "", 
                           pUnit = "pH",
                           pLocation = "reactionType") {
  
  if (!(pToAdd %in% names(rv.PARAMETERS$parameters))) {
    # Generate Parameter ID
    ids <- GenerateId(rv.ID$id.var.seed, "parameter")
    unique.id <- ids[[2]]
    rv.ID$id.var.seed <- ids[[1]]
    idx.to.add <- nrow(rv.ID$id.df) + 1
    rv.ID$id.df[idx.to.add, ] <- c(unique.id, pToAdd)
    # Add Parameter to Parameter List
    nPar <- length(rv.PARAMETERS$parameters)
    rv.PARAMETERS$parameters[[nPar + 1]] <- list(Name = pToAdd,
                                      ID = ids[[1]],
                                      Value = pValue,
                                      Unit = pUnit,
                                      Description = pDescription,
                                      Type = pLocation)

    # Assign List Name
    names(rv.PARAMETERS$parameters)[nPar + 1] <- pToAdd

    # Rewrite the loop parameter table
    # rv.PLOT.LOOP$loop.parameters <- rv.PARAMETERS$parameters.df %>% 
    #   select("Name", "Value", "Unit", "Description")
    
  }
}

StoreParamsRate <- function(parameterToAdd) {
  
  if (!rv.PARAMETERS$first.rate.eqn.stored) rv.PARAMETERS$first.rate.eqn.stored = TRUE

}

build_db_row <- function(eqn_type, RHS_coef, RHS_var, LHS_coef,
                         LHS_var,arrow_type, kf, kr, description){
  row_out <- c(eqn_type, RHS_coef, RHS_var, LHS_coef, LHS_var,arrow_type,
               kf, kr, description)
}

BuildEquationSide <- function(coefUI, varUI, n) {
  # coefUI - strings of coef ui used to build equations ("2", "1" from input$LHS_coef)
  # varUI - strings of var used to build equations ("E2F", from input$LHS_Var_)
  # n - number of inputs on this side of the equation
  coefs <- vector()
  vars <- vector()
  ids <- vector()
  
  for (i in seq(n)) { #find all coefficients and variables on left hand side of equation and add them to vectors
    coef <- eval(parse(text = paste0(coefUI, as.character(i))))
    var <- eval(parse(text = paste0(varUI, as.character(i))))
    coefs <- append(coefs, coef)
    vars <- append(vars, var)
    ids <- append(ids, FindId(var))
  }
  coefs <- paste(coefs, collapse = ", ") #paste vectors into space separated variables (ex k1 k2 k3)
  vars <- paste(vars, collapse = ", ") #paste vectors into space separated variables
  ids   <- paste(ids, collapse = ", ")
  
  out <- list("coefs" = coefs, 
              "vars" = vars,
              "ids" = ids)
  return(out)
}

BuildRegulatorSide <- function(regUI, 
                               RC.UI,
                               RC.val,
                               n, 
                               LHS.var, 
                               RHS.var,
                               ForwardReg) {
  # regUI - strings of regulators ui used to build equations
  # RC.UI - strings of rate constants used to build equations
  # RC.val - strings of rate constant values 
  # n - number of inputs on this side of the equation
  # LHS.var - variables on the left (used for parameter description)
  # RHS.var - variables on the right
  # ForwardReg - True if forward regulator (used for description)
  regs        <- vector()
  RCs         <- vector()
  vals        <- vector()
  p.add       <- vector()
  rc.descript <- vector()
  ids         <- vector()
  
  # Find all coefficients and variables on left hand side of equation
  # and add them to vectors
  for (i in seq(n)) { 
    reg   <- eval(parse(text = paste0(regUI, as.character(i))))
    rc    <- eval(parse(text = paste0(RC.UI, as.character(i))))
    val   <- eval(parse(text = paste0(RC.val, as.character(i))))
    
    regs  <- append(regs, reg)
    RCs   <- append(RCs, rc)
    vals  <- append(vals, val)
    
    ids   <- c(ids, FindId(reg))
    
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
    rc.descript <- append(rc.descript, rc.d)
  }
  # regs <- paste0(regs, collapse = ", ") 
  # RCs  <- paste0(RCs, collapse = ", ") 
  # ids  <- paste0(ids, collapse = ", ")
  # vals  <- paste0(vals, collapse = ", ")
  
  out <- list("regulators"     = regs, 
              "rateConstants"  = RCs,
              "regulator.val"  = vals,
              "rc.descript"    = rc.descript,
              "reg.ids"        = ids)
  return(out)
}

observeEvent(input$createVar_addVarToList, {
  updatePickerInput(
    session, 
    "eqnCreate_recep", 
    choices = sort(rv.SPECIES$species.names))
  updatePickerInput(
    session, 
    "eqnCreate_lig", 
    choices = sort(rv.SPECIES$species.names))
})

observeEvent(input$eqnCreate_recep, {
  updateTextInput(
    session,
    "eqnCreate_lig_recep_product",
    value = paste0(input$eqnCreate_recep, input$eqnCreate_lig)
  )
})

observeEvent(input$eqnCreate_lig, {
  updateTextInput(
    session,
    "eqnCreate_lig_recep_product",
    value = paste0(input$eqnCreate_recep, input$eqnCreate_lig)
  )
})


# Reactive Variable Filtering By Compartment -----------------------------------

observeEvent({input$eqnCreate_active_compartment
              rv.COMPARTMENTS$compartments
              rv.SPECIES$species}, {
  req(!is_empty(rv.SPECIES$species.df))

  rv.SPECIES$df.by.compartment <- 
    rv.SPECIES$species.df %>% 
    filter(Compartment == input$eqnCreate_active_compartment)
})

# Event: Reaction Law Change
# observeEvent(input$eqnCreate_reaction_law, {
#   
#   # Want to hide button when on Create cusomt
#   if (input$eqnCreate_reaction_law == "create_custom") {
#     
#   } else {
#     
#   }
#   
# })



# Add Reaction Event -----------------------------------------------------------
observeEvent(input$eqnCreate_addEqnToVector, {
  # This event stores all equation information to their respective RVs for 
  # later analysis. This RVs are parsed in many places including the 
  # differential equation solver, export, and import functions. 
  
  # The event is sorted by reaction type where the app data is extracted, 
  # then passed through an error checker, and then stored in its respective 
  # places. 
  
  # Changes to this event will usually cause the need for changes in:
  #   equationLatexBuilder
  #   equationMathjaxBuilder
  #   equationTextBuilder
  #   differential solver scripts
  #   smbl load parsers
  #   Edit scripts: modal, solver, textBuilder
  #   Parameter table change, need to add RV storages for new equations
  
  # browser()
  
  #waiter.rv.REACTIONS$show()
  w.test$show()
  shinyjs::disable("eqnCreate_addEqnToVector")
  Sys.sleep(0.5)
  
  eqn_type           <- input$eqnCreate_reaction_law
  # Storage Vectors to build equation parts
  parameters          <- c() # Parameter Variable Vector
  param.vals          <- c() # Parameter Values
  param.units         <- c() # parameter Unit Vector
  unit.descriptions   <- c() # Parameter Unit Breakdown Vector
  param.descriptions  <- c() # Parameter Description Vector
  base.units          <- c() # Base Unit for calculations
  base.values         <- c() # Base Unit Values
  species             <- c() # Variables in model to add
  parameters.id       <- c() # Parameter Ids
  species.id          <- c() # Variable Ids
  passed.error.check  <- TRUE
  
  # Get Compartment information
  compartment    <- input$eqnCreate_active_compartment
  compartment.id <- FindId(compartment)
  
  # Equation Reaction Schemes
  text.eqn    <- equationBuilder()
  latex.eqn   <- equationLatexBuilder()
  mathjax.eqn <- equationMathJaxBuilder()
  
  # Find Volume Variable
  volume.var <- rv.COMPARTMENTS$compartments[[compartment.id]]$Volume
  
  # Initalize reactants/products
  reactants    <- NA
  reactants.id <- NA
  products     <- NA
  products.id  <- NA
  isReversible <- FALSE
  
  # Mass Action
  if (input$eqnCreate_reaction_law == "mass_action") {
    reaction.id <- NA
    eqn.display <- "Mass Action"
    backend.call <- "mass_action"
    # browser()
    # browser()
    modifiers    <- NA
    modifiers.id <- NA
    
    number.reactants <- as.numeric(input$NI_mass_action_num_reactants)
    number.products  <- as.numeric(input$NI_mass_action_num_products)
    
    # Build left hand side of equation
    left     <- BuildEquationSide("input$NI_MA_r_stoichiometry_", 
                                  "input$PI_MA_reactant_", 
                                  number.reactants)
    r.stoich      <- left[["coefs"]]
    reactants     <- left[["vars"]]
    reactants.id  <- left[["ids"]]
    
    # Build right hand side equation
    right    <- BuildEquationSide("input$NI_MA_p_stoichiometry_",
                                  "input$PI_MA_product_", 
                                  number.products)
    p.stoich    <- right[["coefs"]]
    products    <- right[["vars"]]
    products.id <- right[["ids"]]
    
    eqn.description <- ""
    eqn.d <- "Mass Action Reaction"
    species    <- c(strsplit(reactants, ", ")[[1]], 
                    strsplit(products, ", ")[[1]])
    species.id <- c(strsplit(reactants.id, ", ")[[1]],
                    strsplit(products.id, ", ")[[1]])
    
    # Find Kf information
    kf    <- input$TI_mass_action_forward_k

    # Rate Constant Values
    kf.val <- input$TI_mass_action_forward_k_value
    # Build Rate Constant Units
    kf.unit <- DetermineRateConstantUnits(
      r.stoich,
      rv.UNITS$units.base$For.Var,
      rv.UNITS$units.base$Volume,
      rv.UNITS$units.base$Duration,
      rv.UNITS$units.selected$For.Var,
      rv.UNITS$units.selected$Volume,
      rv.UNITS$units.selected$Duration
    )
    # Convert rate constant units if necessary
    if (kf.unit$unit != kf.unit$unit.base) {
      kf.base.val <- UnitConversion(kf.unit$unit.description,
                                    kf.unit$unit,
                                    kf.unit$unit.base,
                                    as.numeric(kf.val))
    } else {
      kf.base.val <- kf.val
    }
    
    # Write Unit Descriptions
    kf.d <- paste0("Forward rate constant for the reaction of ",
                   reactants,
                   " to ",
                   products)
    
    parameters         <- c(parameters, kf)
    param.vals         <- c(param.vals, kf.val)
    param.units        <- c(param.units, kf.unit$unit)
    unit.descriptions  <- c(unit.descriptions, kf.unit$unit.description)
    param.descriptions <- c(param.descriptions, kf.d)
    base.units         <- c(base.units, kf.unit$unit.base)
    base.values        <- c(base.values, kf.base.val)
    
    reversible <- input$PI_mass_action_reverisble_option
    if (reversible == "both_directions") {
      isReversible <- TRUE
      # If the reaction is reversible then we need to build the reverse
      # rate constant for the reaction
      kr     <- input$TI_mass_action_reverse_k
      kr.val <- input$TI_mass_action_reverse_k_value
      
      # Build Rate Constant Units
      kr.unit <- DetermineRateConstantUnits(
        p.stoich,
        rv.UNITS$units.base$For.Var,
        rv.UNITS$units.base$Volume,
        rv.UNITS$units.base$Duration,
        rv.UNITS$units.selected$For.Var,
        rv.UNITS$units.selected$Volume,
        rv.UNITS$units.selected$Duration
      )
      
      # Convert rate constant units if necessary
      if (kr.unit$unit != kr.unit$unit.base) {
        kr.base.val <- UnitConversion(kr.unit$unit.description,
                                      kr.unit$unit,
                                      kr.unit$unit.base,
                                      as.numeric(kr.val))
      } else {
        kr.base.val <- kr.val
      }
      
      # Write Unit Descriptions
      kr.d <- paste0("Reverse rate constant for the reaction of ",
                     reactants,
                     " to ",
                     products
      )
      
      parameters         <- c(parameters, kr)
      param.vals         <- c(param.vals, kr.val)
      param.units        <- c(param.units,kr.unit$unit)
      unit.descriptions  <- c(unit.descriptions, kr.unit$unit.description)
      param.descriptions <- c(param.descriptions, kr.d)
      base.units         <- c(base.units, kr.unit$unit.base)
      base.values        <- c(base.values, kr.base.val)
      
    } 
    else if (reversible == "forward_only") {
      kr     <- NA
      kr.val <- NA
    }
    # browser()
    # Build Rate Law
    laws <- Law_Of_Mass_Action(r.stoich,
                               reactants,
                               p.stoich,
                               products,
                               reversible,
                               kf,
                               kr,
                               volume.var)
    
    # Extract reaction laws 
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    content.ml  <- laws$content.ml

  } 
  else if (input$eqnCreate_reaction_law == "exponential_growth") {
    reaction.id  <- NA
    eqn.display  <- "Exponential Growth"
    backend.call <- "exponential_growth"
    modifiers    <- NA
    modifiers.id <- NA
    reactants    <- NA
    reactants.id <- NA
    products     <- NA
    products.id  <- NA
    isReversible <- FALSE
    
    growth.species    <- input$PI_exp_growth_species
    growth.species.id <- FindId(growth.species)
    species           <- growth.species
    species.id        <- growth.species.id
    
    mu.name     <- input$TI_exp_growth_mu
    mu.val      <- input$NI_exp_growth_mu_value
    unit.description <- "num <div> time"
    base.unit   <- paste0("1/", rv.UNITS$units.base$Duration)
    param.unit  <- paste0("1/", rv.UNITS$units.selected$Duration)
    param.description <- paste0("Specific growth rate for ", growth.species)
    
    if (param.unit != base.unit) {
      base.val <- UnitConversion(unit.description,
                                 param.unit,
                                 base.unit,
                                 as.numeric(mu.val))
    } else {
      base.val <- mu.val
    }
    
    parameters         <- c(parameters, mu.name)
    param.vals         <- c(param.vals, mu.val)
    param.units        <- c(param.units, param.unit)
    unit.descriptions  <- c(unit.descriptions, unit.description)
    param.descriptions <- c(param.descriptions, param.description)
    base.units         <- c(base.units, base.unit)
    base.values        <- c(base.values, base.val)
    
    rate.law    <- paste0(mu.name, "*", growth.species)
    p.rate.law  <- rate.law
    latex.law   <- paste0(mu.name, "\\cdot ", growth.species)
    mathjax.law <- paste0(Var2MathJ(mu.name), "*", Var2MathJ(growth.species))
    mathml.law  <- NA
    content.ml  <- NA
    eqn.d       <- "Exponential growth dX/dt = mu*X"
  }
  else if (input$eqnCreate_reaction_law == "monod_growth") {
    reaction.id  <- NA
    eqn.display  <- "Monod Growth"
    backend.call <- "monod_growth"
    modifiers    <- NA
    modifiers.id <- NA
    isReversible <- FALSE
    
    growth.species    <- input$PI_monod_species
    growth.species.id <- FindId(growth.species)
    substrate         <- input$PI_monod_substrate
    substrate.id     <- FindId(substrate)
    species           <- c(growth.species, substrate)
    species.id        <- c(growth.species.id, substrate.id)
    
    # Substrate is consumed (reactant), growing species is produced (product)
    reactants    <- substrate
    reactants.id <- substrate.id
    products     <- growth.species
    products.id  <- growth.species.id
    
    mu_max.name     <- input$TI_monod_mu_max
    mu_max.val      <- input$NI_monod_mu_max_value
    unit.description.mu <- "num <div> time"
    base.unit.mu    <- paste0("1/", rv.UNITS$units.base$Duration)
    param.unit.mu   <- paste0("1/", rv.UNITS$units.selected$Duration)
    param.description.mu <- paste0("Maximum specific growth rate for ", growth.species)
    
    if (param.unit.mu != base.unit.mu) {
      base.val.mu <- UnitConversion(unit.description.mu,
                                    param.unit.mu,
                                    base.unit.mu,
                                    as.numeric(mu_max.val))
    } else {
      base.val.mu <- mu_max.val
    }
    
    K_s.name     <- input$TI_monod_K_s
    K_s.val      <- input$NI_monod_K_s_value
    unit.K_s     <- rv.UNITS$units.selected$For.Var
    base.K_s     <- rv.UNITS$units.base$For.Var
    unit.description.K_s <- paste0("conc (", base.K_s, ")")
    param.description.K_s <- paste0("Half-saturation constant for ", substrate)
    
    if (unit.K_s != base.K_s) {
      base.val.K_s <- UnitConversion(unit.description.K_s,
                                     unit.K_s,
                                     base.K_s,
                                     as.numeric(K_s.val))
    } else {
      base.val.K_s <- K_s.val
    }
    
    parameters         <- c(parameters, mu_max.name, K_s.name)
    param.vals         <- c(param.vals, mu_max.val, K_s.val)
    param.units        <- c(param.units, param.unit.mu, unit.K_s)
    unit.descriptions  <- c(unit.descriptions, unit.description.mu, unit.description.K_s)
    param.descriptions <- c(param.descriptions, param.description.mu, param.description.K_s)
    base.units         <- c(base.units, base.unit.mu, base.K_s)
    base.values        <- c(base.values, base.val.mu, base.val.K_s)
    
    # Rate law: mu_max * X * S / (K_s + S)
    rate.law    <- paste0(mu_max.name, "*", growth.species, "*", substrate, "/(", K_s.name, "+", substrate, ")")
    p.rate.law  <- rate.law
    latex.law   <- paste0(mu_max.name, "\\cdot ", growth.species, "\\cdot \\frac{", substrate, "}{", K_s.name, "+", substrate, "}")
    mathjax.law <- paste0(Var2MathJ(mu_max.name), "*", Var2MathJ(growth.species), "*\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.name), "+", Var2MathJ(substrate), "}")
    mathml.law  <- NA
    content.ml  <- NA
    eqn.d       <- paste0("Monod growth d", growth.species, "/dt = ", mu_max.name, "*", growth.species, "*", substrate, "/(", K_s.name, "+", substrate, ")")
  }
  else if (input$eqnCreate_reaction_law == "competitive_monod") {
    # Check if single species mode (only X grows competitively)
    single.species.mode <- isTruthy(input$CB_comp_monod_single_species)
    # Check if substrate consumption should exclude competitive restriction
    no.substrate.restriction <- isTruthy(input$CB_comp_monod_no_substrate_restriction)
    
    # Build three species-specific rate laws: X, Y, and S (or just X and S in single species mode)
    reaction.id  <- NA
    eqn.display  <- if (single.species.mode) "Competitive Monod Growth (Single Species)" else "Competitive Monod Growth"
    backend.call <- "competitive_monod"
    modifiers    <- NA
    modifiers.id <- NA
    isReversible <- FALSE
    skip.reaction.entry <- TRUE
    
    # Use different input IDs based on mode
    if (single.species.mode) {
      species.x    <- input$PI_comp_monod_species_x_2
      species.y    <- input$PI_comp_monod_species_y_2
      substrate    <- input$PI_comp_monod_substrate_2
    } else {
      species.x    <- input$PI_comp_monod_species_x
      species.y    <- input$PI_comp_monod_species_y
      substrate    <- input$PI_comp_monod_substrate
    }
    species.id.x <- FindId(species.x)
    species.id.y <- FindId(species.y)
    substrate.id <- FindId(substrate)
    
    if (single.species.mode) {
      # Only X grows competitively, Y is a modifier
      species      <- c(species.x, substrate)
      species.id   <- c(species.id.x, substrate.id)
      modifiers    <- species.y
      modifiers.id <- species.id.y
    } else {
      # Both species compete
      species      <- c(species.x, species.y, substrate)
      species.id   <- c(species.id.x, species.id.y, substrate.id)
    }
    
    # Parameters
    mu_max.x.name  <- input$TI_comp_monod_mu_max_x
    mu_max.x.val   <- input$NI_comp_monod_mu_max_x_value
    K_s.x.name     <- input$TI_comp_monod_K_s_x
    K_s.x.val      <- input$NI_comp_monod_K_s_x_value
    alpha.xy.name  <- input$TI_comp_monod_alpha_xy
    alpha.xy.val   <- input$NI_comp_monod_alpha_xy_value
    Kc.name        <- input$TI_comp_monod_Kc
    Kc.val         <- input$NI_comp_monod_Kc_value
    Y_x.name       <- input$TI_comp_monod_Y_x
    Y_x.val        <- input$NI_comp_monod_Y_x_value
    
    # Units: mu_max 1/time, K_s same as substrate, alpha dimensionless, Kc same as species, Y dimensionless
    unit.description.mu <- "num <div> time"
    base.unit.mu        <- paste0("1/", rv.UNITS$units.base$Duration)
    unit.mu             <- paste0("1/", rv.UNITS$units.selected$Duration)
    
    unit.K_s     <- rv.UNITS$units.selected$For.Var
    base.K_s     <- rv.UNITS$units.base$For.Var
    unit.description.K_s <- paste0("conc (", base.K_s, ")")
    
    unit.Kc      <- rv.UNITS$units.selected$For.Var
    base.Kc      <- rv.UNITS$units.base$For.Var
    
    addParam <- function(name, val, unit, base.unit, unit.desc, desc){
      if (unit != base.unit) {
        base.val <- UnitConversion(unit.desc, unit, base.unit, as.numeric(val))
      } else { base.val <- val }
      list(name=name,val=val,unit=unit,base.unit=base.unit,unit.desc=unit.desc,
           base.val=base.val, desc=desc)
    }
    
    p.mu_max.x <- addParam(mu_max.x.name, mu_max.x.val, unit.mu, base.unit.mu, unit.description.mu,
                          paste0("Maximum growth rate of ", species.x))
    p.K_s.x    <- addParam(K_s.x.name, K_s.x.val, unit.K_s, base.K_s, unit.description.K_s,
                          paste0("Half-saturation constant for ", species.x))
    p.alpha.xy <- addParam(alpha.xy.name, alpha.xy.val, "dimensionless", "dimensionless",
                          "dimensionless", paste0("Effect of ", species.y, " on ", species.x))
    p.Kc       <- addParam(Kc.name, Kc.val, unit.Kc, base.Kc,
                          paste0("conc (", base.Kc, ")"),
                          "Community carrying capacity")
    p.Y_x      <- addParam(Y_x.name, Y_x.val, "dimensionless", "dimensionless",
                          "dimensionless", paste0("Yield coefficient for ", species.x))
    
    if (single.species.mode) {
      # Single species mode: only X parameters
      pack <- list(p.mu_max.x, p.K_s.x, p.alpha.xy, p.Kc, p.Y_x)
    } else {
      # Both species mode: need all parameters
      mu_max.y.name  <- input$TI_comp_monod_mu_max_y
      mu_max.y.val   <- input$NI_comp_monod_mu_max_y_value
      K_s.y.name     <- input$TI_comp_monod_K_s_y
      K_s.y.val      <- input$NI_comp_monod_K_s_y_value
      alpha.yx.name  <- input$TI_comp_monod_alpha_yx
      alpha.yx.val   <- input$NI_comp_monod_alpha_yx_value
      Y_y.name       <- input$TI_comp_monod_Y_y
      Y_y.val        <- input$NI_comp_monod_Y_y_value
      
      p.mu_max.y <- addParam(mu_max.y.name, mu_max.y.val, unit.mu, base.unit.mu, unit.description.mu,
                            paste0("Maximum growth rate of ", species.y))
      p.K_s.y    <- addParam(K_s.y.name, K_s.y.val, unit.K_s, base.K_s, unit.description.K_s,
                            paste0("Half-saturation constant for ", species.y))
      p.alpha.yx <- addParam(alpha.yx.name, alpha.yx.val, "dimensionless", "dimensionless",
                            "dimensionless", paste0("Effect of ", species.x, " on ", species.y))
      p.Y_y      <- addParam(Y_y.name, Y_y.val, "dimensionless", "dimensionless",
                            "dimensionless", paste0("Yield coefficient for ", species.y))
      pack <- list(p.mu_max.x, p.mu_max.y, p.K_s.x, p.K_s.y, p.alpha.xy, p.alpha.yx, p.Kc, p.Y_x, p.Y_y)
    }
    
    for (p in pack){
      parameters         <- c(parameters, p$name)
      param.vals         <- c(param.vals, p$val)
      param.units        <- c(param.units, p$unit)
      unit.descriptions  <- c(unit.descriptions, p$unit.desc)
      param.descriptions <- c(param.descriptions, p$desc)
      base.units         <- c(base.units, p$base.unit)
      base.values        <- c(base.values, p$base.val)
    }
    
    # Rate laws:
    # For X: μ_max_x * X * S / (K_s_x + S) * (1 - (X + α_xy * Y) / K_c)
    rate.law.x <- paste0(mu_max.x.name, "*", species.x, "*", substrate, "/(", K_s.x.name, "+", substrate, ")*(1-(", species.x, "+", alpha.xy.name, "*", species.y, ")/", Kc.name, ")")
    # For S consumption from X: Y_x * (growth rate of X) - ODE derivation will add negative sign for reactant
    # If no.substrate.restriction is TRUE, remove competitive term from substrate consumption
    if (no.substrate.restriction) {
      rate.law.s.x <- paste0(Y_x.name, "*", mu_max.x.name, "*", species.x, "*", substrate, "/(", K_s.x.name, "+", substrate, ")")
    } else {
      rate.law.s.x <- paste0(Y_x.name, "*", mu_max.x.name, "*", species.x, "*", substrate, "/(", K_s.x.name, "+", substrate, ")*(1-(", species.x, "+", alpha.xy.name, "*", species.y, ")/", Kc.name, ")")
    }
    
    if (single.species.mode) {
      # Only X equation and S consumption from X
      rate.law.y <- NA
      rate.law.s.y <- NA
      # Build MathJax - substrate consumption may or may not have competitive term
      if (no.substrate.restriction) {
        mathjax.law <- paste0("\\begin{aligned}",
                              "\\frac{d", Var2MathJ(species.x), "}{dt} &= ", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(alpha.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                              "\\frac{d", Var2MathJ(substrate), "}{dt} &= -", Var2MathJ(Y_x.name), "*", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}",
                              "\\end{aligned}")
      } else {
        mathjax.law <- paste0("\\begin{aligned}",
                              "\\frac{d", Var2MathJ(species.x), "}{dt} &= ", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(alpha.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                              "\\frac{d", Var2MathJ(substrate), "}{dt} &= -", Var2MathJ(Y_x.name), "*", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(alpha.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right)",
                              "\\end{aligned}")
      }
      eqn.d       <- paste0("Competitive Monod growth: ", species.x, " grows competitively with ", species.y, " as competitor")
    } else {
      # Both species equations
      mu_max.y.name  <- input$TI_comp_monod_mu_max_y
      K_s.y.name     <- input$TI_comp_monod_K_s_y
      alpha.yx.name  <- input$TI_comp_monod_alpha_yx
      Y_y.name       <- input$TI_comp_monod_Y_y
      
      rate.law.y <- paste0(mu_max.y.name, "*", species.y, "*", substrate, "/(", K_s.y.name, "+", substrate, ")*(1-(", species.y, "+", alpha.yx.name, "*", species.x, ")/", Kc.name, ")")
      # If no.substrate.restriction is TRUE, remove competitive term from substrate consumption
      if (no.substrate.restriction) {
        rate.law.s.y <- paste0(Y_y.name, "*", mu_max.y.name, "*", species.y, "*", substrate, "/(", K_s.y.name, "+", substrate, ")")
      } else {
        rate.law.s.y <- paste0(Y_y.name, "*", mu_max.y.name, "*", species.y, "*", substrate, "/(", K_s.y.name, "+", substrate, ")*(1-(", species.y, "+", alpha.yx.name, "*", species.x, ")/", Kc.name, ")")
      }
      # Build MathJax - substrate consumption may or may not have competitive term
      if (no.substrate.restriction) {
        mathjax.law <- paste0("\\begin{aligned}",
                              "\\frac{d", Var2MathJ(species.x), "}{dt} &= ", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(alpha.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                              "\\frac{d", Var2MathJ(species.y), "}{dt} &= ", Var2MathJ(mu_max.y.name), Var2MathJ(species.y), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.y.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.y), "+", Var2MathJ(alpha.yx.name), Var2MathJ(species.x), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                              "\\frac{d", Var2MathJ(substrate), "}{dt} &= -", Var2MathJ(Y_x.name), "*", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}-", Var2MathJ(Y_y.name), "*", Var2MathJ(mu_max.y.name), Var2MathJ(species.y), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.y.name), "+", Var2MathJ(substrate), "}",
                              "\\end{aligned}")
      } else {
        mathjax.law <- paste0("\\begin{aligned}",
                              "\\frac{d", Var2MathJ(species.x), "}{dt} &= ", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(alpha.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                              "\\frac{d", Var2MathJ(species.y), "}{dt} &= ", Var2MathJ(mu_max.y.name), Var2MathJ(species.y), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.y.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.y), "+", Var2MathJ(alpha.yx.name), Var2MathJ(species.x), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                              "\\frac{d", Var2MathJ(substrate), "}{dt} &= -", Var2MathJ(Y_x.name), "*", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(alpha.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right)-", Var2MathJ(Y_y.name), "*", Var2MathJ(mu_max.y.name), Var2MathJ(species.y), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.y.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.y), "+", Var2MathJ(alpha.yx.name), Var2MathJ(species.x), "}{", Var2MathJ(Kc.name), "}\\right)",
                              "\\end{aligned}")
      }
      eqn.d       <- "Competitive Monod growth between two species on shared substrate"
    }
    
    # Set scalars for shared fields (used for parameter table display)
    rate.law    <- rate.law.x
    p.rate.law  <- rate.law.x
    latex.law   <- rate.law.x
    mathml.law  <- NA
    content.ml  <- NA
  }
  else if (input$eqnCreate_reaction_law == "logistic_competition") {
    # Check if single species mode (only X grows competitively)
    single.species.mode <- isTruthy(input$CB_log_comp_single_species)
    # Build two species-specific rate laws and store as two reactions
    reaction.id  <- NA
    eqn.display  <- if (single.species.mode) "Logistic Competition (Single Species)" else "Logistic Competition"
    backend.call <- "logistic_competition"
    modifiers    <- NA
    modifiers.id <- NA
    reactants    <- NA
    reactants.id <- NA
    products     <- NA
    products.id  <- NA
    isReversible <- FALSE
    skip.reaction.entry <- TRUE
    
    # Use different input IDs based on mode
    if (single.species.mode) {
      species.x    <- input$PI_log_comp_species_x_2
      species.y    <- input$PI_log_comp_species_y_2
    } else {
      species.x    <- input$PI_log_comp_species_x
      species.y    <- input$PI_log_comp_species_y
    }
    
    # Ensure we have valid species selections
    if (is.null(species.x) || species.x == "") {
      return() # Can't proceed without species X
    }
    if (is.null(species.y) || species.y == "") {
      return() # Can't proceed without species Y
    }
    species.id.x <- FindId(species.x)
    species.id.y <- FindId(species.y)
    
    if (single.species.mode) {
      # Only X grows competitively, Y is a modifier
      species      <- species.x
      species.id   <- species.id.x
      modifiers    <- species.y
      modifiers.id <- species.id.y
    } else {
      # Both species compete
      species      <- c(species.x, species.y)
      species.id   <- c(species.id.x, species.id.y)
    }
    
    # parameters
    r.x.name  <- input$TI_log_comp_r_x
    r.x.val   <- input$NI_log_comp_r_x_value
    a.xy.name <- input$TI_log_comp_alpha_xy
    a.xy.val  <- input$NI_log_comp_alpha_xy_value
    Kc.name   <- input$TI_log_comp_Kc
    Kc.val    <- input$NI_log_comp_Kc_value
    
    # units: r 1/time, alpha 1 (dimensionless), Kc same units as species (count/conc)
    unit.description.r  <- "num <div> time"
    base.unit.r         <- paste0("1/", rv.UNITS$units.base$Duration)
    unit.r              <- paste0("1/", rv.UNITS$units.selected$Duration)
    
    addParam <- function(name, val, unit, base.unit, unit.desc, desc){
      if (unit != base.unit) {
        base.val <- UnitConversion(unit.desc, unit, base.unit, as.numeric(val))
      } else { base.val <- val }
      list(name=name,val=val,unit=unit,base.unit=base.unit,unit.desc=unit.desc,
           base.val=base.val, desc=desc)
    }
    
    p.r.x <- addParam(r.x.name, r.x.val, unit.r, base.unit.r, unit.description.r,
                      paste0("Growth rate of ", species.x))
    p.a.xy<- addParam(a.xy.name, a.xy.val, "dimensionless", "dimensionless",
                      "dimensionless", paste0("Effect of ", species.y, " on ", species.x))
    # Kc uses species units; treat as concentration/base var
    unit.Kc <- rv.UNITS$units.selected$For.Var
    base.Kc <- rv.UNITS$units.base$For.Var
    p.Kc <- addParam(Kc.name, Kc.val, unit.Kc, base.Kc,
                     paste0("conc (", base.Kc, ")"),
                     "Community carrying capacity")
    
    if (single.species.mode) {
      # Single species mode: only X parameters
      pack <- list(p.r.x, p.a.xy, p.Kc)
    } else {
      # Both species mode: need r.y and alpha.yx
      r.y.name  <- input$TI_log_comp_r_y
      r.y.val   <- input$NI_log_comp_r_y_value
      a.yx.name <- input$TI_log_comp_alpha_yx
      a.yx.val  <- input$NI_log_comp_alpha_yx_value
      
      p.r.y <- addParam(r.y.name, r.y.val, unit.r, base.unit.r, unit.description.r,
                        paste0("Growth rate of ", species.y))
      p.a.yx<- addParam(a.yx.name, a.yx.val, "dimensionless", "dimensionless",
                        "dimensionless", paste0("Effect of ", species.x, " on ", species.y))
      pack <- list(p.r.x, p.r.y, p.a.xy, p.a.yx, p.Kc)
    }
    
    for (p in pack){
      parameters         <- c(parameters, p$name)
      param.vals         <- c(param.vals, p$val)
      param.units        <- c(param.units, p$unit)
      unit.descriptions  <- c(unit.descriptions, p$unit.desc)
      param.descriptions <- c(param.descriptions, p$desc)
      base.units         <- c(base.units, p$base.unit)
      base.values        <- c(base.values, p$base.val)
    }
    
    # Construct rate law - ensure species.y is used correctly
    rate.law.x <- paste0(r.x.name,"*",species.x,"*(1-(",species.x,"+",a.xy.name,"*",species.y,")/",Kc.name,")")
    
    if (single.species.mode) {
      # Only X equation
      rate.law.y <- NA
      mathjax.law <- paste0("\\frac{d", Var2MathJ(species.x), "}{dt} = ", Var2MathJ(r.x.name), Var2MathJ(species.x), "\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(a.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right)")
      eqn.d       <- paste0("Logistic competition: ", species.x, " grows competitively with ", species.y, " as competitor")
    } else {
      # Both species equations
      r.y.name  <- input$TI_log_comp_r_y
      a.yx.name <- input$TI_log_comp_alpha_yx
      rate.law.y <- paste0(r.y.name,"*",species.y,"*(1-(",species.y,"+",a.yx.name,"*",species.x,")/",Kc.name,")")
      mathjax.law <- paste0("\\begin{aligned}",
                            "\\frac{d", Var2MathJ(species.x), "}{dt} &= ", Var2MathJ(r.x.name), Var2MathJ(species.x), "\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(a.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                            "\\frac{d", Var2MathJ(species.y), "}{dt} &= ", Var2MathJ(r.y.name), Var2MathJ(species.y), "\\left(1-\\frac{", Var2MathJ(species.y), "+", Var2MathJ(a.yx.name), Var2MathJ(species.x), "}{", Var2MathJ(Kc.name), "}\\right)",
                            "\\end{aligned}")
      eqn.d       <- "Logistic competition between two species"
    }
    
    # Set scalars for shared fields (used for parameter table display)
    rate.law    <- rate.law.x
    p.rate.law  <- rate.law.x
    latex.law   <- rate.law.x
    mathml.law  <- NA
    content.ml  <- NA
  }
  else if (input$eqnCreate_reaction_law == "predator_prey") {
    reaction.id  <- NA
    eqn.display  <- "Predator–Prey"
    backend.call <- "predator_prey"
    modifiers    <- NA
    modifiers.id <- NA
    reactants    <- NA
    reactants.id <- NA
    products     <- NA
    products.id  <- NA
    isReversible <- FALSE
    skip.reaction.entry <- TRUE
    
    # Species
    species.x    <- input$PI_pred_prey_prey
    species.y    <- input$PI_pred_prey_predator
    if (is.null(species.x) || species.x == "" || is.null(species.y) || species.y == "") {
      return()
    }
    species.id.x <- FindId(species.x)
    species.id.y <- FindId(species.y)
    species      <- c(species.x, species.y)
    species.id   <- c(species.id.x, species.id.y)
    
    # Parameters
    r.name <- input$TI_pred_prey_r
    r.val  <- input$NI_pred_prey_r_value
    a.name <- input$TI_pred_prey_a
    a.val  <- input$NI_pred_prey_a_value
    b.name <- input$TI_pred_prey_b
    b.val  <- input$NI_pred_prey_b_value
    d.name <- input$TI_pred_prey_d
    d.val  <- input$NI_pred_prey_d_value
    
    unit.description.r <- "num <div> time"
    base.unit.r        <- paste0("1/", rv.UNITS$units.base$Duration)
    unit.r             <- paste0("1/", rv.UNITS$units.selected$Duration)
    
    addParam <- function(name, val, unit, base.unit, unit.desc, desc){
      if (unit != base.unit) {
        base.val <- UnitConversion(unit.desc, unit, base.unit, as.numeric(val))
      } else { base.val <- val }
      list(name=name,val=val,unit=unit,base.unit=base.unit,unit.desc=unit.desc,
           base.val=base.val, desc=desc)
    }
    
    p.r <- addParam(r.name, r.val, unit.r, base.unit.r, unit.description.r,
                    paste0("Prey growth rate for ", species.x))
    p.a <- addParam(a.name, a.val, unit.r, base.unit.r, unit.description.r,
                    paste0("Attack rate (loss of ", species.x, " due to ", species.y, ")"))
    p.b <- addParam(b.name, b.val, unit.r, base.unit.r, unit.description.r,
                    paste0("Conversion rate (gain of ", species.y, " from consuming ", species.x, ")"))
    p.d <- addParam(d.name, d.val, unit.r, base.unit.r, unit.description.r,
                    paste0("Predator death rate for ", species.y))
    
    pack <- list(p.r, p.a, p.b, p.d)
    for (p in pack){
      parameters         <- c(parameters, p$name)
      param.vals         <- c(param.vals, p$val)
      param.units        <- c(param.units, p$unit)
      unit.descriptions  <- c(unit.descriptions, p$unit.desc)
      param.descriptions <- c(param.descriptions, p$desc)
      base.units         <- c(base.units, p$base.unit)
      base.values        <- c(base.values, p$base.val)
    }
    
    # Rate laws (net right-hand sides)
    rate.law.x <- paste0(r.name, "*", species.x, "-", a.name, "*", species.x, "*", species.y)
    rate.law.y <- paste0(b.name, "*", species.x, "*", species.y, "-", d.name, "*", species.y)
    
    mathjax.law <- paste0("\\begin{aligned}",
                          "\\frac{d", Var2MathJ(species.x), "}{dt} &= ", Var2MathJ(r.name), Var2MathJ(species.x),
                          "-", Var2MathJ(a.name), Var2MathJ(species.x), Var2MathJ(species.y), " \\\\",
                          "\\frac{d", Var2MathJ(species.y), "}{dt} &= ", Var2MathJ(b.name), Var2MathJ(species.x), Var2MathJ(species.y),
                          "-", Var2MathJ(d.name), Var2MathJ(species.y),
                          "\\end{aligned}")
    rate.law    <- rate.law.x
    p.rate.law  <- rate.law.x
    latex.law   <- rate.law.x
    mathml.law  <- NA
    content.ml  <- NA
    eqn.d       <- paste0("Predator–prey interaction between ", species.x, " (prey) and ", species.y, " (predator)")
  }
  else if (input$eqnCreate_reaction_law == "substrate_synthesis_competition") {
    reaction.id  <- NA
    eqn.display  <- "Substrate Synthesis (Competition)"
    backend.call <- "substrate_synthesis_competition"
    modifiers    <- NA
    modifiers.id <- NA
    isReversible <- FALSE
    skip.reaction.entry <- TRUE
    
    # Get species, substrate, and optional competitor
    species         <- input$PI_sub_syn_comp_species
    species.id      <- FindId(species)
    substrate       <- input$PI_sub_syn_comp_substrate
    substrate.id    <- FindId(substrate)
    competitor      <- input$PI_sub_syn_comp_competitor
    competitor.id   <- if (!is.null(competitor) && competitor != "") FindId(competitor) else NA
    
    # Check if species-dependent checkbox is checked
    species.dependent <- isTruthy(input$CB_sub_syn_comp_species_dependent)
    
    # Build species list
    if (!is.na(competitor.id)) {
      species.list      <- c(species, substrate, competitor)
      species.list.id   <- c(species.id, substrate.id, competitor.id)
      modifiers         <- competitor
      modifiers.id      <- competitor.id
    } else {
      species.list      <- c(species, substrate)
      species.list.id   <- c(species.id, substrate.id)
    }
    
    # Substrate is consumed (reactant), species is produced (product)
    reactants    <- substrate
    reactants.id <- substrate.id
    products     <- species
    products.id  <- species.id
    
    # Parameters
    k.name     <- input$TI_sub_syn_comp_k
    k.val      <- input$NI_sub_syn_comp_k_value
    alpha.name <- input$TI_sub_syn_comp_alpha
    alpha.val  <- input$NI_sub_syn_comp_alpha_value
    Kc.name    <- input$TI_sub_syn_comp_Kc
    Kc.val     <- input$NI_sub_syn_comp_Kc_value
    
    # Units: k has units of (1/time) * (1/concentration) if species-dependent, or (1/time) if not
    # For simplicity, we'll use 1/time for both and let the user adjust
    unit.description.k <- "num <div> time"
    base.unit.k        <- paste0("1/", rv.UNITS$units.base$Duration)
    param.unit.k       <- paste0("1/", rv.UNITS$units.selected$Duration)
    param.description.k <- paste0("Synthesis rate constant for ", species)
    
    if (param.unit.k != base.unit.k) {
      base.val.k <- UnitConversion(unit.description.k,
                                   param.unit.k,
                                   base.unit.k,
                                   as.numeric(k.val))
    } else {
      base.val.k <- k.val
    }
    
    # alpha is dimensionless
    # Kc uses species units
    unit.Kc     <- rv.UNITS$units.selected$For.Var
    base.Kc     <- rv.UNITS$units.base$For.Var
    unit.description.Kc <- paste0("conc (", base.Kc, ")")
    param.description.Kc <- "Community carrying capacity"
    
    if (unit.Kc != base.Kc) {
      base.val.Kc <- UnitConversion(unit.description.Kc,
                                    unit.Kc,
                                    base.Kc,
                                    as.numeric(Kc.val))
    } else {
      base.val.Kc <- Kc.val
    }
    
    parameters         <- c(parameters, k.name, alpha.name, Kc.name)
    param.vals         <- c(param.vals, k.val, alpha.val, Kc.val)
    param.units        <- c(param.units, param.unit.k, "dimensionless", unit.Kc)
    unit.descriptions  <- c(unit.descriptions, unit.description.k, "dimensionless", unit.description.Kc)
    param.descriptions <- c(param.descriptions, param.description.k, 
                           if (!is.na(competitor.id)) paste0("Effect of ", competitor, " on ", species) else "Competition coefficient",
                           param.description.Kc)
    base.units         <- c(base.units, base.unit.k, "dimensionless", base.Kc)
    base.values        <- c(base.values, base.val.k, alpha.val, base.val.Kc)
    
    # Get volume variable
    compartment    <- input$eqnCreate_active_compartment
    compartment.id <- FindId(compartment)
    volume.var     <- rv.COMPARTMENTS$compartments[[compartment.id]]$Volume
    
    # Use rate law function
    laws <- Substrate_Synthesis_Competition(k.name,
                                            substrate,
                                            species,
                                            if (!is.na(competitor.id)) competitor else NA,
                                            alpha.name,
                                            Kc.name,
                                            species.dependent,
                                            volume.var)
    
    # Extract reaction laws
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    content.ml  <- laws$content.ml
    
    # Build equation description
    if (species.dependent) {
      if (!is.na(competitor.id)) {
        eqn.d <- paste0("Substrate synthesis with competition: d", species, "/dt = ", k.name, "*", substrate, "*", species, "*(1-(", species, "+", alpha.name, "*", competitor, ")/", Kc.name, ")")
      } else {
        eqn.d <- paste0("Substrate synthesis with competition: d", species, "/dt = ", k.name, "*", substrate, "*", species, "*(1-", species, "/", Kc.name, ")")
      }
    } else {
      if (!is.na(competitor.id)) {
        eqn.d <- paste0("Substrate synthesis with competition: d", species, "/dt = ", k.name, "*", substrate, "*(1-(", species, "+", alpha.name, "*", competitor, ")/", Kc.name, ")")
      } else {
        eqn.d <- paste0("Substrate synthesis with competition: d", species, "/dt = ", k.name, "*", substrate, "*(1-", species, "/", Kc.name, ")")
      }
    }
  }
  else if (input$eqnCreate_reaction_law == "mass_action_w_reg") {
    reaction.id <- NA
    eqn.display <- "Regulated Mass Action"
    backend.call <- "mass_action_w_reg"
    # browser()
    
    # Get Compartment information
    compartment    <- input$eqnCreate_active_compartment
    compartment.id <- FindId(compartment)
    
    # Find Volume Variable
    volume.var <- rv.COMPARTMENTS$compartments[[compartment.id]]$Volume
    
    modifiers    <- NA
    modifiers.id <- NA
    
    # Base rate constants that can vary based on options
    kf     <- NA
    kf.id  <- NA
    kf.val <- NA
    kr     <- NA
    kr.id  <- NA
    kr.val <- NA
    
    # Modifier rate constants/variables that can vary based on options
    Forward.Mods    <- NA
    Forward.Mods.id <- NA
    Forward.Pars    <- NA
    Forward.Pars.id <- NA
    Reverse.Mods    <- NA
    Reverse.Mods.id <- NA
    Reverse.Pars    <- NA
    Reverse.Pars.id <- NA
    # browser()
    number.reactants <- as.numeric(input$NI_mass_action_wReg_num_reactants)
    number.products  <- as.numeric(input$NI_mass_action_wReg_num_products)
    
    has.f.reg <- input$CB_MAwR_chem_modifier_forward
    has.r.reg <- input$CB_MAwR_chem_modifier_reverse
    n.f.reg   <- as.numeric(input$NI_MAwR_n_forward_regulators) 
    n.r.reg   <- as.numeric(input$NI_MAwR_n_reverse_regulators) 
    
    # Build left hand side of equation
    left     <- BuildEquationSide("input$NI_MAwR_r_stoichiometry_", 
                                  "input$PI_MAwR_reactant_", 
                                  number.reactants)
    r.stoich      <- left[["coefs"]]
    reactants     <- left[["vars"]]
    reactants.id  <- left[["ids"]]
    
    # Build right hand side equation
    right    <- BuildEquationSide("input$NI_MAwR_p_stoichiometry_",
                                  "input$PI_MAwR_product_", 
                                  number.products)
    p.stoich    <- right[["coefs"]]
    products    <- right[["vars"]]
    products.id <- right[["ids"]]
    
    eqn.description <- ""
    species    <- c(strsplit(reactants, ", ")[[1]], 
                    strsplit(products, ", ")[[1]])
    species.id <- c(strsplit(reactants.id, ", ")[[1]],
                    strsplit(products.id, ", ")[[1]])
    
    # Check for forwared regulators
    if (has.f.reg) {
      # Parse forward modifiers information
      f.regs <- BuildRegulatorSide("input$PI_MAwR_forward_regulator_", 
                                   "input$TI_MAwR_forward_regulator_RC_",
                                   "input$TI_MAwR_forward_regulator_RC_value_",
                                   n.f.reg,
                                   reactants,
                                   products,
                                   TRUE)
      FMs     <- f.regs[["regulators"]]
      FM.RC   <- f.regs[["rateConstants"]]
      FM.ids  <- f.regs[["reg.ids"]]
      FM.vals <- f.regs[["regulator.val"]]
     
      FM.rc.descript <- f.regs[["rc.descript"]]
      
      Forward.Mods    <- paste0(FMs, collapse = ", ")
      Forward.Mods.id <- paste0(FM.ids, collapse = ", ")
      Forward.Pars    <- paste0(FM.RC, collapse = ", ")
      
      for (i in seq_along(FM.RC)) {
        u <- DetermineRateConstantUnits("1",
                                        rv.UNITS$units.base$For.Var,
                                        rv.UNITS$units.base$Volume,
                                        rv.UNITS$units.base$Duration,
                                        rv.UNITS$units.selected$For.Var,
                                        rv.UNITS$units.selected$Volume,
                                        rv.UNITS$units.selected$Duration,
                                        addOrder = 1)
        # Perform conversion to base units if needed
        if (u$unit != u$unit.base) {
          base.val <- UnitConversion(u$unit.d,
                                     u$unit,
                                     u$unit.base,
                                     as.numeric(FM.vals[i]))
        } else {
          base.val <- FM.vals[i]
        }
        
        
        parameters         <- c(parameters, FM.RC[i])
        param.vals         <- c(param.vals, FM.vals[i])
        param.units        <- c(param.units, u$unit)
        unit.descriptions  <- c(unit.descriptions, u$unit.d)
        param.descriptions <- c(param.descriptions, FM.rc.descript[i])
        base.units         <- c(base.units, u$unit.base)
        base.values        <- c(base.values, base.val)
      }
      
    } 
    else {
      # Find kf if there are no modifiers for it
      
      kf    <- input$TI_MAwR_forward_k
      kf.id <- FindId(kf)
      # Rate Constant Values
      kf.val <- input$TI_MAwR_forward_k_value
      
      # Build Rate Constant Units
      kf.unit <- DetermineRateConstantUnits(
        p.stoich,
        rv.UNITS$units.base$For.Var,
        rv.UNITS$units.base$Volume,
        rv.UNITS$units.base$Duration,
        rv.UNITS$units.selected$For.Var,
        rv.UNITS$units.selected$Volume,
        rv.UNITS$units.selected$Duration
      )
      
      # Convert rate constant units if necessary
      if (kf.unit$unit != kf.unit$unit.base) {
        kf.base.val <- UnitConversion(kf.unit$unit.description,
                                      kf.unit$unit,
                                      kf.unit$unit.base,
                                      as.numeric(kf.val))
      } else {
        kf.base.val <- kf.val
      }
      
      # Write Unit Descriptions
      kf.d <- paste0("Forward rate constant for the reaction of ",
                     reactants,
                     " to ",
                     products)
      
      parameters         <- c(parameters, kf)
      param.vals         <- c(param.vals, kf.val)
      param.units        <- c(param.units, kf.unit$unit)
      unit.descriptions  <- c(unit.descriptions, kf.unit$unit.description)
      param.descriptions <- c(param.descriptions, kf.d)
      base.units         <- c(base.units, kf.unit$unit.base)
      base.values        <- c(base.values, kf.base.val)
      
    }
    
    reversible <- input$reaction_mass_action_wReg_reverisble
    if (reversible == "both_directions") {
      # If the reaction is reversible then we need to build the reverse
      # rate constant for the reaction
      isReversible <- TRUE
      if (has.r.reg) {
        r.regs <- BuildRegulatorSide("input$PI_MAwR_reverse_regulator_", 
                                     "input$TI_MAwR_reverse_regulator_RC_", 
                                     "input$TI_MAwR_reverse_regulator_RC_value_",
                                     n.r.reg,
                                     reactants,
                                     products,
                                     FALSE)
        RMs     <- r.regs[["regulators"]]
        RM.RC   <- r.regs[["rateConstants"]]
        RM.ids  <- r.regs[["reg.ids"]]
        RM.vals <- r.regs[["regulator.val"]]
        
        RM.rc.descript <- r.regs[["rc.descript"]]
        
        Reverse.Mods    <- paste0(RMs, collapse = ", ")
        Reverse.Mods.id <- paste0(RM.ids, collapse = ", ")
        Reverse.Pars    <- paste0(RM.RC, collapse = ", ")
        
        for (i in seq_along(RM.RC)) {
          u <- DetermineRateConstantUnits("1",
                                          rv.UNITS$units.base$For.Var,
                                          rv.UNITS$units.base$Volume,
                                          rv.UNITS$units.base$Duration,
                                          rv.UNITS$units.selected$For.Var,
                                          rv.UNITS$units.selected$Volume,
                                          rv.UNITS$units.selected$Duration,
                                          addOrder = 1)
          
          # Perform conversion to base units if needed
          if (u$unit != u$unit.base) {
            base.val <- UnitConversion(u$unit.d,
                                       u$unit,
                                       u$unit.base,
                                       as.numeric(RM.vals[i]))
          } else {
            base.val <- RM.vals[i]
          }
          
          parameters         <- c(parameters, RM.RC[i])
          param.vals         <- c(param.vals, RM.vals[i])
          param.units        <- c(param.units, u$unit)
          unit.descriptions  <- c(unit.descriptions, u$unit.d)
          param.descriptions <- c(param.descriptions, RM.rc.descript[i])
          base.units         <- c(base.units, u$unit.base)
          base.values        <- c(base.values, base.val)
        }
      } 
      else {
        kr     <- input$TI_MAwR_reverse_k
        kr.val <- input$TI_MAwR_reverse_k_value
        kr.id  <- FindId(kr)
        # Build Rate Constant Units
        kr.unit <- DetermineRateConstantUnits(
          r.stoich,
          rv.UNITS$units.base$For.Var,
          rv.UNITS$units.base$Volume,
          rv.UNITS$units.base$Duration,
          rv.UNITS$units.selected$For.Var,
          rv.UNITS$units.selected$Volume,
          rv.UNITS$units.selected$Duration
        )
        
        # Convert rate constant units if necessary
        if (kr.unit$unit != kr.unit$unit.base) {
          kr.base.val <- UnitConversion(kr.unit$unit.description,
                                        kr.unit$unit,
                                        kr.unit$unit.base,
                                        as.numeric(kr.val))
        } else {
          kr.base.val <- kr.val
        }
        
        # Write Unit Descriptions
        kr.d <- paste0("Reverse rate constant for the reaction of ",
                       reactants,
                       " to ",
                       products
        )
        
        parameters         <- c(parameters, kr)
        param.vals         <- c(param.vals, kr.val)
        param.units        <- c(param.units,kr.unit$unit)
        unit.descriptions  <- c(unit.descriptions, kr.unit$unit.description)
        param.descriptions <- c(param.descriptions, kr.d)
        base.units         <- c(base.units, kr.unit$unit.base)
        base.values        <- c(base.values, kr.base.val)
      }
    }
    
    # Build Modifier Structures
    if (has.f.reg & has.r.reg) {
      modifiers    <- c(FMs, RMs)
      modifiers.id <- c(FM.ids, RM.ids)
    } else if (has.f.reg & !has.r.reg) {
      modifiers    <- FMs
      modifiers.id <- FM.ids
    } else if (!has.f.reg & has.r.reg) {
      modifiers    <- RMs
      modifiers.id <- RM.ids
    } else {
      #pass
    }
      
    eqn.d <- "Mass Action with Regulation"
    laws <- Regulated_Law_Of_Mass_Action(r.stoich, 
                                         reactants,
                                         p.stoich,
                                         products,
                                         reversible,
                                         kf,
                                         kr,
                                         volume.var,
                                         has.f.reg,
                                         Forward.Mods,
                                         Forward.Pars,
                                         has.r.reg,
                                         Reverse.Mods,
                                         Reverse.Pars)
    
    # Extract reaction laws 
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    content.ml  <- laws$content.ml
  }
  else if (input$eqnCreate_reaction_law == "synthesis") {
    # Separate if factor or not
    if (input$CB_synthesis_factor_checkbox) {
      # Synthesis uses a factor
      eqn.d        <- "Synthesis Reaction by Factor"
      eqn.display  <- "Synthesis (Factor)"
      backend.call <- "synthesis_factor"
      
      var.syn    <- input$PI_synthesis_byFactor_var
      var.syn.id <- FindId(var.syn)
      factor     <- input$PI_synthesis_byFactor_factor
      factor.id  <- FindId(factor)
      
      # factor is not involved in differential equations
      modifiers    <- factor
      modifiers.id <- factor.id
      
      products    <- var.syn
      products.id <- var.syn.id
      
      species     <- c(species, var.syn)
      species.id  <- c(species.id, var.syn.id)
      
      # unit by factor is 1/time
      parameter          <- input$TI_synthesis_byFactor_RC
      param.val          <- input$TI_synthesis_byFactor_RC_value
      base.unit          <- paste0("1/", rv.UNITS$units.base$Duration)
      param.unit         <- paste0("1/", rv.UNITS$units.selected$Duration)
      unit.description   <- "num <div> time"
      param.description  <- paste0("Synthesis rate constant of ", 
                                   species,
                                   " by factor ",
                                   factor)
      
      # Base unit conversion if necessary
      if (param.unit != base.unit) {
        base.val <- UnitConversion(unit.description,
                                   param.unit,
                                   base.unit,
                                   as.numeric(param.val))
      } else {
        base.val <- param.val
      }
      
      parameters          <- c(parameters, parameter)
      param.vals          <- c(param.vals, param.val)
      param.units         <- c(param.units, param.unit)
      unit.descriptions   <- c(unit.descriptions, unit.description)
      param.descriptions  <- c(param.descriptions, param.description)
      base.units          <- c(base.units, base.unit)
      base.values         <- c(base.values, base.val)
      
      laws <- Synthesis_By_Factor(parameter, factor, volume.var)

    } else {
      # Synthesis by rate
      eqn.d       <- "Synthesis Reaction by Rate"
      eqn.display <- "Synthesis (Rate)"
      backend.call <- "synthesis_base_rate"
      
      modifiers    <- NA
      modifiers.id <- NA
      
      var.syn    <- input$PI_synthesis_rate_var
      var.syn.id <- FindId(var.syn)
      factor     <- NA
      factor.id  <- NA
      
      products    <- var.syn
      products.id <- var.syn.id
      
      species     <- c(species, var.syn)
      species.id  <- c(species.id, var.syn.id)
      
      # unit for parameter is concentration/(volume*time)
      parameter          <- input$TI_synthesis_rate_RC
      param.val          <- input$TI_synthesis_rate_RC_value
      base.unit          <- paste0(rv.UNITS$units.base$For.Var,
                                   "/", 
                                   "(",
                                   rv.UNITS$units.base$Volume,
                                   "*",
                                   rv.UNITS$units.base$Duration,
                                   ")")
      param.unit          <- paste0(rv.UNITS$units.selected$For.Var,
                                   "/", 
                                   "(",
                                   rv.UNITS$units.selected$Volume,
                                   "*",
                                   rv.UNITS$units.selected$Duration,
                                   ")")
      unit.description   <- paste0("conc (", 
                                   rv.UNITS$units.base$For.Var,
                                   ")",
                                   " <div> ",
                                   "<group> volume <multiply> time <endgroup>"
                                   )
      param.description  <- paste0("Synthesis rate constant of ", 
                                   species,
                                   " by factor ",
                                   factor)
      
      # Base unit conversion if necessary
      if (param.unit != base.unit) {
        base.val <- UnitConversion(unit.description,
                                   param.unit,
                                   base.unit,
                                   as.numeric(param.val))
      } else {
        base.val <- param.val
      }
      
      parameters          <- c(parameters, parameter)
      param.vals          <- c(param.vals, param.val)
      param.units         <- c(param.units, param.unit)
      unit.descriptions   <- c(unit.descriptions, unit.description)
      param.descriptions  <- c(param.descriptions, param.description)
      base.units          <- c(base.units, base.unit)
      base.values         <- c(base.values, base.val)
      
      laws <- Synthesis_By_Rate(parameter, volume.var)
    }
    
    # Extract reaction laws 
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    content.ml  <- laws$content.ml
  }
  else if (input$eqnCreate_reaction_law == "degradation_rate") {
    # browser()
    eqn.d       <- "Degrdation by Rate"
    eqn.display <- "Degradation (Rate)"
    backend.call <- "degradation_rate"
    
    modifiers    <- NA
    modifiers.id <- NA
    
    deg.species    <- input$PI_degradation_rate_species
    deg.species.id <- FindId(deg.species)
    ConcDep        <- input$CB_degradation_rate_conc_dependent
    
    reactants    <- deg.species
    reactants.id <- deg.species.id
    if (ConcDep) {
      backend.call <- "degradation_rate_concDep"
    } else {
      backend.call <- "degradation_rate_not_concDep"
    }
    
    # Check to see if products are being produced and store them
    if (input$CB_degradation_rate_toProducts) {
      if (ConcDep) {
        backend.call <- "degradation_rate_concDep_products"
      } else {
        backend.call <- "degradation_rate_not_concDep_products"
      }
      products    <- c()
      products.id <- c()
      num.deg.products <- as.numeric(input$NI_degradation_rate_num_products)
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$PI_degradation_rate_product_", 
                                         as.character(i))))
        prod.id <- FindId(prod)
        
        products <- c(products, prod)
        products.id <- c(products.id, prod.id)
      }
      # Collapse Products into string list if needed
      products.collapsed     <- paste0(products, collapse = ", ")
      products.id.collapsed  <- paste0(products.id, collapse = ", ")
    } else {
      products               <- NA
      products.id            <- NA
      products.collapsed     <- NA
      products.id.collapsed  <- NA
    }
    
    if (!is.na(products.collapsed)) {
      species    <- c(deg.species, products)
      species.id <- c(deg.species.id, products.id)
    } else {
      species    <- deg.species
      species.id <- deg.species.id
    }

    # units: 1/time
    parameter         <- input$TI_degradation_rate_RC
    param.val         <- input$TI_degradation_rate_RC_value
    base.unit         <- paste0("1/", rv.UNITS$units.base$Duration)
    param.unit        <- paste0("1/", rv.UNITS$units.selected$Duration)
    unit.description  <- "num <div> time"
    param.description <- paste0("Degradation rate constant for ", species)
    
    # Base unit conversion if necessary
    if (param.unit != base.unit) {
      base.val <- UnitConversion(unit.description,
                                 param.unit,
                                 base.unit,
                                 as.numeric(param.val))
    } else {
      base.val <- param.val
    }
    
    parameters          <- c(parameters, parameter)
    param.vals          <- c(param.vals, param.val)
    param.units         <- c(param.units, param.unit)
    unit.descriptions   <- c(unit.descriptions, unit.description)
    param.descriptions  <- c(param.descriptions, param.description)
    base.units          <- c(base.units, base.unit)
    base.values         <- c(base.values, base.val)
    
    # Add krel parameter if products are being produced AND relative formation is checked
    krel.param <- NA
    krel.param.id <- NA
    if (input$CB_degradation_rate_toProducts && isTruthy(input$CB_degradation_rate_relative_formation)) {
      krel.param         <- input$TI_degradation_rate_krel
      krel.param.val     <- input$NI_degradation_rate_krel_value
      krel.base.unit     <- "dimensionless"
      krel.param.unit    <- "dimensionless"
      krel.unit.desc     <- "dimensionless"
      krel.param.desc    <- paste0("Product yield fraction for degradation of ", deg.species)
      
      parameters          <- c(parameters, krel.param)
      param.vals          <- c(param.vals, krel.param.val)
      param.units         <- c(param.units, krel.param.unit)
      unit.descriptions   <- c(unit.descriptions, krel.unit.desc)
      param.descriptions  <- c(param.descriptions, krel.param.desc)
      base.units          <- c(base.units, krel.base.unit)
      base.values         <- c(base.values, krel.param.val)
    }
    
    # Store Rate Law
    laws <- Degradation_By_Rate(parameter, ConcDep, deg.species, volume.var)
    
    # Extract reaction laws 
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    content.ml  <- laws$content.ml
  }
  else if (input$eqnCreate_reaction_law == "degradation_by_enzyme") {
    
    eqn.d        <- "Degrdation by enzyme"
    eqn.display  <- "Degradation (By Enzyme)"

    # Initialize vars that are pathway dependent to NA
    modifiers    <- NA
    modifiers.id <- NA
    enzyme       <- NA
    enzyme.id    <- NA
    kcat         <- NA
    kcat.id      <- NA
    Vmax         <- NA
    Vmax.id      <- NA

    deg.species    <- input$PI_degradation_enzyme_species
    deg.species.id <- FindId(deg.species)
    
    reactants    <- deg.species
    reactants.id <- deg.species.id
    
    Use.Vmax   <- input$CB_degradation_enzyme_useVmax
    
    #browser()
    # Check to see if products are being produced and store them
    if (input$CB_degradation_enzyme_toProducts) {
      backend.call <- "degradation_by_enzyme_wProducts"
      products    <- c()
      products.id <- c()
      num.deg.products <- as.numeric(input$NI_degradation_enzyme_num_products)
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$PI_degradation_enzyme_product_", 
                                         as.character(i))))
        prod.id <- FindId(prod)
        
        products    <- c(products, prod)
        products.id <- c(products.id, prod.id)
      }
      # Collapse Products into string list if needed
      products.collapsed     <- paste0(products, collapse = ", ")
      products.id.collapsed  <- paste0(products.id, collapse = ", ")
    } else {
      products               <- NA
      products.id            <- NA
      products.collapsed     <- NA
      products.id.collapsed  <- NA
    }

    if (!is.na(products.collapsed)) {
      species    <- c(deg.species, products)
      species.id <- c(deg.species.id, products.id)
    } else {
      species    <- deg.species
      species.id <- deg.species.id
    }

    # Km Rate Constant, unit: concentration/volume (mol/L)
    Km               <- input$TI_degradation_enzyme_Km
    Km.val           <- input$TI_degradation_enzyme_Km_value
    Km.unit          <- paste0(rv.UNITS$units.selected$For.Var,
                               "/",
                               rv.UNITS$units.selected$Volume
                               )
    Km.base.unit     <- paste0(rv.UNITS$units.base$For.Var,
                               "/",
                               rv.UNITS$units.base$Volume
                               )
    Km.unit.descript <- paste0("conc (", rv.UNITS$units.base$For.Var, ")",
                               " <div> ",
                               "volume")
    Km.descript      <- paste0("Michelias Menten constant for degradation of ",
                               species)
    
    # Base unit conversion if necessary
    if (Km.unit != Km.base.unit) {
      Km.base.val <- UnitConversion(Km.unit.descript,
                                    Km.unit,
                                    Km.base.unit,
                                    as.numeric(Km.val))
    } else {
      Km.base.val <- Km.val
    }
    
    # Store Km Parameter
    parameters          <- c(parameters, Km)
    param.vals          <- c(param.vals, Km.val)
    param.units         <- c(param.units, Km.unit)
    unit.descriptions   <- c(unit.descriptions, Km.unit.descript)
    param.descriptions  <- c(param.descriptions, Km.descript)
    base.units          <- c(base.units, Km.base.unit)
    base.values         <- c(base.values, Km.base.val)
    
    # If Uses Vmax 
    if (Use.Vmax) {
      # In this option the reaction used Vmax instead of kcat*enzyme
      backend.call <- "degradation_by_enzyme_use_vmax"
      
      
      # Vmax Rate Constant, unit: concentration/(volume*time)
      Vmax               <- input$TI_degradation_enzyme_Vmax
      Vmax.val           <- input$TI_degradation_enzyme_Vmax_value
      Vmax.base.unit     <- paste0(rv.UNITS$units.base$For.Var,
                                   "/", 
                                   "(",
                                   rv.UNITS$units.base$Volume,
                                   "*",
                                   rv.UNITS$units.base$Duration,
                                   ")")
      Vmax.unit          <- paste0(rv.UNITS$units.selected$For.Var,
                                    "/", 
                                    "(",
                                    rv.UNITS$units.selected$Volume,
                                    "*",
                                    rv.UNITS$units.selected$Duration,
                                    ")")
      Vmax.unit.descript   <- paste0("conc (",
                                   rv.UNITS$units.base$For.Var,
                                   ")",
                                   " <div> ",
                                   "<group> volume <multiply> time <endgroup>"
                                    )
      
      Vmax.descript    <- paste0("Maximum Velocity for degradation of ", 
                                 species)
      
      if (Vmax.unit != Vmax.base.unit) {
        Vmax.base.val <- UnitConversion(Vmax.unit.descript,
                                        Vmax.unit,
                                        Vmax.base.unit,
                                        as.numeric(Vmax.val))
      } else {
        Vmax.base.val <- Vmax.val
      }
      
      # Store Vmax Parameter
      parameters          <- c(parameters, Vmax)
      param.vals          <- c(param.vals, Vmax.val)
      param.units         <- c(param.units, Vmax.unit)
      unit.descriptions   <- c(unit.descriptions, Vmax.unit.descript)
      param.descriptions  <- c(param.descriptions, Vmax.descript)
      base.units          <- c(base.units, Vmax.base.unit)
      base.values         <- c(base.values, Vmax.base.val)
      
      # Store Rate Law
      laws <- Degradation_By_Enzyme_Vmax(deg.species, Km, Vmax, volume.var)
    } else {
      # In this option kcat*enzyme is used instead of Vmax for reaction
      backend.call <- "degradation_by_enzyme_no_vmax"
      
      enzyme    <- input$PI_degradation_enzyme_enzyme
      enzyme.id <- FindId(enzyme)
      
      modifiers    <- enzyme
      modifiers.id <- enzyme.id
      
      
      # kcat, unit 1/time
      kcat               <- input$TI_degradation_enzyme_kcat
      kcat.val           <- input$TI_degradation_enzyme_kcat_value
      kcat.base.unit     <- paste0("1/", rv.UNITS$units.base$Duration)
      kcat.unit          <- paste0("1/", rv.UNITS$units.selected$Duration)
      kcat.unit.descript <- "num <div> time"
      kcat.descript      <- paste0("Enzymatic degradation rate constant of ", 
                                   species,
                                   " by ",
                                   enzyme)
      
      if (kcat.unit != kcat.base.unit) {
        kcat.base.val <- UnitConversion(kcat.unit.descript,
                                        kcat.unit,
                                        kcat.base.unit,
                                        as.numeric(kcat.val))
      } else {
        kcat.base.val <- kcat.val
      }
      
      # Store kcat Parameter
      parameters          <- c(parameters, kcat)
      param.vals          <- c(param.vals, kcat.val)
      param.units         <- c(param.units, kcat.unit)
      unit.descriptions   <- c(unit.descriptions, kcat.unit.descript)
      param.descriptions  <- c(param.descriptions, kcat.descript)
      base.units          <- c(base.units, kcat.base.unit)
      base.values         <- c(base.values, kcat.base.val)
      
      # Store Rate Law
      laws <- Degradation_By_Enzyme_no_Vmax(deg.species, 
                                            Km, 
                                            kcat, 
                                            enzyme, 
                                            volume.var)
    }
    
    # Add krel parameter if products are being produced AND relative formation is checked
    krel.param <- NA
    krel.param.id <- NA
    if (input$CB_degradation_enzyme_toProducts && isTruthy(input$CB_degradation_enzyme_relative_formation)) {
      krel.param         <- input$TI_degradation_enzyme_krel
      krel.param.val     <- input$NI_degradation_enzyme_krel_value
      krel.base.unit     <- "dimensionless"
      krel.param.unit    <- "dimensionless"
      krel.unit.desc     <- "dimensionless"
      krel.param.desc    <- paste0("Product yield fraction for degradation of ", deg.species)
      
      parameters          <- c(parameters, krel.param)
      param.vals          <- c(param.vals, krel.param.val)
      param.units         <- c(param.units, krel.param.unit)
      unit.descriptions   <- c(unit.descriptions, krel.unit.desc)
      param.descriptions  <- c(param.descriptions, krel.param.desc)
      base.units          <- c(base.units, krel.base.unit)
      base.values         <- c(base.values, krel.param.val)
      
      # Note: krel.param.id will be determined later when par.ids is created
      # For now, just leave it as NA - it will be set in the sub.entry creation block
    }
    
    # Extract reaction laws 
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    content.ml  <- laws$content.ml
  }
  else if (input$eqnCreate_reaction_law == "michaelis_menten") {
    # Initialize vars that are pathway dependent to NA
    modifiers    <- NA
    modifiers.id <- NA
    enzyme       <- NA
    enzyme.id    <- NA
    kcat         <- NA
    kcat.id      <- NA
    Vmax         <- NA
    Vmax.id      <- NA
    
    eqn.d        <- "Michaelis Menten Enzyme Kinetics"
    eqn.display  <- "Michaelis Menten"

    substrate    <- input$PI_michaelis_menten_substrate
    substrate.id <- FindId(substrate)
    
    reactants    <- substrate
    reactants.id <- substrate.id
    products     <- input$PI_michaelis_menten_product
    products.id  <- FindId(products)
    
    species    <- c(reactants, products)
    species.id <- c(reactants.id, products.id)
    
    Use.Vmax   <- input$CB_michaelis_menten_useVmax
    
    # Km Rate Constant
    Km               <- input$TI_michaelis_menten_Km
    Km.val           <- input$TI_michaelis_menten_Km_value
    Km.unit          <- paste0(rv.UNITS$units.selected$For.Var,
                               "/",
                               rv.UNITS$units.selected$Volume
    )
    Km.base.unit     <- paste0(rv.UNITS$units.base$For.Var,
                               "/",
                               rv.UNITS$units.base$Volume
    )
    Km.unit.descript <- paste0("conc (", rv.UNITS$units.base$For.Var, ")",
                               " <div> ",
                               "volume")
    Km.descript      <- paste0("Michelias Menten constant for enzymatic", 
                               " conversion of ",
                               species,
                               " to ",
                               products)
    
    # Base unit conversion if necessary
    if (Km.unit != Km.base.unit) {
      Km.base.val <- UnitConversion(Km.unit.descript,
                                    Km.unit,
                                    Km.base.unit,
                                    as.numeric(Km.val))
    } else {
      Km.base.val <- Km.val
    }
    
    # Store Km Parameter
    parameters          <- c(parameters, Km)
    param.vals          <- c(param.vals, Km.val)
    param.units         <- c(param.units, Km.unit)
    unit.descriptions   <- c(unit.descriptions, Km.unit.descript)
    param.descriptions  <- c(param.descriptions, Km.descript)
    base.units          <- c(base.units, Km.base.unit)
    base.values         <- c(base.values, Km.base.val)
    
    # If Uses Vmax 
    if (Use.Vmax) {
      # In this option the reaction used Vmax instead of kcat*enzyme
      backend.call <- "michaelis_menten_use_vmax"
      
      # Vmax Rate Constant
      Vmax               <- input$TI_michaelis_menten_vmax
      Vmax.val           <- input$TI_michaelis_menten_vmax_value
      Vmax.base.unit     <- paste0(rv.UNITS$units.base$For.Var,
                                   "/", 
                                   "(",
                                   rv.UNITS$units.base$Volume,
                                   "*",
                                   rv.UNITS$units.base$Duration,
                                   ")")
      Vmax.unit          <- paste0(rv.UNITS$units.selected$For.Var,
                                   "/", 
                                   "(",
                                   rv.UNITS$units.selected$Volume,
                                   "*",
                                   rv.UNITS$units.selected$Duration,
                                   ")")
      Vmax.unit.descript   <- paste0("conc (",
                                     rv.UNITS$units.base$For.Var,
                                     ")",
                                     " <div> ",
                                     "<group> volume <multiply> time <endgroup>"
      )
      Vmax.descript <- paste0("Maximum Velocity for enzymatic conversion of ",
                              species,
                              " to ",
                              products)
      
      if (Vmax.unit != Vmax.base.unit) {
        Vmax.base.val <- UnitConversion(Vmax.unit.descript,
                                        Vmax.unit,
                                        Vmax.base.unit,
                                        as.numeric(Vmax.val))
      } else {
        Vmax.base.val <- Vmax.val
      }
      
      # Store Vmax Parameter
      parameters          <- c(parameters, Vmax)
      param.vals          <- c(param.vals, Vmax.val)
      param.units         <- c(param.units, Vmax.unit)
      unit.descriptions   <- c(unit.descriptions, Vmax.unit.descript)
      param.descriptions  <- c(param.descriptions, Vmax.descript)
      base.units          <- c(base.units, Vmax.base.unit)
      base.values         <- c(base.values, Vmax.base.val)
      
      # Find Rate Law
      laws <- Henri_Michaelis_Menten_Vmax(substrate, Km, Vmax, volume.var)
      
    } else {
      # In this option kcat*enzyme is used instead of Vmax for reaction
      backend.call <- "michaelis_menten_convert_vmax"
      
      enzyme    <- input$PI_michaelis_menten_enzyme
      enzyme.id <- FindId(enzyme)
      
      modifiers    <- enzyme
      modifiers.id <- enzyme.id
      
      
      # kcat
      kcat               <- input$TI_michaelis_menten_kcat
      kcat.val           <- input$TI_michaelis_menten_kcat_value
      kcat.base.unit     <- paste0("1/", rv.UNITS$units.base$Duration)
      kcat.unit          <- paste0("1/", rv.UNITS$units.selected$Duration)
      kcat.unit.descript <- "num <div> time"
      kcat.descript      <- paste0("Enzymatic rate constant for the",
                                   " conversion of ",
                                   species,
                                   " to ",
                                   products, 
                                   " by ",
                                   enzyme)
      
      if (kcat.unit != kcat.base.unit) {
        kcat.base.val <- UnitConversion(kcat.unit.descript,
                                        kcat.unit,
                                        kcat.base.unit,
                                        as.numeric(kcat.val))
      } else {
        kcat.base.val <- kcat.val
      }
      
      # Store kcat Parameter
      parameters          <- c(parameters, kcat)
      param.vals          <- c(param.vals, kcat.val)
      param.units         <- c(param.units, kcat.unit)
      unit.descriptions   <- c(unit.descriptions, kcat.unit.descript)
      param.descriptions  <- c(param.descriptions, kcat.descript)
      base.units          <- c(base.units, kcat.base.unit)
      base.values         <- c(base.values, kcat.base.val)
      
      # Store rate law
      laws <- Henri_Michaelis_Menten_no_Vmax(substrate, 
                                             Km, 
                                             kcat, 
                                             enzyme, 
                                             volume.var)
    }
    
    # Extract reaction laws 
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    content.ml  <- laws$content.ml
  }
  else if (startsWith(input$eqnCreate_reaction_law, "user_custom_law_")) {
    # Parse and store information for custom entered law
    # browser()
    # Find the custom law that is being used
    backend.name <- input$eqnCreate_reaction_law
    custom.id    <- strsplit(backend.name, "_")[[1]][4]
    
    # Find the reaction entry of this id
    law.entry <- rv.CUSTOM.LAWS$cl.reaction[[custom.id]]
    backend.call <- input$eqnCreate_reaction_law
    
    # Pull entry base variables
    base.reactants  <- SplitEntry(law.entry$Reactants)
    base.products   <- SplitEntry(law.entry$Products)
    base.modifiers  <- SplitEntry(law.entry$Modifiers)
    base.parameters <- SplitEntry(law.entry$Parameters)
    base.rate.law   <- law.entry$String.Rate.Law
    
    eqn.display   <- law.entry$Law.Name
    eqn.d         <- law.entry$Description
    isReversible  <- law.entry$Reversible
    
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
    reactants  <- NA
    products   <- NA
    modifiers  <- NA
    parameters <- NA
    
    reactants.id <- NA
    products.id  <- NA
    modifiers.id <- NA
    
    if (has.reactants) {
      reactants    <- c()
      reactants.id <- c()
      for (i in seq(n.reactants)) {
        reactants <- c(reactants, 
                       eval(parse(text = paste0("input$PI_CL_reactant_", 
                                                as.character(i)))))
        reactants.id <- c(reactants.id, FindId(reactants[i]))
      }
    } 
    
    if (has.products) {
      products    <- c()
      products.id <- c()
      for (i in seq(n.products)) {
        products <- c(products, 
                      eval(parse(text = paste0("input$PI_CL_product_", 
                                               as.character(i)))))
        products.id <- c(products.id, FindId(products[i]))
      }
    } 
    
    if (has.modifiers) {
      modifiers    <- c()
      modifiers.id <- c()
      for (i in seq(n.modifiers)) {
        modifiers <- c(modifiers, 
                       eval(parse(text = paste0("input$PI_CL_modifier_", 
                                                as.character(i)))))
        modifiers.id <- c(modifiers.id, FindId(modifiers[i]))
      }
    }
    
    if (has.parameters) {
      parameters <- c()
      for (i in seq(n.parameters)) {
        parameters <- c(parameters, 
                        eval(parse(text = paste0("input$PI_CL_parameter_", 
                                                 as.character(i)))))
      }
      parameter.values <- c()
      for (i in seq(n.parameters)) {
        parameter.values <- c(parameter.values, 
                        eval(parse(text = paste0("input$PI_CL_parameter_value_", 
                                                 as.character(i)))))
      }
      
      # Set Parameter Information to NA for units
      param.vals          <- parameter.values
      param.units         <- rep(NA, n.parameters)
      unit.descriptions   <- rep(NA, n.parameters)
      param.descriptions  <- rep("Custom Reaction Parameter", n.parameters)
      base.units          <- rep(NA, n.parameters)
      base.values         <- parameter.values
    }
    
    species    <- RemoveNA(c(reactants, products))
    species.id <- RemoveNA(c(reactants.id, products.id))
    
    # Build Reaction Schemes
    eqn.builds <- BuildCustomEquationText(reactants,
                                          products,
                                          modifiers,
                                          parameters)
    
    text.eqn    <- eqn.builds$text
    latex.eqn   <- eqn.builds$latex
    mathjax.eqn <- eqn.builds$mathjax
    
    # Build Rate Laws 
    rate.law <- SubstituteRateLawTerms(base.rate.law,
                                       base.reactants,
                                       base.products,
                                       base.modifiers,
                                       base.parameters,
                                       reactants,
                                       products,
                                       modifiers,
                                       parameters)
    
    p.rate.law <- NA
    convert.rate.law <- ConvertRateLaw(rate.law)
    latex.law   <- convert.rate.law$latex
    mathjax.law <- convert.rate.law$mathjax
    mathml.law  <- katex::katex_mathml(latex.law)
    content.ml <- 
      paste0("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
             string2mathml(rate.law),
             "</math>")
    
  }
  # browser()
  #Error Check
  # We need parameter name, unit description
  passed.error.check <- TRUE
  for (i in seq_along(parameters)) {
    par.error.DS <- list("Name" = parameters[i],
                         "UnitDescription" = unit.descriptions[i])
    error.check <- CheckParametersForErrors(par.error.DS,
                                            rv.SPECIES$species,
                                            rv.PARAMETERS$parameters,
                                            rv.COMPARTMENTS$compartments)
    passed.check <- error.check[[1]]
    # Break loop and return error message if parameter fails check
    if (!passed.check) {passed.error.check <- FALSE}
  }
  
  
  if (passed.error.check) {
    
    # Build Eqn.id
    # Generate eqn ID
    ID.gen <- GenerateId(rv.ID$id.eqn.seed, "eqn")
    rv.ID$id.eqn.seed <- rv.ID$id.eqn.seed + 1
    ID.to.add <- ID.gen[["id"]]
    idx.to.add <- nrow(rv.ID$id.df) + 1
    
    rv.ID$id.df[idx.to.add, ] <- c(ID.to.add, text.eqn)
    
    # Parameters
    par.ids <- c()
    for (i in seq_along(parameters)) {
      # Check to see if parameter name is new or needs to be appeneded
      if (parameters[i] %in% rv.PARAMETERS$parameters.names) {
        #APPEND
        # Find parameter id
        par.id <- FindId(parameters[i])
        par.ids <- c(par.ids, par.id)
        
        type <- 
          strsplit(rv.PARAMETERS$parameters[[par.id]]$Type, ", ")[[1]]
        type.note <- SplitEntry(rv.PARAMETERS$parameters[[par.id]]$Type.Note)
        used.in   <- SplitEntry(rv.PARAMETERS$parameters[[par.id]]$Used.In)
        is.custom <- rv.PARAMETERS$parameters[[par.id]]$Custom
        old.par.des <- 
          SplitEntry(rv.PARAMETERS$parameters[[par.id]]$Description)
        
        new.type      <- collapseVector(c(type, "Reaction"))
        new.type.note <- collapseVector(c(type.note, 
                                          input$eqnCreate_reaction_law))
        new.used.in   <- collapseVector(c(used.in, ID.to.add))
        new.par.des   <- collapseVector(c(old.par.des, param.descriptions[i]))
        
        # Write out to parameter
        to.par.list <- list("Name"            = parameters[i],
                            "ID"              = par.id,
                            "Value"           = as.numeric(param.vals[i]),
                            "Unit"            = param.units[i],
                            "UnitDescription" = unit.descriptions[i],
                            "BaseUnit"        = base.units[i],
                            "BaseValue"       = as.numeric(base.values[i]),
                            "Description"     = new.par.des,
                            "Type"            = new.type,
                            "Type.Note"       = new.type.note,
                            "Used.In"         = new.used.in,
                            "Custom"          = is.custom
                            )
        
        # Append parameter entry
        rv.PARAMETERS$parameters[[par.id]] <- to.par.list
        
      } else {
        # Create new ID and store parameter
        # Generate Parameter ID
        par.gen <- GenerateId(rv.ID$id.param.seed, "parameter")
        rv.ID$id.param.seed <- par.gen$seed
        par.id <- par.gen$id
        par.ids <- c(par.ids, par.id)
        
        # Store ID to database
        idx.to.add <- nrow(rv.ID$id.df) + 1
        rv.ID$id.df[idx.to.add, ] <- c(par.id, parameters[i])
        
        # Write out to parameter
        to.par.list <- list("Name"            = parameters[i],
                            "ID"              = par.id,
                            "Value"           = as.numeric(param.vals[i]),
                            "Unit"            = param.units[i],
                            "UnitDescription" = unit.descriptions[i],
                            "BaseUnit"        = base.units[i],
                            "BaseValue"       = as.numeric(base.values[i]),
                            "Description"     = param.descriptions[i],
                            "Type"            = "Reaction",
                            "Type.Note"       = input$eqnCreate_reaction_law,
                            "Used.In"         = ID.to.add,
                            "Custom"          = FALSE)
        
        # Store to parameter list
        rv.PARAMETERS$parameters[[par.id]] <- to.par.list
      }
    }
    # browser()
    
    # Link species to reaction IDs (skip for logistic_competition, competitive_monod, predator_prey - handled separately)
    if (isTruthy(species.id) && 
        input$eqnCreate_reaction_law != "logistic_competition" && 
        input$eqnCreate_reaction_law != "competitive_monod" &&
        input$eqnCreate_reaction_law != "predator_prey") {
      # Loop through species id to begin addition
      for (i in seq_along(species.id)) {
        # Check that the species id has IO.ids already or if its NA
        if (is.na(rv.SPECIES$species[[species.id[i]]]$Reaction.ids)) {
          # If its NA, make current id  the id
          rv.SPECIES$species[[species.id[i]]]$Reaction.ids <- ID.to.add
        } else {
          # Else paste0 collapse current id with ", "
          items <- 
            strsplit(
              rv.SPECIES$species[[species.id[i]]]$Reaction.ids, ", ")[[1]]
          items <- c(items, ID.to.add)
          rv.SPECIES$species[[species.id[i]]]$Reaction.ids <- 
            paste0(items, collapse = ", ")
        }
      }
    }
    
    # Build equation description (uses user entered description)
    if (isTruthy(trimws(input$TAI_reaction_description_add))) {
      eqn.d <- input$TAI_reaction_description_add
    }
    
    # We need to collapse these vector terms otherwise when the list is 
    # converted to a dataframe there will be errors

    par.collapsed          <- collapseVector(parameters)
    par.id.collapsed       <- collapseVector(par.ids)
    reactants.collapsed    <- collapseVector(reactants)
    reactants.id.collapsed <- collapseVector(reactants.id)
    products.collapsed     <- collapseVector(products)
    products.id.collapsed  <- collapseVector(products.id)
    species.collapsed      <- collapseVector(species)
    species.id.collapsed   <- collapseVector(species.id)
    modifiers.collapsed    <- collapseVector(modifiers)
    modifiers.id.collapsed <- collapseVector(modifiers.id)
    
    # Add overall reaction information (skip for logistic_competition and competitive_monod which write custom entries)
    if (input$eqnCreate_reaction_law != "logistic_competition" && input$eqnCreate_reaction_law != "competitive_monod" && input$eqnCreate_reaction_law != "substrate_synthesis_competition" && input$eqnCreate_reaction_law != "predator_prey") {
      reaction.entry <- list(
        "ID"               = ID.to.add,
        "Eqn.Display.Type" = eqn.display,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "Backend.Call"     = backend.call,
        "Species"          = species.collapsed,
        "Reactants"        = reactants.collapsed,
        "Products"         = products.collapsed, 
        "Modifiers"        = modifiers.collapsed,
        "Parameters"       = par.collapsed,
        "Compartment"      = compartment,
        "Description"      = eqn.d,
        "Species.id"       = species.id.collapsed,
        "Reactants.id"     = reactants.id.collapsed,
        "Products.id"      = products.id.collapsed,
        "Modifiers.id"     = modifiers.id.collapsed, 
        "Parameters.id"    = par.id.collapsed,
        "Compartment.id"   = compartment.id,
        "Equation.Text"    = text.eqn,
        "Equation.Latex"   = latex.eqn,
        "Equation.MathJax" = mathjax.eqn,
        "String.Rate.Law"  = rate.law,
        "Pretty.Rate.Law"  = p.rate.law,
        "Latex.Rate.Law"   = latex.law,
        "MathJax.Rate.Law" = mathjax.law,
        "MathMl.Rate.Law"  = mathml.law,
        "Content.MathMl"   = content.ml,
        "Reversible"       = isReversible
      )
      
      n.eqns <- length(rv.REACTIONS$reactions)
      rv.REACTIONS$reactions[[n.eqns + 1]] <- reaction.entry
      names(rv.REACTIONS$reactions)[n.eqns+1] <- ID.to.add
    }
    
    # Build specific reaction type reactive variable
    if (input$eqnCreate_reaction_law == "mass_action") {
      if (length(par.ids) == 1) {
        kf.id = par.ids[1]
        kr.id = NA
      } else {
        kf.id = par.ids[1]
        kr.id = par.ids[2]
      }
      
      sub.entry <- list(
        "ID" = ID.to.add,
        "Reaction.Law"    = input$eqnCreate_reaction_law,
        "r.stoichiometry" = r.stoich,
        "Reactants"       = reactants,
        "Reactants.id"    = reactants.id,
        "p.stoichiometry" = p.stoich,
        "Products"        = products,
        "Products.id"     = products.id,
        "Reversible"      = reversible,
        "kf"              = kf,
        "kr"              = kr,
        "kf.id"           = kf.id,
        "kr.id"           = kr.id
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$massAction)
      rv.REACTIONS$massAction[[n+1]] <- sub.entry
      names(rv.REACTIONS$massAction)[n+1] <- ID.to.add

    } 
    else if (input$eqnCreate_reaction_law == "exponential_growth") {
      mu.id <- par.ids[1]
      sub.entry <- list(
        "ID"            = ID.to.add,
        "Reaction.Law"  = input$eqnCreate_reaction_law,
        "Species"       = species,
        "Species.id"    = species.id,
        "Mu"            = parameters[1],
        "Mu.id"         = mu.id,
        "Mu.val"        = param.vals[1],
        "Mu.unit"       = param.units[1],
        "Mu.unit.desc"  = unit.descriptions[1],
        "Mu.base.unit"  = base.units[1],
        "Mu.base.val"   = base.values[1]
      )
      
      n <- length(rv.REACTIONS$exponentialGrowth)
      rv.REACTIONS$exponentialGrowth[[n + 1]] <- sub.entry
      names(rv.REACTIONS$exponentialGrowth)[n + 1] <- ID.to.add
    }
  else if (input$eqnCreate_reaction_law == "monod_growth") {
      mu_max.id <- par.ids[1]
      K_s.id    <- par.ids[2]
      sub.entry <- list(
        "ID"            = ID.to.add,
        "Reaction.Law"  = input$eqnCreate_reaction_law,
        "Species"       = growth.species,
        "Species.id"    = growth.species.id,
        "Substrate"     = substrate,
        "Substrate.id"  = substrate.id,
        "Mu_max"        = parameters[1],
        "Mu_max.id"     = mu_max.id,
        "Mu_max.val"    = param.vals[1],
        "Mu_max.unit"   = param.units[1],
        "Mu_max.unit.desc" = unit.descriptions[1],
        "Mu_max.base.unit" = base.units[1],
        "Mu_max.base.val"  = base.values[1],
        "K_s"           = parameters[2],
        "K_s.id"        = K_s.id,
        "K_s.val"       = param.vals[2],
        "K_s.unit"      = param.units[2],
        "K_s.unit.desc" = unit.descriptions[2],
        "K_s.base.unit" = base.units[2],
        "K_s.base.val"  = base.values[2]
      )
      
      n <- length(rv.REACTIONS$monodGrowth)
      rv.REACTIONS$monodGrowth[[n + 1]] <- sub.entry
      names(rv.REACTIONS$monodGrowth)[n + 1] <- ID.to.add
    }
  else if (input$eqnCreate_reaction_law == "predator_prey") {
    # Create reaction entries for predator_prey (after par.ids is created)
    # Re-define all variables to ensure they're available
    species.x    <- input$PI_pred_prey_prey
    species.y    <- input$PI_pred_prey_predator
    species.id.x <- FindId(species.x)
    species.id.y <- FindId(species.y)
    
    # Re-define rate laws and parameters to ensure they're available
    r.name <- input$TI_pred_prey_r
    a.name <- input$TI_pred_prey_a
    b.name <- input$TI_pred_prey_b
    d.name <- input$TI_pred_prey_d
    
    rate.law.x <- paste0(r.name, "*", species.x, "-", a.name, "*", species.x, "*", species.y)
    rate.law.y <- paste0(b.name, "*", species.x, "*", species.y, "-", d.name, "*", species.y)
    
    # Re-define other variables from first block
    eqn.display  <- "Predator–Prey"
    backend.call <- "predator_prey"
    eqn.d        <- paste0("Predator–prey interaction between ", species.x, " (prey) and ", species.y, " (predator)")
    eqn.text     <- paste0(species.x, " <-->(predator-prey) ", species.y)
    mathjax.law  <- paste0("\\begin{aligned}",
                          "\\frac{d", Var2MathJ(species.x), "}{dt} &= ", Var2MathJ(r.name), Var2MathJ(species.x),
                          "-", Var2MathJ(a.name), Var2MathJ(species.x), Var2MathJ(species.y), " \\\\",
                          "\\frac{d", Var2MathJ(species.y), "}{dt} &= ", Var2MathJ(b.name), Var2MathJ(species.x), Var2MathJ(species.y),
                          "-", Var2MathJ(d.name), Var2MathJ(species.y),
                          "\\end{aligned}")
    latex.law    <- rate.law.x  # Used for display, but actual rate laws are in String.Rate.Law
    
    if (exists("par.ids") && length(par.ids) >= 4) {
      r.id <- par.ids[1]
      a.id <- par.ids[2]
      b.id <- par.ids[3]
      d.id <- par.ids[4]
    } else {
      r.id <- NA; a.id <- NA; b.id <- NA; d.id <- NA
    }
    
    # Create reaction entry for prey (X) - main visible entry
    sub.entry.x <- list(
      "ID"               = ID.to.add,
      "Eqn.Display.Type" = eqn.display,
      "Reaction.Law"     = input$eqnCreate_reaction_law,
      "Backend.Call"     = backend.call,
      "Species"          = species.x,
      "Reactants"        = NA,
      "Products"         = NA, 
      "Modifiers"        = NA,
      "Parameters"       = collapseVector(parameters),
      "Compartment"      = compartment,
      "Description"      = eqn.d,
      "Species.id"       = species.id.x,
      "Reactants.id"     = NA,
      "Products.id"      = NA,
      "Modifiers.id"     = NA, 
      "Parameters.id"    = collapseVector(par.ids),
      "Compartment.id"   = compartment.id,
      "Equation.Text"    = eqn.text,
      "Equation.Latex"   = latex.law,
      "Equation.MathJax" = mathjax.law,
      "String.Rate.Law"  = rate.law.x,
      "Pretty.Rate.Law"  = rate.law.x,
      "Latex.Rate.Law"   = rate.law.x,
      "MathJax.Rate.Law" = ConvertRateLaw(rate.law.x)$mathjax,
      "MathMl.Rate.Law"  = NA,
      "Content.MathMl"   = NA,
      "Reversible"       = FALSE,
      "Show.In.Table"    = TRUE
    )
    n.eqns <- length(rv.REACTIONS$reactions)
    rv.REACTIONS$reactions[[n.eqns + 1]] <- sub.entry.x
    names(rv.REACTIONS$reactions)[n.eqns + 1] <- ID.to.add
    
    # Create reaction entry for predator (Y)
    gen2 <- GenerateId(rv.ID$id.eqn.seed, "equation")
    rv.ID$id.eqn.seed <- gen2$seed
    ID.to.add.y <- gen2$id
    
    sub.entry.y <- sub.entry.x
    sub.entry.y$ID               <- ID.to.add.y
    sub.entry.y$Species          <- species.y
    sub.entry.y$Species.id       <- species.id.y
    sub.entry.y$String.Rate.Law  <- rate.law.y
    sub.entry.y$Pretty.Rate.Law  <- rate.law.y
    sub.entry.y$Latex.Rate.Law   <- rate.law.y
    sub.entry.y$MathJax.Rate.Law <- ConvertRateLaw(rate.law.y)$mathjax
    sub.entry.y$Show.In.Table    <- FALSE  # Hide from table - internal only, prey entry is the main one
    
    rv.REACTIONS$reactions[[n.eqns + 2]] <- sub.entry.y
    names(rv.REACTIONS$reactions)[n.eqns + 2] <- ID.to.add.y
    
    # Store compact entry in predatorPrey RV
    pp.entry <- list(
      "ID"           = ID.to.add,
      "Reaction.Law" = input$eqnCreate_reaction_law,
      "Prey"         = species.x,
      "Prey.id"      = species.id.x,
      "Predator"     = species.y,
      "Predator.id"  = species.id.y,
      "r"            = parameters[1],
      "r.id"         = r.id,
      "r.val"        = param.vals[1],
      "a"            = parameters[2],
      "a.id"         = a.id,
      "a.val"        = param.vals[2],
      "b"            = parameters[3],
      "b.id"         = b.id,
      "b.val"        = param.vals[3],
      "d"            = parameters[4],
      "d.id"         = d.id,
      "d.val"        = param.vals[4]
    )
    npp <- length(rv.REACTIONS$predatorPrey)
    rv.REACTIONS$predatorPrey[[npp + 1]] <- pp.entry
    names(rv.REACTIONS$predatorPrey)[npp + 1] <- ID.to.add
    
    # Link species to their respective reaction IDs
    # Species X (prey) -> ID.to.add (has rate.law.x)
    if (is.na(rv.SPECIES$species[[species.id.x]]$Reaction.ids) || rv.SPECIES$species[[species.id.x]]$Reaction.ids == "") {
      rv.SPECIES$species[[species.id.x]]$Reaction.ids <- ID.to.add
    } else {
      items <- strsplit(rv.SPECIES$species[[species.id.x]]$Reaction.ids, ", ")[[1]]
      if (!ID.to.add %in% items) {
        items <- c(items, ID.to.add)
        rv.SPECIES$species[[species.id.x]]$Reaction.ids <- paste0(items, collapse = ", ")
      }
    }
    
    # Species Y (predator) -> ID.to.add.y (has rate.law.y)
    if (is.na(rv.SPECIES$species[[species.id.y]]$Reaction.ids) || rv.SPECIES$species[[species.id.y]]$Reaction.ids == "") {
      rv.SPECIES$species[[species.id.y]]$Reaction.ids <- ID.to.add.y
    } else {
      items <- strsplit(rv.SPECIES$species[[species.id.y]]$Reaction.ids, ", ")[[1]]
      if (!ID.to.add.y %in% items) {
        items <- c(items, ID.to.add.y)
        rv.SPECIES$species[[species.id.y]]$Reaction.ids <- paste0(items, collapse = ", ")
      }
    }
  }
  else if (input$eqnCreate_reaction_law == "competitive_monod") {
    # Check if single species mode
    single.species.mode <- isTruthy(input$CB_comp_monod_single_species)
    
    # Create reaction entries based on mode
    # First species X - this is the MAIN entry to show in table
    sub.entry.x <- list(
      "ID"               = ID.to.add,
      "Eqn.Display.Type" = eqn.display,
      "Reaction.Law"     = input$eqnCreate_reaction_law,
      "Backend.Call"     = backend.call,
      "Species"          = species.x,
      "Reactants"        = substrate,
      "Products"         = species.x, 
      "Modifiers"        = if (single.species.mode) species.y else NA,
      "Parameters"       = collapseVector(parameters),
      "Compartment"      = compartment,
      "Description"      = eqn.d,
      "Species.id"       = species.id.x,
      "Reactants.id"     = substrate.id,
      "Products.id"      = species.id.x,
      "Modifiers.id"     = if (single.species.mode) species.id.y else NA, 
      "Parameters.id"    = collapseVector(par.ids),
      "Compartment.id"   = compartment.id,
      "Equation.Text"    = if (single.species.mode) 
                             paste0("competitive monod (", species.x, " with ", species.y, " as competitor, ", substrate, ")") 
                           else 
                             paste0("competitive monod (", species.x, ",", species.y, ",", substrate, ")"),
      "Equation.Latex"   = latex.law,
      "Equation.MathJax" = mathjax.law,
      "String.Rate.Law"  = rate.law.x,
      "Pretty.Rate.Law"  = rate.law.x,
      "Latex.Rate.Law"   = rate.law.x,
      "MathJax.Rate.Law" = ConvertRateLaw(rate.law.x)$mathjax,
      "MathMl.Rate.Law"  = NA,
      "Content.MathMl"   = NA,
      "Reversible"       = FALSE,
      "Show.In.Table"    = TRUE  # Mark this as the main entry to display
    )
    n.eqns <- length(rv.REACTIONS$reactions)
    rv.REACTIONS$reactions[[n.eqns + 1]] <- sub.entry.x
    names(rv.REACTIONS$reactions)[n.eqns+1] <- ID.to.add
    
    if (!single.species.mode) {
      # Both species mode: create second reaction entry for Y
      gen2 <- GenerateId(rv.ID$id.eqn.seed, "equation")
      rv.ID$id.eqn.seed <- gen2$seed
      ID.to.add.y <- gen2$id
      sub.entry.y <- sub.entry.x
      sub.entry.y$ID               <- ID.to.add.y
      sub.entry.y$Species          <- species.y
      sub.entry.y$Species.id       <- species.id.y
      sub.entry.y$Products        <- species.y
      sub.entry.y$Products.id      <- species.id.y
      sub.entry.y$Modifiers        <- NA
      sub.entry.y$Modifiers.id     <- NA
      sub.entry.y$String.Rate.Law  <- rate.law.y
      sub.entry.y$Pretty.Rate.Law  <- rate.law.y
      sub.entry.y$Latex.Rate.Law   <- rate.law.y
      sub.entry.y$MathJax.Rate.Law <- ConvertRateLaw(rate.law.y)$mathjax
      sub.entry.y$Show.In.Table    <- FALSE  # Hide from table - internal only
      rv.REACTIONS$reactions[[n.eqns + 2]] <- sub.entry.y
      names(rv.REACTIONS$reactions)[n.eqns+2] <- ID.to.add.y
    }
    
    # Substrate S from X reaction -> new ID
    gen3 <- GenerateId(rv.ID$id.eqn.seed, "equation")
    rv.ID$id.eqn.seed <- gen3$seed
    ID.to.add.s.x <- gen3$id
    sub.entry.s.x <- sub.entry.x
    sub.entry.s.x$ID               <- ID.to.add.s.x
    sub.entry.s.x$Species          <- substrate
    sub.entry.s.x$Species.id       <- substrate.id
    sub.entry.s.x$Reactants        <- substrate
    sub.entry.s.x$Reactants.id     <- substrate.id
    sub.entry.s.x$Products         <- NA
    sub.entry.s.x$Products.id      <- NA
    sub.entry.s.x$Modifiers        <- NA
    sub.entry.s.x$Modifiers.id     <- NA
    sub.entry.s.x$String.Rate.Law  <- rate.law.s.x
    sub.entry.s.x$Pretty.Rate.Law  <- rate.law.s.x
    sub.entry.s.x$Latex.Rate.Law   <- rate.law.s.x
    sub.entry.s.x$MathJax.Rate.Law <- ConvertRateLaw(rate.law.s.x)$mathjax
    sub.entry.s.x$Show.In.Table    <- FALSE  # Hide from table - internal only
    rv.REACTIONS$reactions[[n.eqns + 3]] <- sub.entry.s.x
    names(rv.REACTIONS$reactions)[n.eqns+3] <- ID.to.add.s.x
    
    if (!single.species.mode) {
      # Both species mode: create substrate S from Y reaction -> new ID
      gen4 <- GenerateId(rv.ID$id.eqn.seed, "equation")
      rv.ID$id.eqn.seed <- gen4$seed
      ID.to.add.s.y <- gen4$id
      sub.entry.s.y <- sub.entry.s.x
      sub.entry.s.y$ID               <- ID.to.add.s.y
      sub.entry.s.y$String.Rate.Law  <- rate.law.s.y
      sub.entry.s.y$Pretty.Rate.Law  <- rate.law.s.y
      sub.entry.s.y$Latex.Rate.Law   <- rate.law.s.y
      sub.entry.s.y$MathJax.Rate.Law <- ConvertRateLaw(rate.law.s.y)$mathjax
      sub.entry.s.y$Show.In.Table    <- FALSE  # Hide from table - internal only
      rv.REACTIONS$reactions[[n.eqns + 4]] <- sub.entry.s.y
      names(rv.REACTIONS$reactions)[n.eqns+4] <- ID.to.add.s.y
    }
    
    # Track in competitiveMonod RV (single entry)
    mu_max.x.id  <- par.ids[1]
    K_s.x.id     <- par.ids[2]
    alpha.xy.id  <- par.ids[3]
    Kc.id        <- par.ids[4]
    Y_x.id       <- par.ids[5]
    
    if (single.species.mode) {
      cm.entry <- list(
        "ID"           = ID.to.add,
        "Reaction.Law" = input$eqnCreate_reaction_law,
        "Single.Species.Mode" = TRUE,
        "No.Substrate.Restriction" = no.substrate.restriction,
        "Species.X"    = species.x,
        "Species.X.id" = species.id.x,
        "Species.Y"    = species.y,
        "Species.Y.id" = species.id.y,
        "Substrate"    = substrate,
        "Substrate.id" = substrate.id,
        "mu_max.x"     = parameters[1],
        "mu_max.x.id"  = mu_max.x.id,
        "K_s.x"        = parameters[2],
        "K_s.x.id"     = K_s.x.id,
        "alpha.xy"     = parameters[3],
        "alpha.xy.id"  = alpha.xy.id,
        "Kc"           = parameters[4],
        "Kc.id"        = Kc.id,
        "Y_x"          = parameters[5],
        "Y_x.id"       = Y_x.id
      )
    } else {
      mu_max.y.id  <- par.ids[2]
      K_s.y.id     <- par.ids[4]
      alpha.yx.id  <- par.ids[6]
      Y_y.id       <- par.ids[9]
      cm.entry <- list(
        "ID"           = ID.to.add,
        "Reaction.Law" = input$eqnCreate_reaction_law,
        "Single.Species.Mode" = FALSE,
        "No.Substrate.Restriction" = no.substrate.restriction,
        "Species.X"    = species.x,
        "Species.X.id" = species.id.x,
        "Species.Y"    = species.y,
        "Species.Y.id" = species.id.y,
        "Substrate"    = substrate,
        "Substrate.id" = substrate.id,
        "mu_max.x"     = parameters[1],
        "mu_max.x.id"  = mu_max.x.id,
        "mu_max.y"     = parameters[2],
        "mu_max.y.id"  = mu_max.y.id,
        "K_s.x"        = parameters[3],
        "K_s.x.id"     = K_s.x.id,
        "K_s.y"        = parameters[4],
        "K_s.y.id"     = K_s.y.id,
        "alpha.xy"     = parameters[5],
        "alpha.xy.id"  = alpha.xy.id,
        "alpha.yx"     = parameters[6],
        "alpha.yx.id"  = alpha.yx.id,
        "Kc"           = parameters[7],
        "Kc.id"        = Kc.id,
        "Y_x"          = parameters[8],
        "Y_x.id"       = Y_x.id,
        "Y_y"          = parameters[9],
        "Y_y.id"       = Y_y.id
      )
    }
    ncm <- length(rv.REACTIONS$competitiveMonod)
    rv.REACTIONS$competitiveMonod[[ncm+1]] <- cm.entry
    names(rv.REACTIONS$competitiveMonod)[ncm+1] <- ID.to.add
    
    # Link species to their respective reaction IDs
    # Species X -> ID.to.add (has rate.law.x)
    if (is.na(rv.SPECIES$species[[species.id.x]]$Reaction.ids)) {
      rv.SPECIES$species[[species.id.x]]$Reaction.ids <- ID.to.add
    } else {
      items <- strsplit(rv.SPECIES$species[[species.id.x]]$Reaction.ids, ", ")[[1]]
      items <- c(items, ID.to.add)
      rv.SPECIES$species[[species.id.x]]$Reaction.ids <- paste0(items, collapse = ", ")
    }
    
    if (!single.species.mode) {
      # Species Y -> ID.to.add.y (has rate.law.y) - only in both species mode
      if (is.na(rv.SPECIES$species[[species.id.y]]$Reaction.ids)) {
        rv.SPECIES$species[[species.id.y]]$Reaction.ids <- ID.to.add.y
      } else {
        items <- strsplit(rv.SPECIES$species[[species.id.y]]$Reaction.ids, ", ")[[1]]
        items <- c(items, ID.to.add.y)
        rv.SPECIES$species[[species.id.y]]$Reaction.ids <- paste0(items, collapse = ", ")
      }
    }
    
    # Substrate S -> ID.to.add.s.x (and ID.to.add.s.y if both species mode)
    if (single.species.mode) {
      # Only consumption from X
      if (is.na(rv.SPECIES$species[[substrate.id]]$Reaction.ids)) {
        rv.SPECIES$species[[substrate.id]]$Reaction.ids <- ID.to.add.s.x
      } else {
        items <- strsplit(rv.SPECIES$species[[substrate.id]]$Reaction.ids, ", ")[[1]]
        items <- c(items, ID.to.add.s.x)
        rv.SPECIES$species[[substrate.id]]$Reaction.ids <- paste0(items, collapse = ", ")
      }
    } else {
      # Consumption from both X and Y
      if (is.na(rv.SPECIES$species[[substrate.id]]$Reaction.ids)) {
        rv.SPECIES$species[[substrate.id]]$Reaction.ids <- paste0(ID.to.add.s.x, ", ", ID.to.add.s.y)
      } else {
        items <- strsplit(rv.SPECIES$species[[substrate.id]]$Reaction.ids, ", ")[[1]]
        items <- c(items, ID.to.add.s.x, ID.to.add.s.y)
        rv.SPECIES$species[[substrate.id]]$Reaction.ids <- paste0(items, collapse = ", ")
      }
    }
  }
  else if (input$eqnCreate_reaction_law == "logistic_competition") {
    # Check if single species mode
    single.species.mode <- isTruthy(input$CB_log_comp_single_species)
    
    # Create reaction entry(ies) based on mode
    # First species X (always created) - this is the MAIN entry to show in table
    sub.entry.x <- list(
      "ID"               = ID.to.add,
      "Eqn.Display.Type" = eqn.display,
      "Reaction.Law"     = input$eqnCreate_reaction_law,
      "Backend.Call"     = backend.call,
      "Species"          = species.x,
      "Reactants"        = NA,
      "Products"         = NA, 
      "Modifiers"        = if (single.species.mode) species.y else NA,
      "Parameters"       = collapseVector(parameters),
      "Compartment"      = compartment,
      "Description"      = eqn.d,
      "Species.id"       = species.id.x,
      "Reactants.id"     = NA,
      "Products.id"      = NA,
      "Modifiers.id"     = if (single.species.mode) species.id.y else NA, 
      "Parameters.id"    = collapseVector(par.ids),
      "Compartment.id"   = compartment.id,
      "Equation.Text"    = if (single.species.mode) 
                             paste0("logistic competition (", species.x, " with ", species.y, " as competitor)") 
                           else 
                             paste0("logistic competition (", species.x, ",", species.y, ")"),
      "Equation.Latex"   = latex.law,
      "Equation.MathJax" = mathjax.law,
      "String.Rate.Law"  = rate.law.x,
      "Pretty.Rate.Law"  = rate.law.x,
      "Latex.Rate.Law"   = rate.law.x,
      "MathJax.Rate.Law" = ConvertRateLaw(rate.law.x)$mathjax,
      "MathMl.Rate.Law"  = NA,
      "Content.MathMl"   = NA,
      "Reversible"       = FALSE,
      "Show.In.Table"    = TRUE  # Mark this as the main entry to display
    )
    n.eqns <- length(rv.REACTIONS$reactions)
    rv.REACTIONS$reactions[[n.eqns + 1]] <- sub.entry.x
    names(rv.REACTIONS$reactions)[n.eqns+1] <- ID.to.add
    
    if (!single.species.mode) {
      # Both species mode: create second reaction entry for Y
      gen2 <- GenerateId(rv.ID$id.eqn.seed, "equation")
      rv.ID$id.eqn.seed <- gen2$seed
      ID.to.add.y <- gen2$id
      sub.entry.y <- sub.entry.x
      sub.entry.y$ID               <- ID.to.add.y
      sub.entry.y$Species          <- species.y
      sub.entry.y$Species.id       <- species.id.y
      sub.entry.y$Modifiers        <- NA
      sub.entry.y$Modifiers.id     <- NA
      sub.entry.y$String.Rate.Law  <- rate.law.y
      sub.entry.y$Pretty.Rate.Law  <- rate.law.y
      sub.entry.y$Latex.Rate.Law   <- rate.law.y
      sub.entry.y$MathJax.Rate.Law <- ConvertRateLaw(rate.law.y)$mathjax
      sub.entry.y$Show.In.Table    <- FALSE  # Hide from table - internal only
      # Add second reaction
      rv.REACTIONS$reactions[[n.eqns + 2]] <- sub.entry.y
      names(rv.REACTIONS$reactions)[n.eqns+2] <- ID.to.add.y
    }
    
    # Track in logisticCompetition RV (single entry)
    r.x.id      <- par.ids[1]
    alpha.xy.id <- par.ids[2]
    Kc.id       <- par.ids[3]
    
    if (single.species.mode) {
      lc.entry <- list(
        "ID"           = ID.to.add,
        "Reaction.Law" = input$eqnCreate_reaction_law,
        "Single.Species.Mode" = TRUE,
        "Species.X"    = species.x,
        "Species.X.id" = species.id.x,
        "Species.Y"    = species.y,
        "Species.Y.id" = species.id.y,
        "r.x"          = parameters[1],
        "r.x.id"       = r.x.id,
        "alpha.xy"     = parameters[2],
        "alpha.xy.id"  = alpha.xy.id,
        "Kc"           = parameters[3],
        "Kc.id"        = Kc.id
      )
    } else {
      r.y.id      <- par.ids[2]
      alpha.yx.id <- par.ids[4]
      lc.entry <- list(
        "ID"           = ID.to.add,
        "Reaction.Law" = input$eqnCreate_reaction_law,
        "Single.Species.Mode" = FALSE,
        "Species.X"    = species.x,
        "Species.X.id" = species.id.x,
        "Species.Y"    = species.y,
        "Species.Y.id" = species.id.y,
        "r.x"          = parameters[1],
        "r.x.id"       = r.x.id,
        "r.y"          = parameters[2],
        "r.y.id"       = r.y.id,
        "alpha.xy"     = parameters[3],
        "alpha.xy.id"  = alpha.xy.id,
        "alpha.yx"     = parameters[4],
        "alpha.yx.id"  = alpha.yx.id,
        "Kc"           = parameters[5],
        "Kc.id"        = Kc.id
      )
    }
    nlc <- length(rv.REACTIONS$logisticCompetition)
    rv.REACTIONS$logisticCompetition[[nlc+1]] <- lc.entry
    names(rv.REACTIONS$logisticCompetition)[nlc+1] <- ID.to.add
    
    # Link species to their respective reaction IDs
    # Species X -> ID.to.add (has rate.law.x)
    if (is.na(rv.SPECIES$species[[species.id.x]]$Reaction.ids)) {
      rv.SPECIES$species[[species.id.x]]$Reaction.ids <- ID.to.add
    } else {
      items <- strsplit(rv.SPECIES$species[[species.id.x]]$Reaction.ids, ", ")[[1]]
      items <- c(items, ID.to.add)
      rv.SPECIES$species[[species.id.x]]$Reaction.ids <- paste0(items, collapse = ", ")
    }
    
    if (!single.species.mode) {
      # Species Y -> ID.to.add.y (has rate.law.y) - only in both species mode
      if (is.na(rv.SPECIES$species[[species.id.y]]$Reaction.ids)) {
        rv.SPECIES$species[[species.id.y]]$Reaction.ids <- ID.to.add.y
      } else {
        items <- strsplit(rv.SPECIES$species[[species.id.y]]$Reaction.ids, ", ")[[1]]
        items <- c(items, ID.to.add.y)
        rv.SPECIES$species[[species.id.y]]$Reaction.ids <- paste0(items, collapse = ", ")
      }
    }
  }
  else if (input$eqnCreate_reaction_law == "substrate_synthesis_competition") {
    # Storage happens here after par.ids is built in main code block
    # Determine parameter IDs from par.ids (built in main code block)
    if (exists("par.ids") && length(par.ids) >= 3) {
      k.id      <- par.ids[1]
      alpha.id  <- par.ids[2]
      Kc.id     <- par.ids[3]
    } else {
      # Fallback if par.ids doesn't exist (shouldn't happen, but safety check)
      k.id      <- NA
      alpha.id  <- NA
      Kc.id     <- NA
    }
    
    # Create main reaction entry (for species synthesis)
    sub.entry <- list(
      "ID"               = ID.to.add,
      "Eqn.Display.Type" = eqn.display,
      "Reaction.Law"     = input$eqnCreate_reaction_law,
      "Backend.Call"     = backend.call,
      "Species"          = species,
      "Reactants"        = substrate,
      "Products"         = species,
      "Modifiers"        = if (!is.na(competitor.id)) competitor else NA,
      "Parameters"       = collapseVector(parameters),
      "Compartment"      = compartment,
      "Description"      = eqn.d,
      "Species.id"       = species.id,
      "Reactants.id"     = substrate.id,
      "Products.id"      = species.id,
      "Modifiers.id"     = if (!is.na(competitor.id)) competitor.id else NA,
      "Parameters.id"    = if (exists("par.ids")) collapseVector(par.ids) else NA,
      "Compartment.id"    = compartment.id,
      "Equation.Text"    = eqn.d,
      "Equation.Latex"   = latex.law,
      "Equation.MathJax" = mathjax.law,
      "String.Rate.Law"  = rate.law,
      "Latex.Rate.Law"   = latex.law,
      "MathJax.Rate.Law" = mathjax.law,
      "Show.In.Table"    = TRUE
    )
    
    # Initialize if NULL
    if (is.null(rv.REACTIONS$reactions)) {
      rv.REACTIONS$reactions <- list()
    }
    n <- length(rv.REACTIONS$reactions)
    rv.REACTIONS$reactions[[n + 1]] <- sub.entry
    # Get existing names - must match current list length
    existing.names <- names(rv.REACTIONS$reactions)
    if (is.null(existing.names)) {
      existing.names <- rep("", n + 1)
    } else {
      # Ensure names vector matches list length
      if (length(existing.names) != n + 1) {
        existing.names <- c(existing.names[1:n], rep("", n + 1 - length(existing.names)))
      }
    }
    # Set the name for the new entry
    existing.names[n + 1] <- ID.to.add
    names(rv.REACTIONS$reactions) <- existing.names
    
    # Create substrate consumption entry (negative rate)
    ID.to.add.s <- paste0("SSC_S_", ID.to.add)
    # Remove volume wrapper, negate, then re-add volume
    rate.law.inner <- gsub(paste0("^", volume.var, "\\*\\("), "", rate.law)
    rate.law.inner <- gsub("\\)$", "", rate.law.inner)
    rate.law.s <- paste0(volume.var, "*(-(", rate.law.inner, "))")
    
    sub.entry.s <- list(
      "ID"               = ID.to.add.s,
      "Eqn.Display.Type" = eqn.display,
      "Reaction.Law"     = input$eqnCreate_reaction_law,
      "Backend.Call"     = backend.call,
      "Species"          = substrate,
      "Reactants"        = substrate,
      "Products"         = NA,
      "Modifiers"        = if (!is.na(competitor.id)) competitor else NA,
      "Parameters"       = collapseVector(parameters),
      "Compartment"      = compartment,
      "Description"      = paste0("Substrate consumption: ", eqn.d),
      "Species.id"       = substrate.id,
      "Reactants.id"     = substrate.id,
      "Products.id"      = NA,
      "Modifiers.id"     = if (!is.na(competitor.id)) competitor.id else NA,
      "Parameters.id"    = if (exists("par.ids")) collapseVector(par.ids) else NA,
      "Compartment.id"   = compartment.id,
      "Equation.Text"    = paste0("Substrate consumption: ", eqn.d),
      "Equation.Latex"   = paste0("-", latex.law),
      "Equation.MathJax" = paste0("-", mathjax.law),
      "String.Rate.Law"  = rate.law.s,
      "Latex.Rate.Law"   = paste0("-", latex.law),
      "MathJax.Rate.Law" = paste0("-", mathjax.law),
      "Show.In.Table"    = FALSE
    )
    
    # Add substrate consumption entry to reactions
    # Note: reactions list should already exist from above, but check just in case
    if (is.null(rv.REACTIONS$reactions)) {
      rv.REACTIONS$reactions <- list()
    }
    n.s <- length(rv.REACTIONS$reactions)
    rv.REACTIONS$reactions[[n.s + 1]] <- sub.entry.s
    # Get existing names - must match current list length
    existing.names.s <- names(rv.REACTIONS$reactions)
    if (is.null(existing.names.s)) {
      existing.names.s <- rep("", n.s + 1)
    } else {
      # Ensure names vector matches list length
      if (length(existing.names.s) != n.s + 1) {
        existing.names.s <- c(existing.names.s[1:n.s], rep("", n.s + 1 - length(existing.names.s)))
      }
    }
    # Set the name for the new entry
    existing.names.s[n.s + 1] <- ID.to.add.s
    names(rv.REACTIONS$reactions) <- existing.names.s
    
    # Store in substrateSynthesisCompetition list
    ssc.entry <- list(
      "ID"                = ID.to.add,
      "Reaction.Law"      = input$eqnCreate_reaction_law,
      "Species"           = species,
      "Species.id"        = species.id,
      "Substrate"         = substrate,
      "Substrate.id"      = substrate.id,
      "Competitor"        = if (!is.na(competitor.id)) competitor else NA,
      "Competitor.id"     = if (!is.na(competitor.id)) competitor.id else NA,
      "Species.Dependent" = species.dependent,
      "k"                 = parameters[1],
      "k.id"              = if (exists("par.ids") && length(par.ids) >= 1) par.ids[1] else NA,
      "k.val"              = param.vals[1],
      "alpha"              = parameters[2],
      "alpha.id"           = if (exists("par.ids") && length(par.ids) >= 2) par.ids[2] else NA,
      "alpha.val"          = param.vals[2],
      "Kc"                 = parameters[3],
      "Kc.id"              = if (exists("par.ids") && length(par.ids) >= 3) par.ids[3] else NA,
      "Kc.val"             = param.vals[3]
    )
    
    # Initialize if NULL or empty
    if (is.null(rv.REACTIONS$substrateSynthesisCompetition)) {
      rv.REACTIONS$substrateSynthesisCompetition <- list()
    }
    n.ssc <- length(rv.REACTIONS$substrateSynthesisCompetition)
    rv.REACTIONS$substrateSynthesisCompetition[[n.ssc + 1]] <- ssc.entry
    # Get existing names - must match current list length
    existing.names.ssc <- names(rv.REACTIONS$substrateSynthesisCompetition)
    if (is.null(existing.names.ssc)) {
      existing.names.ssc <- rep("", n.ssc + 1)
    } else {
      # Ensure names vector matches list length
      if (length(existing.names.ssc) != n.ssc + 1) {
        existing.names.ssc <- c(existing.names.ssc[1:n.ssc], rep("", n.ssc + 1 - length(existing.names.ssc)))
      }
    }
    # Set the name for the new entry
    existing.names.ssc[n.ssc + 1] <- ID.to.add
    names(rv.REACTIONS$substrateSynthesisCompetition) <- existing.names.ssc
    
    # Link species to reaction IDs
    if (is.na(rv.SPECIES$species[[species.id]]$Reaction.ids)) {
      rv.SPECIES$species[[species.id]]$Reaction.ids <- ID.to.add
    } else {
      items <- strsplit(rv.SPECIES$species[[species.id]]$Reaction.ids, ", ")[[1]]
      items <- c(items, ID.to.add)
      rv.SPECIES$species[[species.id]]$Reaction.ids <- paste0(items, collapse = ", ")
    }
    
    # Link substrate to reaction ID (consumption)
    if (is.na(rv.SPECIES$species[[substrate.id]]$Reaction.ids)) {
      rv.SPECIES$species[[substrate.id]]$Reaction.ids <- ID.to.add.s
    } else {
      items <- strsplit(rv.SPECIES$species[[substrate.id]]$Reaction.ids, ", ")[[1]]
      items <- c(items, ID.to.add.s)
      rv.SPECIES$species[[substrate.id]]$Reaction.ids <- paste0(items, collapse = ", ")
    }
  }
    else if (input$eqnCreate_reaction_law == "mass_action_w_reg") {
     
       pc <- 1
      # Determine with param ids are which
      if (!is.na(kf)) {
        kf.id <- par.ids[pc]
        pc <- pc + 1
      }
       
      if (!is.na(kr)) {
        kr.id <- par.ids[pc]
        pc <- pc + 1
      }

      if (has.f.reg) {
        n.f.reg <- length(strsplit(Forward.Pars, ", ")[[1]])
        Forward.Pars.id <- par.ids[pc:(pc+n.f.reg-1)]
        pc <- pc + n.f.reg
        Forward.Pars.id <- paste0(Forward.Pars.id, collapse = ", ")
      } else {
        Forward.Pars.id <- NA
      }
      
      if (has.r.reg) {
        n.r.reg <- length(strsplit(Reverse.Pars, ", ")[[1]])
        Reverse.Pars.id <- par.ids[pc:(pc+n.r.reg-1)]
        Reverse.Pars.id <- paste0(Reverse.Pars.id, collapse = ", ")
      } else {
        Reverse.Pars.id <- NA
      }
      
      sub.entry <- list(
        "ID" = ID.to.add,
        "Reaction.Law"    = input$eqnCreate_reaction_law,
        "r.stoichiometry" = r.stoich,
        "Reactants"       = reactants,
        "Reactants.id"    = reactants.id,
        "p.stoichiometry" = p.stoich,
        "Products"        = products,
        "Products.id"     = products.id,
        "Reversible"      = reversible,
        "kf"              = kf,
        "kr"              = kr,
        "kf.id"           = kf.id,
        "kr.id"           = kr.id,
        "Use.Forward.Mod" = has.f.reg,
        "Forward.Mods"    = Forward.Mods,
        "Forward.Mods.id" = Forward.Mods.id,
        "Forward.Pars"   = Forward.Pars,
        "Forward.Pars.id" = Forward.Pars.id,
        "Use.Reverse.Mod" = has.r.reg,
        "Reverse.Mods"    = Reverse.Mods,
        "Reverse.Mods.id" = Reverse.Mods.id,
        "Reverse.Pars"    = Reverse.Pars,
        "Reverse.Pars.id" = Reverse.Pars.id
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$massActionwReg)
      rv.REACTIONS$massActionwReg[[n+1]] <- sub.entry
      names(rv.REACTIONS$massActionwReg)[n+1] <- ID.to.add
    }
    else if (input$eqnCreate_reaction_law == "synthesis") {
      sub.entry <- list(
        "ID"               = ID.to.add,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "VarSyn"           = var.syn,
        "VarSyn.id"        = var.syn.id,
        "Rate.Constant"    = parameter,
        "Rate.Constant.id" = par.ids[1],
        "Factor"           = factor,
        "Factor.id"        = factor.id
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$synthesis)
      rv.REACTIONS$synthesis[[n+1]] <- sub.entry
      names(rv.REACTIONS$synthesis)[n+1] <- ID.to.add
      
    }
    else if (input$eqnCreate_reaction_law == "degradation_rate") {
      # Determine krel.param.id - it will be par.ids[2] if products exist AND relative formation is checked, otherwise NA
      krel.param.id <- NA
      if (input$CB_degradation_rate_toProducts && isTruthy(input$CB_degradation_rate_relative_formation) && length(par.ids) >= 2) {
        krel.param.id <- par.ids[2]
      }
      
      sub.entry <- list(
        "ID"               = ID.to.add,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "VarDeg"           = deg.species,
        "VarDeg.id"        = deg.species.id,
        "ConcDep"          = ConcDep,
        "Rate.Constant"    = parameter,
        "Rate.Constant.id" = par.ids[1],
        "Products"         = products.collapsed,
        "Products.id"      = products.id.collapsed,
        "krel"             = krel.param,
        "krel.id"          = krel.param.id
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$degradation.by.rate)
      rv.REACTIONS$degradation.by.rate[[n+1]] <- sub.entry
      names(rv.REACTIONS$degradation.by.rate)[n+1] <- ID.to.add
    }
    else if (input$eqnCreate_reaction_law == "degradation_by_enzyme") {
      # Gets ids based on use.Vmax
      Vmax.id <- NA
      kcat.id <- NA
      Km.id   <- if (length(par.ids) >= 1) par.ids[1] else NA
      
      if (Use.Vmax) {
        if (length(par.ids) >= 2) {
          Vmax.id <- par.ids[2]
        }
      } else {
        if (length(par.ids) >= 2) {
          kcat.id <- par.ids[2]
        }
      }
      
      # Determine krel.param.id - it will be the last parameter ID if krel exists
      # krel.param was added to parameters earlier, so if it exists, it will be the last one
      krel.param.id <- NA
      krel.param.value <- NA
      if (input$CB_degradation_enzyme_toProducts && isTruthy(input$CB_degradation_enzyme_relative_formation)) {
        krel.param.value <- input$TI_degradation_enzyme_krel
        if (length(par.ids) >= 3) {
          krel.param.id <- par.ids[length(par.ids)]
        }
      }
      
      sub.entry <- list(
        "ID"               = ID.to.add,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "VarDeg"           = deg.species,
        "VarDeg.id"        = deg.species.id,
        "UseVmax"          = Use.Vmax,
        "Km"               = Km,
        "Km.id"            = Km.id,
        "Vmax"             = Vmax,
        "Vmax.id"          = Vmax.id,
        "Enzyme"           = enzyme,
        "Enzyme.id"        = enzyme.id,
        "kcat"             = kcat,
        "kcat.id"          = kcat.id,
        "Products"         = products.collapsed,
        "Products.id"      = products.id.collapsed,
        "krel"             = krel.param.value,
        "krel.id"          = krel.param.id
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$degradation.by.enzyme)
      rv.REACTIONS$degradation.by.enzyme[[n+1]] <- sub.entry
      names(rv.REACTIONS$degradation.by.enzyme)[n+1] <- ID.to.add
    }
    else if (input$eqnCreate_reaction_law == "michaelis_menten") {
      # Gets ids based on use.Vmax
      Vmax.id <- NA
      kcat.id <- NA
      Km.id   <- par.ids[1]
      
      if (Use.Vmax) {
        Vmax.id <- par.ids[2]
      } else {
        kcat.id <- par.ids[2]
      }
      
      sub.entry <- list(
        "ID"               = ID.to.add,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "Substrate"        = substrate,
        "Substrate.id"     = substrate.id,
        "Product"          = products,
        "Product.id"       = products.id,
        "UseVmax"          = Use.Vmax,
        "Km"               = Km,
        "Km.id"            = Km.id,
        "Vmax"             = Vmax,
        "Vmax.id"          = Vmax.id,
        "Enzyme"           = enzyme,
        "Enzyme.id"        = enzyme.id,
        "kcat"             = kcat,
        "kcat.id"          = kcat.id
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$michaelisMenten)
      rv.REACTIONS$michaelisMenten[[n+1]] <- sub.entry
      names(rv.REACTIONS$michaelisMenten)[n+1] <- ID.to.add
    }
    
    # Resolve Diffeqs
    solveForDiffEqs()
    
    # Tracks subscripts of eqns
    rv.REACTIONS$reaction.id.counter <- rv.REACTIONS$reaction.id.counter + 1
    
    # Clear equation description box
    updateTextAreaInput(
      session = session, 
      inputId = "TAI_reaction_description_add",
      value = "", 
      placeholder = "Enter your reaction description here."
    )
  }
  

  
  #waiter.rv.REACTIONS$hide()
  w.test$hide()
  
  shinyjs::enable("eqnCreate_addEqnToVector")
  
  if (input$checkbox_modal_keep_active_add_eqn) {
    toggleModal(session,
                "modal_create_equations",
                toggle = "close")
  }
  
})


# Equation Main Table Render ---------------------------------------------------
output$main_eqns_table <- renderRHandsontable({
  override <- rv.REFRESH$refresh.eqn.table
  # Filter reactions to only show entries marked for table display
  # (or entries without Show.In.Table field, for backward compatibility)
  reactions.to.show <- lapply(rv.REACTIONS$reactions, function(r) {
    if (is.null(r$Show.In.Table) || isTRUE(r$Show.In.Table)) {
      return(r)
    } else {
      return(NULL)
    }
  })
  reactions.to.show <- reactions.to.show[!sapply(reactions.to.show, is.null)]
  
  if (length(reactions.to.show) == 0) {
    df <- data.frame()
  } else {
    df <- as_tibble(do.call(rbind, reactions.to.show))
  }
  # df <- bind_rows(rv.REACTIONS$reactions)
  if (nrow(df) == 0) {
  # if (nrow(rv.REACTIONS$reactions.df) == 0) {
    temp <- data.frame(c("Press addition button below to add equations
                       to compartment."))
    temp <- transpose(temp)
    colnames(temp) <- c("Instructions")
    rhandsontable(temp,
                  rowHeaders = NULL,
                  overflow = "visible",
                  colHeaderWidth = 100,
                  stretchH = "all",
                  readOnly = TRUE
    ) %>%
      hot_cols(manualColumnMove = FALSE,
               manualColumnResize = FALSE,
               halign = "htCenter",
               valign = "htMiddle",
               renderer = "
         function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.NumericRenderer.apply(this, arguments);
           if (row % 2 == 0) {
            td.style.background = '#f9f9f9';
           } else {
            td.style.background = 'white';
           };
         }") %>%
      hot_rows(rowHeights = 30) %>%
      hot_context_menu(allowRowEdit = FALSE,
                       allowColEdit = FALSE
      )
    } else {
    df.to.show <- select(df,
                         "Equation.Text",
                         "Eqn.Display.Type",
                         "Compartment")
    
    df.to.show <- as.data.frame(df.to.show)
    colnames(df.to.show) <- c("Equation", 
                              "Type", 
                              "Compartment")
    
    hot <- rhandsontable(df.to.show,
                  overflow = "visible",
                  readOnly = TRUE,
                  selectCallback = TRUE,
                  colHeaderWidth = 100,
                  stretchH = "all",
                  fillHandle = FALSE
    ) %>%
      hot_cols(
        colWidth = c(60, 20, 20, 20),
        manualColumnMove = FALSE,
        manualColumnResize = TRUE,
        halign = "htCenter",
        valign = "htMiddle",
        renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (row % 2 == 0) {
              td.style.background = '#f9f9f9';
              td.style.color = 'black';
             } else {
              td.style.background = 'white';
              td.style.color = 'black';
             };
           }") %>%
      #hot_col("Variable Name", readOnly = TRUE) %>%
      hot_rows(rowHeights = 30) %>%
      hot_context_menu(
        allowRowEdit = FALSE,
        allowColEdit = FALSE
      )
    csv = list(
      name = "Download",
      callback  = htmlwidgets::JS(
        "function (key, options) {
           var csv = csvString(this, sep=',', dec='.');
           var link = document.createElement('a');
           link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
             encodeURIComponent(csv));
           link.setAttribute('download', 'equations.csv');
           document.body.appendChild(link);
           link.click();
           document.body.removeChild(link);
         }"
      )
    )
    
    # context menu callback has 3 inputs, key, selection, clickevent
    eqnEdit = list(
      name = "Edit",
      callback = htmlwidgets::JS(
        "function(key, options) {
                Shiny.setInputValue('edit_equation_menu_item', options, {priority: 'event'});
              }"
      )
    )
    
    eqnAdd = list(
      name = "Add",
      callback = htmlwidgets::JS(
        "function(key, options) {
                Shiny.setInputValue('add_equation_menu_item', options, {priority: 'event'});
              }"
      )
    )
    
    eqnDel = list(
      name = "Delete",
      callback = htmlwidgets::JS(
        "function(key, options) {
                Shiny.setInputValue('delete_equation_menu_item', options, {priority: 'event'});
              }"
      )
    )
    
    hot$x$contextMenu <- list(items = list(eqnAdd, 
                                           eqnEdit,
                                           eqnDel,
                                           csv))
    hot
  }
})

observeEvent(input$add_equation_menu_item, {
  toggleModal(
    session = session,
    modalId = "modal_create_equations",
    toggle = "open"
  )
})

observeEvent(input$delete_equation_menu_item, {
  toggleModal(
    session = session,
    modalId = "modal_delete_equations",
    toggle = "open"
  )
})

observeEvent(input$edit_equation_menu_item, {

  start.row <- input$edit_equation_menu_item[[1]]
  start.col <- input$edit_equation_menu_item[[2]]
  end.row   <- input$edit_equation_menu_item[[3]]
  end.col   <- input$edit_equation_menu_item[[4]]
  
  # Update equation number with row from edit
  updatePickerInput(
    session = session, 
    inputId = "eqnCreate_edit_select_equation",
    selected = as.character(start.row+1)
  )
  
  # Open Edit Equation Modal
  toggleModal(
    session = session,
    modalId = "modal_edit_equations",
    toggle = "open"
  )
  
  
})

# Rate Equation Store Parameter/Time Dependent ---------------------------------
observeEvent(input$eqnCreate_time_dependent_store_new_parameter, {
  new_parameter <- input$eqnCreate_time_dependent_parameters
  rv.PARAMETERS$time.dep.vars <- append(rv.PARAMETERS$time.dep.vars, new_parameter)
  updateTextInput(session,
                  "eqnCreate_time_dependent_parameters",
                  value = "")
})

# When Equation Add button pressed, store vars to respective places
observeEvent(input$eqnCreate_addEqnToVector, {
  eqn_type <- input$eqnCreate_type_of_equation
  
  #Add additional parameters in rate equation to proper rv
  if (eqn_type == "rate_eqn") {
    if (isTruthy(input$eqnCreate_rate_new_parameter)) { #if new parameters were entered (var1,var2,var3)
      num.param.to.add <- length(str_split(input$eqnCreate_rate_new_parameter, ","))
      parameters <- str_split(input$eqnCreate_rate_new_parameter, ",")
      for (i in seq(num.param.to.add)) {
        new.parameter <- gsub(" ", "", parameters[[i]], fixed = TRUE)
        phrase <- paste0("Added Param ", new.parameter)
        #rv.PARAMETERS$rate.eqn.vars <- append(rv.PARAMETERS$rate.eqn.vars, new.parameter)
        StoreParamsRate(new.parameter)
      }
      
      #remove parameter and value and comment from paramter vectors 
      param.to.remove = input$eqnCreate_rate_firstvar
      rv.PARAMETERS$rate.params <- append(rv.PARAMETERS$rate.params, param.to.remove)
      #search named vector for this parameter and remove
      if (param.to.remove %in% rv.PARAMETERS$eqns.vars) {
        # idx.of.param = which(rv.PARAMETERS$eqns.vars == param.to.remove)
        # rv.PARAMETERS$eqns.vars = rv.PARAMETERS$eqns.vars[-idx.of.param]
        # rv.PARAMETERS$eqns.vals = rv.PARAMETERS$eqns.vals[-idx.of.param]
        # rv.PARAMETERS$eqns.comments = rv.PARAMETERS$eqns.comments[-idx.of.param]
        # if (length(rv.PARAMETERS$eqns.vars) == 0) {
        #    rv.PARAMETERS$first.param.eqn.stored = FALSE
        # }
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

  updateNumericInput(session, 
                     "eqnCreate_num_of_eqn_LHS", 
                     value = 1)
  updateNumericInput(session, 
                     "eqnCreate_num_of_eqn_RHS", 
                     value = 1)
  # Build visible reactions list and labeled choices
  visible <- lapply(rv.REACTIONS$reactions, function(r) { if (is.null(r$Show.In.Table) || isTRUE(r$Show.In.Table)) return(r) else return(NULL) })
  visible <- visible[!vapply(visible, is.null, FUN.VALUE = logical(1))]
  if (length(visible) == 0) {
    edit_choices <- character(0)
  } else {
    labels <- vapply(seq_along(visible), function(i) paste0("(", i, ") ", visible[[i]]$Equation.Text), FUN.VALUE = "")
    edit_choices <- setNames(as.character(seq_len(length(visible))), labels)
  
    message("DEBUG: labels: ", paste(labels, collapse = " | "))
    # message("DEBUG: vals: ", paste(unlist(vals), collapse = " | "))
    message("DEBUG: edit_choices names: ", paste(names(edit_choices), collapse = " | "))
    
  }
  message("DEBUG: updating eqnCreate_edit_select_equation with labels:", paste(names(edit_choices), collapse = " | "))
  updatePickerInput(session,
                    'eqnCreate_edit_select_equation',
                    choices = edit_choices)
  message("DEBUG: updating eqnCreate_delete_select_equation with labels:", paste(names(edit_choices), collapse = " | "))
  updatePickerInput(session,
                    'eqnCreate_delete_select_equation',
                    choices = edit_choices)
  updatePickerInput(session,
                    'eqnCreate_edit_select_equation_custom',
                    choices = seq(length(rv.REACTIONS$additional.eqns)))
  updateCheckboxInput(session,
                      "eqn_options_chem_modifier_forward",
                      value = FALSE)
  updateNumericInput(session, 
                     "eqn_options_chem_num_forward_regulators", 
                     value = 1)
  updateCheckboxInput(session,
                      "eqn_options_chem_modifier_reverse",
                      value = FALSE)
  updateNumericInput(session, 
                     "eqn_options_chem_num_reverse_regulators", 
                     value = 1)

})

# Equation Text outputs --------------------------------------------------------

output$eqnCreate_showEquationBuilding <- renderUI({
  withMathJax(
    equationMathJaxBuilder()
  )
})


output$test_mathjax_equations <- renderUI({
  if (length(rv.REACTIONS$main) == 0) {
    paste("No equations entered")
  } else {
    n_eqns = seq(length(rv.REACTIONS$main))
    eqns_to_display <- c()
    for (i in n_eqns) {
      new_eqn <- paste0("(",i, ") ", rv.REACTIONS$main[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br>")
  }
})


output$eqnCreate_showAdditionalEquations <- renderText({
  if (length(rv.REACTIONS$additional.eqns) == 0) {
    "No additional equations entered"
  } else{
    eqns_to_display <- c()
    n_eqns = seq(length(rv.REACTIONS$additional.eqns))

    for (i in n_eqns) {
      new_eqn <- paste0("(",n_eqns[i], ") ", rv.REACTIONS$additional.eqns[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br>")
  }
})

# Delete Equations -------------------------------------------------------------
output$deleteEquations_table_viewer <- renderRHandsontable({
  
  eqn.num <- as.numeric(input$eqnCreate_delete_select_equation)
  myindex = eqn.num - 1
  
  df.to.show <- select(rv.REACTIONS$reactions.df,
                       "Equation.Text",
                       "Eqn.Display.Type",
                       "Compartment")
  
  df.to.show <- as.data.frame(df.to.show)
  colnames(df.to.show) <- c("Equation", 
                            "Type",
                            "Compartment")
  rhandsontable(df.to.show,
                myindex = myindex) %>%
    hot_cols(renderer = 
     "function(instance, td, row, col, prop, value, cellProperties) {
       Handsontable.renderers.TextRenderer.apply(this, arguments);
       if (instance.params) {
       mhrows = instance.params.myindex;
       mhrows = mhrows instanceof Array ? mhrows : [mhrows];
       }
       if (instance.params && mhrows.includes(row)) td.style.background = '#FFCCCB';
      }"
    )
})

observeEvent(input$modal_delete_eqn_button, {
  # browser()
  eqns.to.delete <- as.numeric(input$eqnCreate_delete_select_equation)
  eqn.ids <- rv.REACTIONS$reactions.df$ID[eqns.to.delete]
  
  # Extract parameter ids used in removed equations
  parameter.ids <- rv.REACTIONS$reactions.df$Parameters.id[eqns.to.delete]
  # browser()
  # Delete associated species
  for (eqn.id in eqn.ids) {
    # Grab associated speces
    spec.ids <- SplitEntry(rv.REACTIONS$reactions[[eqn.id]]$Species.id)
    # Loop through species and remove id from them
    for (spec.id in spec.ids) {
      entry <- rv.SPECIES$species[[spec.id]]
      eqns <- SplitEntry(entry$Reaction.ids)
      eqns <- eqns[!(eqns %in% eqn.id)]
      if (length(eqns) > 0) {
        rv.SPECIES$species[[spec.id]]$Reaction.ids <- collapseVector(eqns) 
      } else {
        rv.SPECIES$species[[spec.id]]$Reaction.ids <- NA
      }
      
    }
  }
  
  
  # Delete Equations from Reactive Variables
  for (i in eqn.ids) {
    rv.REACTIONS$reactions[[i]] <- NULL
  }
  
  # Reform eqn df
  # rv.REACTIONS$reactions.df <- bind_rows(rv.REACTIONS$reactions)
  rv.REACTIONS$reactions.df <- as_tibble(
    do.call(rbind, rv.REACTIONS$reactions))
  
  # Remove Parameters from model if they are not located elsewhere
  pars.to.check <- c()
  for (par.ids in parameter.ids) {
    pars.to.check <- c(pars.to.check, strsplit(par.ids, " ")[[1]])
  }

  # Gather params from equations
  pars.in.eqns <- c()
  par.extraction <- rv.REACTIONS$reactions.df$Parameters.id
  for (par.ids in par.extraction) {
    pars.in.eqns <- c(pars.in.eqns, strsplit(par.ids, " ")[[1]])
  }

  # Gather params from Input/Outputs
  pars.in.IO <- c()
  par.extraction <- rv.IO$IO.df$parameter.id
  for (par.ids in par.extraction) {
    pars.in.IO <- c(pars.in.IO, strsplit(par.ids, " ")[[1]])
  }

  # Join par vectors
  pars.in.model <- c(pars.in.eqns, pars.in.IO)

  # Check IO for parameters and other equations
  pars.to.remove <- c()
  for (i in pars.to.check) {
    # Check other equations
    if (!(i %in% pars.in.model)) {
      pars.to.remove <- c(pars.to.remove, i)
    }
  }

  # Remove Parameters
  for (p in pars.to.remove) {
   rv.PARAMETERS$parameters[[p]] <- NULL 
  }
  
  solveForDiffEqs()
  
  if (input$checkbox_modal_delete_keep_modal_active) {
    toggleModal(session,
                "modal_delete_equations",
                toggle = "close")
  }
})


# Equation Event Updates -------------------------------------------------------

observeEvent(rv.REACTIONS$reactions, {
    # rv.REACTIONS$reactions.df <- bind_rows(rv.REACTIONS$reactions)
  rv.REACTIONS$reactions.df <- as_tibble(
    do.call(rbind, rv.REACTIONS$reactions))
  
  # Update Number Counters on Equation Modals (use labeled visible reactions)
  visible <- lapply(rv.REACTIONS$reactions, function(r) { if (is.null(r$Show.In.Table) || isTRUE(r$Show.In.Table)) return(r) else return(NULL) })
  visible <- visible[!vapply(visible, is.null, FUN.VALUE = logical(1))]
  if (length(visible) == 0) {
    edit_choices <- character(0)
  } else {
    labels <- vapply(seq_along(visible), function(i) paste0("(", i, ") ", visible[[i]]$Equation.Text), FUN.VALUE = "")
    edit_choices <- setNames(as.character(seq_len(length(visible))), labels)
  }
  updatePickerInput(session,
                    'eqnCreate_edit_select_equation',
                    choices = edit_choices)

  updatePickerInput(session,
                    'eqnCreate_delete_select_equation',
                    choices = edit_choices)
})

observeEvent(rv.REACTIONS$massAction, {
  rv.REACTIONS$massAction.df <- bind_rows(rv.REACTIONS$massAction)
})

observeEvent(rv.REACTIONS$michaelisMenten, {
  rv.REACTIONS$michaelisMenten.df <- bind_rows(rv.REACTIONS$michaelisMenten)
})

observeEvent(rv.REACTIONS$synthesis, {
  rv.REACTIONS$synthesis.df <- bind_rows(rv.REACTIONS$synthesis)
})

observeEvent(rv.REACTIONS$degradation, {
  rv.REACTIONS$degradation.df <- bind_rows(rv.REACTIONS$degradation)
})

#--------------------------Random----------------------------------------------

observeEvent(input$eqnCreate_type_of_equation, {
  filter.choice <- input$eqnCreate_type_of_equation
  # Determine the filtering of the law choices
  if (filter.choice == "All") {
    option.names <- rv.REACTIONLAWS$laws %>% pull(Name)
    options      <- rv.REACTIONLAWS$laws %>% pull(BackendName)
  } else if (filter.choice == "chemical_reaction") {
    
    option.names <- rv.REACTIONLAWS$laws %>% 
                    filter(Type == "chemical") %>%
                    pull(Name)
    
    options      <- rv.REACTIONLAWS$laws %>% 
                    filter(Type == "chemical") %>%
                    pull(BackendName)
    
  } else if (filter.choice == "enzyme_reaction") {
    
    option.names <- rv.REACTIONLAWS$laws %>% 
                    filter(Type == "enzyme") %>%
                    pull(Name)
    
    options      <- rv.REACTIONLAWS$laws %>% 
                    filter(Type == "enzyme") %>%
                    pull(BackendName)
    
  } else if (filter.choice == "bacterial_reaction") {
    
    option.names <- rv.REACTIONLAWS$laws %>% 
                    filter(Type == "bacterial") %>%
                    pull(Name)
    
    options      <- rv.REACTIONLAWS$laws %>% 
                    filter(Type == "bacterial") %>%
                    pull(BackendName)
    
  } else if (filter.choice == "custom_reaction") {
    option.names <- rv.REACTIONLAWS$laws %>% 
                    filter(Type == "custom") %>%
                    pull(Name)
    options      <- rv.REACTIONLAWS$laws %>%
                    filter(Type == "custom") %>%
                    pull(BackendName)
  }
  
  names(options) <- option.names
  
  updatePickerInput(
    session = session, 
    inputId = "eqnCreate_reaction_law",
    choices = options
  )
})


# laws <- data.frame(
#   Name = c("mass_action",
#            "mass_action_w_reg",
#            "synthesis",
#            "degradation_rate",
#            "degradation_by_enzyme",
#            
#            "michaelis_menten"),
#   Type = c("chemical",
#            "chemical",
#            "chemical",
#            "chemical",
#            "chemical",
#            
#            "enzyme")
# )
