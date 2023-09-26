
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
                                    kf.unit$base.unit,
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
                                      kr.unit$base.unit,
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
        print(u)
        if (u$unit != u$unit.base) {
          base.val <- UnitConversion(u$unit.d,
                                     u$unit,
                                     u$base.unit,
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
                                      kf.unit$base.unit,
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
                                       u$base.unit,
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
                                        kr.unit$base.unit,
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
    
    # browser()
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
    
    if (isTruthy(species.id)) {
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
    
    # Add overall reaction information
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
      sub.entry <- list(
        "ID"               = ID.to.add,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "VarDeg"           = deg.species,
        "VarDeg.id"        = deg.species.id,
        "ConcDep"          = ConcDep,
        "Rate.Constant"    = parameter,
        "Rate.Constant.id" = par.ids[1],
        "Products"         = products.collapsed,
        "Products.id"      = products.id.collapsed
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
      Km.id   <- par.ids[1]
      
      if (Use.Vmax) {
        Vmax.id <- par.ids[2]
      } else {
        kcat.id <- par.ids[2]
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
        "Products.id"      = products.id.collapsed
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
  df <- bind_rows(rv.REACTIONS$reactions)
  print(df)
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
    # print(rv.REACTIONS$reactions.df)
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
                Shiny.setInputValue('edit_equation_menu_item', options);
              }"
      )
    )
    
    eqnAdd = list(
      name = "Add",
      callback = htmlwidgets::JS(
        "function(key, options) {
                Shiny.setInputValue('add_equation_menu_item', options);
              }"
      )
    )
    
    eqnDel = list(
      name = "Delete",
      callback = htmlwidgets::JS(
        "function(key, options) {
                Shiny.setInputValue('delete_equation_menu_item', options);
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
  updatePickerInput(session,
                    'eqnCreate_edit_select_equation',
                    choices = seq(length(rv.REACTIONS$reactions)))
  updatePickerInput(session,
                    'eqnCreate_delete_select_equation',
                    choices = seq(length(rv.REACTIONS$reactions)))
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
  
  print(rv.REACTIONS$reactions.df)
  #Update Number Counters on Equation Modals
  updatePickerInput(session,
                    'eqnCreate_edit_select_equation',
                    choices = seq(length(rv.REACTIONS$reactions)))
  
  updatePickerInput(session,
                    'eqnCreate_delete_select_equation',
                    choices = seq(length(rv.REACTIONS$reactions)))
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
