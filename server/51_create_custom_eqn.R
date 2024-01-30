

# Change color of Rate Law TextInput if valid/invalid
observeEvent(input$TI_custom_eqn_RHS, {
  a <- parse_string_expression(input$TI_custom_eqn_RHS)
  is.valid <- is_valid_expression(input$TI_custom_eqn_RHS, a$valid.terms)
  
  if (input$TI_custom_eqn_RHS == "") {
    js$backgroundCol("TI_custom_eqn_RHS", "#FFFFFF")
  } else if (is.valid) {
    js$backgroundCol("TI_custom_eqn_RHS", "#90ee90")
  } else {
    js$backgroundCol("TI_custom_eqn_RHS", "#ffcccb")
  }
})

# Change color of Rate Law TextInput if valid/invalid
observeEvent(input$TI_custom_eqn_LHS, {
  
  LHS.var <- input$TI_custom_eqn_LHS
  
  # Vars to check with 
  species.names <- rv.SPECIES$species.names
  param.names   <- rv.PARAMETERS$parameters.names
  
  # Check is LHS.var is valid
  is.valid <- variableCheck(LHS.var, 
                            species.names, 
                            param.names, 
                            TRUE, 
                            TRUE)[[1]]
  
  if (input$TI_custom_eqn_LHS == "") {
    js$backgroundCol("TI_custom_eqn_LHS", "#FFFFFF")
  } else if (is.valid) {
    js$backgroundCol("TI_custom_eqn_LHS", "#90ee90")
  } else {
    js$backgroundCol("TI_custom_eqn_LHS", "#ffcccb")
  }
})

# Store custom equation
observeEvent(input$bttn_custom_eqn_enter, {

  # Extract information
  LHS.var <- RemoveWS(input$TI_custom_eqn_LHS)
  RHS.exp <- RemoveWS(input$TI_custom_eqn_RHS)
  
  # Error Check to make sure everything is valid _______________________________
  LHS.var <- input$TI_custom_eqn_LHS
  
  # Vars to check with 
  species.names <- rv.SPECIES$species.names
  param.names   <- rv.PARAMETERS$parameters.names
  
  # Check is LHS.var is valid
  LHS.valid <- variableCheck(LHS.var, 
                             species.names, 
                             param.names, 
                             TRUE,
                             TRUE)[[1]]
  
  a <- parse_string_expression(input$TI_custom_eqn_RHS)
  RHS.valid <- is_valid_expression(input$TI_custom_eqn_RHS, a$valid.terms)
  
  if (LHS.valid && RHS.valid) {
    # Paste together expression
    eqn.out <- paste0(LHS.var, " = ", input$TI_custom_eqn_RHS)
    
    # Generate custom reaction ID
    ids <- GenerateId(rv.ID$id.custeqnaddional.seed, "custEqnAdditional")
    unique.id <- ids[[2]]
    rv.ID$id.custeqnaddional.seed <- ids[[1]]
    idx.to.add <- nrow(rv.ID$id.df) + 1
    rv.ID$id.df[idx.to.add, ] <- c(unique.id, paste0(LHS.var, "=", RHS.exp))
    eqn.id <- unique.id
    
# Extract existing variables info ______________________________________________
    existing.vars <- hot_to_r(input$RHT_custom_eqn_params_existing)
    existing.species <- existing.vars %>%
      filter(Type == "Species") %>%
      pull(Variables)
    
    if (isTruthy(existing.species)) {
      # Find Ids
      exist.spec.ids <- sapply(existing.species, FindId)
    } else {
      existing.species <- NA
      exist.spec.ids <- NA
    }
    
    existing.params <- existing.vars %>%
      filter(Type == "Parameter") %>%
      pull(Variables)
    
    if (isTruthy(existing.params)) {
      # Find Ids
      exist.param.ids <- sapply(existing.params, FindId)
      
      # Check is parameter is LHS variable and if so change it to custom
     for (i in seq_along(existing.params)) {
       if (existing.params[i] == LHS.var) {
         # change custom val to true
         rv.PARAMETERS$parameters[[exist.param.ids[i]]]$Custom <- TRUE
       }
     }
    } else {
      existing.params <- NA
      exist.param.ids <- NA
    }
    
    # Note if time variable exists
    existing.time <- existing.vars %>%
      filter(Type == "Time") %>%
      pull(Variables)
    
    if (isTruthy(existing.time)) {
      # Find Ids
      time.var.exists <- TRUE
    } else {
      time.var.exists <- FALSE
    }
    
# Extract new variables info ___________________________________________________
    new.vars      <- hot_to_r(input$RHT_custom_eqn_params_new)
    new.species <- new.vars %>%
      filter(Type == "Species") %>%
      pull(Variables)
    
    if (isTruthy(new.species)) {
      # Create new ids
      new.spec.ids <- c()
      for(specie in new.species) {
        # Generate new species id
        ids <- GenerateId(rv.ID$id.var.seed, "var")
        unique.id <- ids[[2]]
        rv.ID$id.var.seed <- ids[[1]]
        idx.to.add <- nrow(rv.ID$id.df) + 1
        rv.ID$id.df[idx.to.add, ] <- c(unique.id, specie)
        new.spec.ids <- c(new.spec.ids, unique.id)
        
        # Add species to species table
        # Create List Entry
        to.add <- list(Name = specie,
                       ID = unique.id,
                       Value = 0,
                       Unit = rv.UNITS$units.selected$For.Var,
                       UnitDescription = paste0("conc (",
                                                rv.UNITS$units.selected$For.Var, 
                                                ")"),
                       BaseUnit = rv.UNITS$units.selected$For.Var,
                       BaseValue = 0,
                       Description = "",
                       Compartment = NA,
                       Compartment.id = NA,
                       BoundaryCondition = TRUE,
                       Reaction.ids = NA,
                       IO.ids = NA
        )
        
        # Add Entry To RV
        rv.SPECIES$species[[unique.id]] <- to.add
        # names(rv.SPECIES$species)[length(rv.SPECIES$species) + 1] <- unique.id
      }
    } else {
      new.species  <- NA
      new.spec.ids <- NA
    }
    
    new.params <- new.vars %>%
      filter(Type == "Parameter") %>%
      pull(Variables)
    
    if (isTruthy(new.params)) {
      new.param.ids <- c()
      for (param in new.params) {
        # Create new parameter id
        par.gen <- GenerateId(rv.ID$id.param.seed, "parameter")
        rv.ID$id.param.seed <- par.gen$seed
        par.id <- par.gen$id
        new.param.ids <- c(new.param.ids, par.id)
        
        # Store ID to database
        idx.to.add <- nrow(rv.ID$id.df) + 1
        rv.ID$id.df[idx.to.add, ] <- c(par.id, param)
        
        # Check to see if the added parameter is being added as custom
        if (param == LHS.var) {is.custom <- TRUE} else {is.custom <- FALSE}
        
        # Add Parameter
        to.par.list <- list("Name"            = param,
                            "ID"              = par.id,
                            "Value"           = 0,
                            "Unit"            = NA,
                            "UnitDescription" = NA,
                            "BaseUnit"        = NA,
                            "BaseValue"       = 0,
                            "Description"     = "Custom Equation Param",
                            "Type"            = "CustomEqn",
                            "Type.Note"       = NA,
                            "Used.In"         = NA,
                            "Custom"          = is.custom
        )
        # Append parameter entry
        rv.PARAMETERS$parameters[[par.id]] <- to.par.list
      }
    } else {
      new.params <- NA
      new.param.ids <- NA
    }
    
    # Store to proper RV
    to.ce.list <- list("ID" = eqn.id,
                       "Equation" = eqn.out,
                       "New.Species" = collapseVector(new.species),
                       "New.Species.id" = collapseVector(new.spec.ids),
                       "New.Parameters" = collapseVector(new.params),
                       "New.Parameters.id" = collapseVector(new.param.ids),
                       "Old.Species" = collapseVector(existing.species),
                       "Old.Species.id" = collapseVector(exist.spec.ids),
                       "Old.Parameters" = collapseVector(existing.params),
                       "Old.Parameters.id" = collapseVector(exist.param.ids),
                       "Has.Time.Var" = time.var.exists)
    
    rv.CUSTOM.EQNS$ce.equations[[eqn.id]] <- to.ce.list
    
    # Clear Text inputs for LHS and RHS expressions
    updateTextInput(
      session = session,
      inputId = "TI_custom_eqn_LHS",
      value = ""
    )
    
    updateTextInput(
      session = session,
      inputId = "TI_custom_eqn_RHS",
      value = ""
    )
    
  } else {
    message <- "Equation is not valid"
    
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = message,
      type = "error"
    )
  }
  

})

# Render Parameter Table
output$RHT_custom_eqn_params_existing <- renderRHandsontable({
  
  # Grab Expression Information
  LHS.var <- RemoveWS(input$TI_custom_eqn_LHS)
  RHS.exp <- RemoveWS(input$TI_custom_eqn_RHS)
  
  # Determine which variables are new or which are already species/parameters
  
  # Vars to check with 
  species.names <- rv.SPECIES$species.names
  param.names   <- rv.PARAMETERS$parameters.names
  
  # Check is LHS.var is valid
  LHS.valid <- variableCheck(LHS.var, 
                             species.names, 
                             param.names, 
                             TRUE,
                             TRUE)[[1]]
  
  # Extract all variables from expression and find valid terms
  a <- parse_string_expression(RHS.exp)
  valid <- a$valid.terms
  
  if (LHS.valid) {
    valid <- c(valid, LHS.var)
  }
  
  # See if valid term in species/param or brand new
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  for (term in valid) {
    exists.already <- FALSE
    type   <- "Parameter"
    
    if (term == "t" || term == "time") {
      exists.already <- TRUE
      type <- "Time"
    }
    if (term %in% species.names) {
      exists.already <- TRUE
      type <- "Species"
    } else if (term %in% param.names) {
      exists.already <- TRUE
      type <- "Parameter"
    }
    
    if (exists.already) {
      row.to.add <- c(term, type)
      df[nrow(df) + 1, ] <- row.to.add
    }
    
  }
  colnames(df) <- c("Variables", "Type")
  # Build Table
  if (isTruthy(valid)) {
    hot <- rhandsontable(df,
                         stretchH = "all",
                         overflow = "visible") %>%
      hot_col(col = "Variables", readOnly = TRUE)
  } else {
    temp <- data.frame(c("Variables will be extracted from above expression 
                         above"))
    temp <- transpose(temp)
    colnames(temp) <- c("Equation Variables")
    hot <- rhandsontable(temp,
                         overflow = "visible",
                         stretchH = "all",
                         readOnly = TRUE,
                         rowHeaders = NULL,
                         height = 200
    ) %>%
      hot_cols(manualColumnMove = FALSE,
               manualColumnResize = FALSE,
               halign = "htCenter",
               valign = "htMiddle")
  }
  
  hot
  
})

# Render Parameter Table
output$RHT_custom_eqn_params_new <- renderRHandsontable({
  
  # Grab Expression Information
  LHS.var <- RemoveWS(input$TI_custom_eqn_LHS)
  RHS.exp <- RemoveWS(input$TI_custom_eqn_RHS)
  
  # Vars to check with 
  species.names <- rv.SPECIES$species.names
  param.names   <- rv.PARAMETERS$parameters.names
  
  # Check is LHS.var is valid
  LHS.valid <- variableCheck(LHS.var, 
                             species.names, 
                             param.names, 
                             TRUE,
                             TRUE)[[1]]
  
  # Extract all variables from expression and find valid terms
  a <- parse_string_expression(RHS.exp)
  valid <- a$valid.terms
  
  # Merge valid terms
  if (LHS.valid) {
    valid <- c(valid, LHS.var)
  }

  # See if valid term in species/param or brand new
  df <- data.frame(matrix(ncol = 2, nrow = 0))
  for (term in valid) {
    exists.already <- FALSE
    type   <- "Parameter"
    
    if (term == "t" || term == "time") {
      exists.already <- TRUE
      type <- "Time"
    }
    if (term %in% species.names) {
      exists.already <- TRUE
      type <- "Species"
    } else if (term %in% param.names) {
      exists.already <- TRUE
      type <- "Parameter"
    }
    
    if (!exists.already) {
      row.to.add <- c(term, type)
      df[nrow(df) + 1, ] <- row.to.add
    }
    
  }
  colnames(df) <- c("Variables", "Type")
  # Build Table
  if (isTruthy(valid)) {
    
    type.options <- c("Parameter", "Species")
    
    hot <- rhandsontable(df,
                         stretchH = "all",
                         overflow = "visible") %>%
      hot_col(col = "Variables", readOnly = TRUE) %>%
      hot_col(col = "Type", type = "dropdown", source = type.options)
  } else {
    temp <- data.frame(c("Variables will be extracted from above expression 
                         above"))
    temp <- transpose(temp)
    colnames(temp) <- c("Equation Variables")
    hot <- rhandsontable(temp,
                         overflow = "visible",
                         stretchH = "all",
                         readOnly = TRUE,
                         rowHeaders = NULL,
                         height = 200
    ) %>%
      hot_cols(manualColumnMove = FALSE,
               manualColumnResize = FALSE,
               halign = "htCenter",
               valign = "htMiddle")
  }
  
  hot
  
})

# Render Table to show current additional equans
output$RHT_custom_eqn_display_existing <- renderRHandsontable({
  
  if (length(rv.CUSTOM.EQNS$ce.equations) != 0) {
    additional.equations <- unname(sapply(rv.CUSTOM.EQNS$ce.equations,
                                          get,
                                          x = "Equation"))
    
    df <- data.frame(additional.equations)
    colnames(df) <- "Equations"
    hot <- 
      rhandsontable(df,
                    stretchH = "all",
                    overflow = "visible") %>% 
      hot_col(col = "Equations", readOnly = TRUE)
  } else {
    temp <- data.frame(c("Added equations will be shown here"))
    temp <- transpose(temp)
    colnames(temp) <- c("Equations")
    hot <- rhandsontable(temp,
                         overflow = "visible",
                         stretchH = "all",
                         readOnly = TRUE,
                         rowHeaders = NULL,
                         height = 200
    ) %>%
      hot_cols(manualColumnMove = FALSE,
               manualColumnResize = FALSE,
               halign = "htCenter",
               valign = "htMiddle")
  }
  
  
  hot
})

# Build Mathjax Expression
custom_law_expression <- reactive ({
  # Grab Expression Information
  LHS.var <- input$TI_custom_eqn_LHS
  RHS.exp <- input$TI_custom_eqn_RHS
  
  textOut <- paste0("$$", LHS.var, " = ", RHS.exp, "$$")
  
  return(textOut)
})

# Render MathJax Expression
output$mathjax_custom_eqn_view <- renderUI({
  tryCatch({
    withMathJax(custom_law_expression())
  }, warning = function(w) {
    # showNotification('there was a warning','',type = "error")
    # return()
  }, error = function(e) {
    # showNotification('there was an error','',type = "error")
    # return()
  }, silent=TRUE)
  
}) 