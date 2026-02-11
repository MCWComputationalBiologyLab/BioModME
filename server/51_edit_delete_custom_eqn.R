# Edit and Delete Custom Equations
# This file handles editing and deleting custom equations

# Update picker choices when custom equations change
observeEvent(rv.CUSTOM.EQNS$ce.equations, {
  if (length(rv.CUSTOM.EQNS$ce.equations) > 0) {
    eqn.names <- sapply(rv.CUSTOM.EQNS$ce.equations, function(x) {
      # Extract LHS from equation string (format: "LHS = RHS")
      eqn <- x$Equation
      if (grepl("=", eqn)) {
        lhs <- trimws(strsplit(eqn, "=")[[1]][1])
        return(lhs)
      }
      return(eqn)
    })
    
    updatePickerInput(
      session = session,
      inputId = "PI_custom_eqn_edit_select",
      choices = eqn.names
    )
    
    updatePickerInput(
      session = session,
      inputId = "PI_custom_eqn_delete_select",
      choices = eqn.names
    )
  } else {
    updatePickerInput(
      session = session,
      inputId = "PI_custom_eqn_edit_select",
      choices = ""
    )
    
    updatePickerInput(
      session = session,
      inputId = "PI_custom_eqn_delete_select",
      choices = ""
    )
  }
})

# Load custom equation data into edit form
observeEvent(input$PI_custom_eqn_edit_select, {
  if (isTruthy(input$PI_custom_eqn_edit_select) && input$PI_custom_eqn_edit_select != "") {
    # Find the equation by LHS name
    eqn.id <- NULL
    for (id in names(rv.CUSTOM.EQNS$ce.equations)) {
      eqn <- rv.CUSTOM.EQNS$ce.equations[[id]]$Equation
      if (grepl("=", eqn)) {
        lhs <- trimws(strsplit(eqn, "=")[[1]][1])
        if (lhs == input$PI_custom_eqn_edit_select) {
          eqn.id <- id
          break
        }
      }
    }
    
    if (!is.null(eqn.id)) {
      eqn.entry <- rv.CUSTOM.EQNS$ce.equations[[eqn.id]]
      
      # Parse equation to get LHS and RHS
      eqn <- eqn.entry$Equation
      if (grepl("=", eqn)) {
        parts <- strsplit(eqn, "=")[[1]]
        lhs <- trimws(parts[1])
        rhs <- trimws(paste(parts[-1], collapse = "="))
        
        updateTextInput(session, "TI_custom_eqn_LHS_edit", value = lhs)
        updateTextInput(session, "TI_custom_eqn_RHS_edit", value = rhs)
      }
    }
  }
})

# Render parameter tables for edit
output$RHT_custom_eqn_params_existing_edit <- renderRHandsontable({
  LHS.var <- RemoveWS(input$TI_custom_eqn_LHS_edit)
  RHS.exp <- RemoveWS(input$TI_custom_eqn_RHS_edit)
  
  species.names <- rv.SPECIES$species.names
  param.names   <- rv.PARAMETERS$parameters.names
  
  LHS.valid <- variableCheck(LHS.var, 
                             species.names, 
                             param.names, 
                             TRUE,
                             TRUE)[[1]]
  
  a <- parse_string_expression(RHS.exp)
  valid <- a$valid.terms
  
  if (LHS.valid) {
    valid <- c(valid, LHS.var)
  }
  
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
  
  if (isTruthy(valid)) {
    hot <- rhandsontable(df,
                         stretchH = "all",
                         overflow = "visible",
                         height = 150) %>%
      hot_col(col = "Variables", readOnly = TRUE)
  } else {
    temp <- data.frame(c("Variables will be extracted from above expression"))
    temp <- transpose(temp)
    colnames(temp) <- c("Equation Variables")
    hot <- rhandsontable(temp,
                         overflow = "visible",
                         stretchH = "all",
                         readOnly = TRUE,
                         rowHeaders = NULL,
                         height = 100
    ) %>%
      hot_cols(manualColumnMove = FALSE,
               manualColumnResize = FALSE,
               halign = "htCenter",
               valign = "htMiddle")
  }
  
  hot
})

output$RHT_custom_eqn_params_new_edit <- renderRHandsontable({
  LHS.var <- RemoveWS(input$TI_custom_eqn_LHS_edit)
  RHS.exp <- RemoveWS(input$TI_custom_eqn_RHS_edit)
  
  species.names <- rv.SPECIES$species.names
  param.names   <- rv.PARAMETERS$parameters.names
  
  LHS.valid <- variableCheck(LHS.var, 
                             species.names, 
                             param.names, 
                             TRUE,
                             TRUE)[[1]]
  
  a <- parse_string_expression(RHS.exp)
  valid <- a$valid.terms
  
  if (LHS.valid) {
    valid <- c(valid, LHS.var)
  }

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
  
  if (isTruthy(valid)) {
    type.options <- c("Parameter", "Species")
    
    hot <- rhandsontable(df,
                         stretchH = "all",
                         overflow = "visible",
                         height = 150) %>%
      hot_col(col = "Variables", readOnly = TRUE) %>%
      hot_col(col = "Type", type = "dropdown", source = type.options)
  } else {
    temp <- data.frame(c("Variables will be extracted from above expression"))
    temp <- transpose(temp)
    colnames(temp) <- c("Equation Variables")
    hot <- rhandsontable(temp,
                         overflow = "visible",
                         stretchH = "all",
                         readOnly = TRUE,
                         rowHeaders = NULL,
                         height = 100
    ) %>%
      hot_cols(manualColumnMove = FALSE,
               manualColumnResize = FALSE,
               halign = "htCenter",
               valign = "htMiddle")
  }
  
  hot
})

# Helper function to convert underscores in expressions to MathJax subscripts
ConvertExpressionToMathJax <- function(expression) {
  # Convert variable names with underscores to MathJax format
  # e.g., "sigma_ABE" -> "sigma_{ABE}"
  # This processes the entire expression, converting all variable names
  
  if (is.null(expression) || expression == "") {
    return(expression)
  }
  
  # Pattern matches: word characters starting with letter, underscore, then word characters
  # This will match things like sigma_ABE, con_ABE, etc.
  # We want to convert these to sigma_{ABE}, con_{ABE}
  # The pattern ensures we only match variable-like patterns (not operators)
  
  # Use regex to find and replace all occurrences
  # Pattern: ([a-zA-Z][a-zA-Z0-9]*)_([a-zA-Z0-9]+)
  # - First part: word starting with letter, followed by letters/numbers
  # - Underscore
  # - Second part: letters/numbers (the subscript)
  # Replacement: \1_{\2} converts to identifier_{identifier}
  
  # Process the expression - this will convert all variable_name patterns
  result <- gsub("([a-zA-Z][a-zA-Z0-9]*)_([a-zA-Z0-9]+)", "\\1_{\\2}", expression, perl = TRUE)
  
  return(result)
}

# MathJax preview for edit
custom_law_expression_edit <- reactive({
  LHS.var <- ConvertExpressionToMathJax(input$TI_custom_eqn_LHS_edit)
  RHS.exp <- ConvertExpressionToMathJax(input$TI_custom_eqn_RHS_edit)
  
  textOut <- paste0("$$", LHS.var, " = ", RHS.exp, "$$")
  
  return(textOut)
})

output$mathjax_custom_eqn_view_edit <- renderUI({
  tryCatch({
    withMathJax(custom_law_expression_edit())
  }, warning = function(w) {
  }, error = function(e) {
  }, silent=TRUE)
})

# Update custom equation
observeEvent(input$bttn_custom_eqn_update, {
  if (!isTruthy(input$PI_custom_eqn_edit_select) || input$PI_custom_eqn_edit_select == "") {
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "Please select an equation to edit.",
      type = "error"
    )
    return()
  }
  
  # Find the equation ID
  eqn.id <- NULL
  for (id in names(rv.CUSTOM.EQNS$ce.equations)) {
    eqn <- rv.CUSTOM.EQNS$ce.equations[[id]]$Equation
    if (grepl("=", eqn)) {
      lhs <- trimws(strsplit(eqn, "=")[[1]][1])
      if (lhs == input$PI_custom_eqn_edit_select) {
        eqn.id <- id
        break
      }
    }
  }
  
  if (is.null(eqn.id)) {
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "Could not find equation to edit.",
      type = "error"
    )
    return()
  }
  
  # Get old equation entry
  old.entry <- rv.CUSTOM.EQNS$ce.equations[[eqn.id]]
  
  # Extract new information
  LHS.var <- RemoveWS(input$TI_custom_eqn_LHS_edit)
  RHS.exp <- RemoveWS(input$TI_custom_eqn_RHS_edit)
  
  # Validation
  species.names <- rv.SPECIES$species.names
  param.names   <- rv.PARAMETERS$parameters.names
  
  LHS.valid <- variableCheck(LHS.var, 
                             species.names, 
                             param.names, 
                             TRUE,
                             TRUE)[[1]]
  
  a <- parse_string_expression(RHS.exp)
  RHS.valid <- is_valid_expression(RHS.exp, a$valid.terms)
  
  if (!LHS.valid || !RHS.valid) {
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "Equation is not valid. Please check your inputs.",
      type = "error"
    )
    return()
  }
  
  # Build new equation
  eqn.out <- paste0(LHS.var, " = ", RHS.exp)
  
  # Extract existing variables info
  existing.vars <- hot_to_r(input$RHT_custom_eqn_params_existing_edit)
  existing.species <- existing.vars %>%
    filter(Type == "Species") %>%
    pull(Variables)
  
  if (isTruthy(existing.species)) {
    exist.spec.ids <- sapply(existing.species, FindId)
  } else {
    existing.species <- NA
    exist.spec.ids <- NA
  }
  
  existing.params <- existing.vars %>%
    filter(Type == "Parameter") %>%
    pull(Variables)
  
  if (isTruthy(existing.params)) {
    exist.param.ids <- sapply(existing.params, FindId)
    
    # Check if parameter is LHS variable and if so change it to custom
    for (i in seq_along(existing.params)) {
      if (existing.params[i] == LHS.var) {
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
  
  time.var.exists <- isTruthy(existing.time)
  
  # Extract new variables info
  new.vars <- hot_to_r(input$RHT_custom_eqn_params_new_edit)
  new.species <- new.vars %>%
    filter(Type == "Species") %>%
    pull(Variables)
  
  if (isTruthy(new.species)) {
    new.spec.ids <- c()
    for(specie in new.species) {
      ids <- GenerateId(rv.ID$id.var.seed, "var")
      unique.id <- ids[[2]]
      rv.ID$id.var.seed <- ids[[1]]
      idx.to.add <- nrow(rv.ID$id.df) + 1
      rv.ID$id.df[idx.to.add, ] <- c(unique.id, specie)
      new.spec.ids <- c(new.spec.ids, unique.id)
      
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
      
      rv.SPECIES$species[[unique.id]] <- to.add
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
      par.gen <- GenerateId(rv.ID$id.param.seed, "parameter")
      rv.ID$id.param.seed <- par.gen$seed
      par.id <- par.gen$id
      new.param.ids <- c(new.param.ids, par.id)
      
      idx.to.add <- nrow(rv.ID$id.df) + 1
      rv.ID$id.df[idx.to.add, ] <- c(par.id, param)
      
      is.custom <- (param == LHS.var)
      
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
      rv.PARAMETERS$parameters[[par.id]] <- to.par.list
    }
  } else {
    new.params <- NA
    new.param.ids <- NA
  }
  
  # Update equation entry
  rv.CUSTOM.EQNS$ce.equations[[eqn.id]] <- list(
    "ID" = eqn.id,
    "Equation" = eqn.out,
    "New.Species" = collapseVector(new.species),
    "New.Species.id" = collapseVector(new.spec.ids),
    "New.Parameters" = collapseVector(new.params),
    "New.Parameters.id" = collapseVector(new.param.ids),
    "Old.Species" = collapseVector(existing.species),
    "Old.Species.id" = collapseVector(exist.spec.ids),
    "Old.Parameters" = collapseVector(existing.params),
    "Old.Parameters.id" = collapseVector(exist.param.ids),
    "Has.Time.Var" = time.var.exists
  )
  
  sendSweetAlert(
    session = session,
    title = "Success!",
    text = "Custom equation updated successfully.",
    type = "success"
  )
  
  # Close modal
  toggleModal(session, "modal_edit_custom_eqn", toggle = "close")
})

# Delete preview table
output$RHT_custom_eqn_delete_preview <- renderRHandsontable({
  if (isTruthy(input$PI_custom_eqn_delete_select) && length(input$PI_custom_eqn_delete_select) > 0) {
    eqns.to.show <- c()
    for (lhs in input$PI_custom_eqn_delete_select) {
      for (id in names(rv.CUSTOM.EQNS$ce.equations)) {
        eqn <- rv.CUSTOM.EQNS$ce.equations[[id]]$Equation
        if (grepl("=", eqn)) {
          eqn.lhs <- trimws(strsplit(eqn, "=")[[1]][1])
          if (eqn.lhs == lhs) {
            eqns.to.show <- c(eqns.to.show, eqn)
            break
          }
        }
      }
    }
    
    if (length(eqns.to.show) > 0) {
      df <- data.frame(Equations = eqns.to.show)
      # Calculate height based on number of rows, max 300px
      n.rows <- nrow(df)
      table.height <- min(50 + (n.rows * 30), 300)
      hot <- rhandsontable(df,
                          stretchH = "all",
                          overflow = "visible",
                          height = table.height) %>%
        hot_col(col = "Equations", readOnly = TRUE)
    } else {
      df <- data.frame(Equations = "No equations selected")
      hot <- rhandsontable(df,
                          stretchH = "all",
                          overflow = "visible",
                          readOnly = TRUE,
                          height = 100)
    }
  } else {
    df <- data.frame(Equations = "Select equations to delete")
    hot <- rhandsontable(df,
                        stretchH = "all",
                        overflow = "visible",
                        readOnly = TRUE,
                        height = 100)
  }
  
  hot
})

# Delete custom equation
observeEvent(input$bttn_custom_eqn_delete_confirm, {
  if (!isTruthy(input$PI_custom_eqn_delete_select) || length(input$PI_custom_eqn_delete_select) == 0) {
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "Please select equation(s) to delete.",
      type = "error"
    )
    return()
  }
  
  # Find equation IDs to delete
  eqn.ids.to.delete <- c()
  for (lhs in input$PI_custom_eqn_delete_select) {
    for (id in names(rv.CUSTOM.EQNS$ce.equations)) {
      eqn <- rv.CUSTOM.EQNS$ce.equations[[id]]$Equation
      if (grepl("=", eqn)) {
        eqn.lhs <- trimws(strsplit(eqn, "=")[[1]][1])
        if (eqn.lhs == lhs) {
          eqn.ids.to.delete <- c(eqn.ids.to.delete, id)
          break
        }
      }
    }
  }
  
  if (length(eqn.ids.to.delete) == 0) {
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "Could not find equations to delete.",
      type = "error"
    )
    return()
  }
  
  # Collect parameters and species to potentially remove
  params.to.check <- c()
  species.to.check <- c()
  
  for (eqn.id in eqn.ids.to.delete) {
    entry <- rv.CUSTOM.EQNS$ce.equations[[eqn.id]]
    
    # Get new parameters/species IDs
    if (isTruthy(entry$New.Parameters.id)) {
      new.param.ids <- strsplit(entry$New.Parameters.id, ", ")[[1]]
      params.to.check <- c(params.to.check, new.param.ids)
    }
    
    if (isTruthy(entry$New.Species.id)) {
      new.spec.ids <- strsplit(entry$New.Species.id, ", ")[[1]]
      species.to.check <- c(species.to.check, new.spec.ids)
    }
    
    # Check if LHS is a parameter and mark for removal if only used here
    eqn <- entry$Equation
    if (grepl("=", eqn)) {
      lhs <- trimws(strsplit(eqn, "=")[[1]][1])
      lhs.id <- FindId(lhs)
      if (!is.na(lhs.id) && lhs.id %in% names(rv.PARAMETERS$parameters)) {
        # Check if parameter is only used in custom equations
        used.in.other <- FALSE
        for (other.id in names(rv.CUSTOM.EQNS$ce.equations)) {
          if (other.id != eqn.id) {
            other.entry <- rv.CUSTOM.EQNS$ce.equations[[other.id]]
            if (grepl("=", other.entry$Equation)) {
              other.lhs <- trimws(strsplit(other.entry$Equation, "=")[[1]][1])
              if (other.lhs == lhs) {
                used.in.other <- TRUE
                break
              }
            }
          }
        }
        if (!used.in.other) {
          params.to.check <- c(params.to.check, lhs.id)
        }
      }
    }
  }
  
  # Remove equations
  for (eqn.id in eqn.ids.to.delete) {
    rv.CUSTOM.EQNS$ce.equations[[eqn.id]] <- NULL
  }
  
  # Check if parameters are used elsewhere (other equations, reactions, IO)
  params.to.remove <- c()
  for (param.id in unique(params.to.check)) {
    if (is.na(param.id) || param.id == "") next
    
    # Check if used in other custom equations
    used.in.other.ce <- FALSE
    for (ce.id in names(rv.CUSTOM.EQNS$ce.equations)) {
      entry <- rv.CUSTOM.EQNS$ce.equations[[ce.id]]
      if (isTruthy(entry$Old.Parameters.id)) {
        old.param.ids <- strsplit(entry$Old.Parameters.id, ", ")[[1]]
        if (param.id %in% old.param.ids) {
          used.in.other.ce <- TRUE
          break
        }
      }
      if (isTruthy(entry$New.Parameters.id)) {
        new.param.ids <- strsplit(entry$New.Parameters.id, ", ")[[1]]
        if (param.id %in% new.param.ids) {
          used.in.other.ce <- TRUE
          break
        }
      }
    }
    
    # Check if used in reactions
    used.in.reactions <- FALSE
    if (exists("rv.REACTIONS") && length(rv.REACTIONS$reactions) > 0) {
      for (rxn.id in names(rv.REACTIONS$reactions)) {
        rxn <- rv.REACTIONS$reactions[[rxn.id]]
        if (isTruthy(rxn$Parameters.id)) {
          param.ids <- strsplit(rxn$Parameters.id, " ")[[1]]
          if (param.id %in% param.ids) {
            used.in.reactions <- TRUE
            break
          }
        }
      }
    }
    
    # Check if used in IO
    used.in.IO <- FALSE
    if (exists("rv.IO") && length(rv.IO$IO) > 0) {
      for (io.id in names(rv.IO$IO)) {
        io <- rv.IO$IO[[io.id]]
        if (isTruthy(io$parameter.id)) {
          if (io$parameter.id == param.id) {
            used.in.IO <- TRUE
            break
          }
        }
      }
    }
    
    if (!used.in.other.ce && !used.in.reactions && !used.in.IO) {
      params.to.remove <- c(params.to.remove, param.id)
    }
  }
  
  # Remove unused parameters
  for (param.id in params.to.remove) {
    rv.PARAMETERS$parameters[[param.id]] <- NULL
  }
  
  # Check if species are used elsewhere
  species.to.remove <- c()
  for (spec.id in unique(species.to.check)) {
    if (is.na(spec.id) || spec.id == "") next
    
    # Check if used in other custom equations
    used.in.other.ce <- FALSE
    for (ce.id in names(rv.CUSTOM.EQNS$ce.equations)) {
      entry <- rv.CUSTOM.EQNS$ce.equations[[ce.id]]
      if (isTruthy(entry$Old.Species.id)) {
        old.spec.ids <- strsplit(entry$Old.Species.id, ", ")[[1]]
        if (spec.id %in% old.spec.ids) {
          used.in.other.ce <- TRUE
          break
        }
      }
      if (isTruthy(entry$New.Species.id)) {
        new.spec.ids <- strsplit(entry$New.Species.id, ", ")[[1]]
        if (spec.id %in% new.spec.ids) {
          used.in.other.ce <- TRUE
          break
        }
      }
    }
    
    # Check if used in reactions
    used.in.reactions <- FALSE
    if (exists("rv.REACTIONS") && length(rv.REACTIONS$reactions) > 0) {
      for (rxn.id in names(rv.REACTIONS$reactions)) {
        rxn <- rv.REACTIONS$reactions[[rxn.id]]
        if (isTruthy(rxn$Species.id)) {
          spec.ids <- strsplit(rxn$Species.id, " ")[[1]]
          if (spec.id %in% spec.ids) {
            used.in.reactions <- TRUE
            break
          }
        }
      }
    }
    
    # Check if used in IO
    used.in.IO <- FALSE
    if (exists("rv.IO") && length(rv.IO$IO) > 0) {
      for (io.id in names(rv.IO$IO)) {
        io <- rv.IO$IO[[io.id]]
        if (isTruthy(io$species.id)) {
          if (io$species.id == spec.id) {
            used.in.IO <- TRUE
            break
          }
        }
      }
    }
    
    if (!used.in.other.ce && !used.in.reactions && !used.in.IO) {
      species.to.remove <- c(species.to.remove, spec.id)
    }
  }
  
  # Remove unused species
  for (spec.id in species.to.remove) {
    rv.SPECIES$species[[spec.id]] <- NULL
  }
  
  sendSweetAlert(
    session = session,
    title = "Success!",
    text = paste0("Deleted ", length(eqn.ids.to.delete), " custom equation(s)."),
    type = "success"
  )
  
  if (input$checkbox_custom_eqn_delete_keep_modal_active) {
    toggleModal(session, "modal_delete_custom_eqn", toggle = "close")
  }
})

