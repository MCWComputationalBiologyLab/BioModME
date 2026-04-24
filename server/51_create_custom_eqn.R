

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
    existing.vars <- rv.CE.BUILDER$existing.df
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
    new.vars    <- rv.CE.BUILDER$new.df
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

CE_NEW_TYPE_OPTIONS <- c("Parameter", "Species")

# Treat empty/NULL selection as NULL.
ce_normalize_sel <- function(sel) {
  if (is.null(sel) || length(sel) == 0) return(NULL)
  as.integer(sel[1])
}

# Derive the partitioned variable set (existing vs. new) from LHS/RHS inputs
# and keep both shadow data.frames in sync. User-selected Types in the "new"
# table are preserved across edits for variables that still exist.
observe({
  safe <- function(x) if (is.null(x)) "" else x
  LHS.var <- RemoveWS(safe(input$TI_custom_eqn_LHS))
  RHS.exp <- RemoveWS(safe(input$TI_custom_eqn_RHS))

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

  existing.rows <- list()
  new.vars      <- character(0)
  for (term in valid) {
    if (!nzchar(term)) next
    if (term == "t" || term == "time") {
      existing.rows[[length(existing.rows) + 1]] <- c(term, "Time")
    } else if (term %in% species.names) {
      existing.rows[[length(existing.rows) + 1]] <- c(term, "Species")
    } else if (term %in% param.names) {
      existing.rows[[length(existing.rows) + 1]] <- c(term, "Parameter")
    } else {
      new.vars <- c(new.vars, term)
    }
  }

  # Build existing.df (always auto-derived; fully read-only).
  if (length(existing.rows) == 0) {
    rv.CE.BUILDER$existing.df <- data.frame(
      Variables = character(),
      Type      = character(),
      stringsAsFactors = FALSE
    )
  } else {
    m <- do.call(rbind, existing.rows)
    rv.CE.BUILDER$existing.df <- data.frame(
      Variables = m[, 1],
      Type      = m[, 2],
      stringsAsFactors = FALSE
    )
  }

  # Build new.df, preserving previously-chosen Types for surviving variables.
  prev.new <- isolate(rv.CE.BUILDER$new.df)
  if (length(new.vars) == 0) {
    rv.CE.BUILDER$new.df <- data.frame(
      Variables = character(),
      Type      = character(),
      stringsAsFactors = FALSE
    )
    rv.CE.BUILDER$new.selected.row <- NULL
  } else {
    carried.types <- prev.new$Type[match(new.vars, prev.new$Variables)]
    carried.types[is.na(carried.types)] <- "Parameter"
    rv.CE.BUILDER$new.df <- data.frame(
      Variables = new.vars,
      Type      = carried.types,
      stringsAsFactors = FALSE
    )
    sel <- ce_normalize_sel(isolate(rv.CE.BUILDER$new.selected.row))
    if (!is.null(sel) && sel > nrow(rv.CE.BUILDER$new.df)) {
      rv.CE.BUILDER$new.selected.row <- NULL
    }
  }
})

# Existing-variables table: read-only display of auto-derived classification.
output$RHT_custom_eqn_params_existing <- renderDT({
  df <- rv.CE.BUILDER$existing.df

  if (is.null(df) || nrow(df) == 0) {
    placeholder <- data.frame(
      "Equation Variables" = "Variables will be extracted from the expression above.",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    return(datatable(
      placeholder,
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t", paging = FALSE, ordering = FALSE,
                     searching = FALSE, info = FALSE)
    ))
  }

  datatable(
    df,
    rownames = FALSE,
    selection = "none",
    class = "cell-border stripe",
    options = list(dom = "t", paging = FALSE, ordering = FALSE,
                   searching = FALSE, info = FALSE)
  )
}, server = FALSE)

# New-variables table: single-row selection; Type edited via picker below.
output$RHT_custom_eqn_params_new <- renderDT({
  df <- rv.CE.BUILDER$new.df

  if (is.null(df) || nrow(df) == 0) {
    placeholder <- data.frame(
      "Equation Variables" = "Variables will be extracted from the expression above.",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    return(datatable(
      placeholder,
      rownames = FALSE,
      selection = "none",
      options = list(dom = "t", paging = FALSE, ordering = FALSE,
                     searching = FALSE, info = FALSE)
    ))
  }

  datatable(
    df,
    rownames = FALSE,
    selection = list(mode = "single", target = "row"),
    class = "cell-border stripe",
    options = list(dom = "t", paging = FALSE, ordering = FALSE,
                   searching = FALSE, info = FALSE)
  )
}, server = FALSE)

# Track selected row on the new-variables table.
observeEvent(input$RHT_custom_eqn_params_new_rows_selected, {
  rv.CE.BUILDER$new.selected.row <-
    ce_normalize_sel(input$RHT_custom_eqn_params_new_rows_selected)
}, ignoreNULL = FALSE)

# Type editor (Parameter/Species picker) shown when a row is selected.
output$CE_new_type_editor <- renderUI({
  sel <- ce_normalize_sel(rv.CE.BUILDER$new.selected.row)
  df  <- rv.CE.BUILDER$new.df
  if (is.null(sel) || is.null(df) || nrow(df) == 0 || sel > nrow(df)) {
    return(helpText("Click a row above to change its Type."))
  }
  current.var  <- df$Variables[sel]
  current.type <- df$Type[sel]

  pickerInput(
    inputId  = "CE_new_type_picker",
    label    = paste0("Type for \"", current.var, "\":"),
    choices  = CE_NEW_TYPE_OPTIONS,
    selected = current.type
  )
})

# Apply picker changes back to the shadow data.frame.
observeEvent(input$CE_new_type_picker, {
  sel <- ce_normalize_sel(rv.CE.BUILDER$new.selected.row)
  if (is.null(sel)) return(NULL)
  if (sel > nrow(rv.CE.BUILDER$new.df)) return(NULL)
  new.type <- input$CE_new_type_picker
  if (!(new.type %in% CE_NEW_TYPE_OPTIONS)) return(NULL)
  rv.CE.BUILDER$new.df$Type[sel] <- new.type
}, ignoreInit = TRUE)

# Render Table to show current additional equans
output$RHT_custom_eqn_display_existing <- renderDT({
  
  if (length(rv.CUSTOM.EQNS$ce.equations) != 0) {
    additional.equations <- unname(sapply(rv.CUSTOM.EQNS$ce.equations,
                                          get,
                                          x = "Equation"))
    
    df <- data.frame(additional.equations)
    colnames(df) <- "Equations"
    datatable(
      df,
      rownames = FALSE,
      editable = FALSE,
      selection = "none",
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = FALSE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      )
    )
  } else {
    temp <- data.frame(Equations = c("Added equations will be shown here"))
    datatable(
      temp,
      rownames = FALSE,
      editable = FALSE,
      selection = "none",
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = FALSE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      )
    )
  }
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