# observeEvent(input$test_popup_table, {
#   print("Test popout button")
#   if (input$createVar_show_active_compartment_only) {
#     #Extract variables of active compartment
#     my.compartment <- input$createVar_active_compartment
#     df.by.comp <- filter(rv.SPECIES$species.df, Compartment == my.compartment)
#     df.by.comp <- select(df.by.comp, 
#                          Name, 
#                          Value, 
#                          Unit, 
#                          Compartment, 
#                          Description)
#   } else {
#     df.by.comp <- select(rv.SPECIES$species.df, 
#                          Name, 
#                          Value, 
#                          Unit, 
#                          Compartment, 
#                          Description)
#   }
#   df.by.comp <- as.data.frame(df.by.comp)
#   colnames(df.by.comp) <- c("Name",
#                             "Value",
#                             "Unit",
#                             "Compartment",
#                             "Description"
#   )
#   print(df.by.comp)
#   showTableInPopup(df.by.comp, session, width = 900, height = 500)
#   print("show over")
# })

# Helper Functions -------------------------------------------------------------
variableCheck <- function(variable, 
                          currentVarList, 
                          parameterList,
                          allowRepeatParam = FALSE,
                          allowRepeatVar = FALSE
) {
  #function checks if variable is good to use for model
  # Inputs: 
  #  @variable - variable to be checked for conflicts
  #  @currentVarList - vector of variable names
  #  @parameterList  - vector of parameter names
  # Outputs:
  #  @var.pass - boolean, true if no conflicts, false if conflicts
  #  @error.message - message describing conflict
  #  @error.code - numeric code referring to type of conflict
  
  
  #Checks for: 
  # 1. Repeat Var Name
  # 2. Var starting with number
  # 3. Var containing punctuation that is not "_" or "."
  # 4. Check that variable does not start with punctuation
  #Returns:
  # 1. True if variable is okay, False if variable is not
  # 2. Error code of the problem
  # 3. Int value relating to error messages
  
  #Error Codes:
  # 0 - No Error
  # 1 - Variable name found in variable name vector
  # 2 - Variable name starts with number
  # 3 - Variable name contains special characters
  # 4 - Variable name starts with punctuation
  # 5 - Variable name found in parameter names
  #browser()
  var.pass <- TRUE
  error.message <- "None"
  error.code = 0 
  first.letter.of.var <- substr(variable, 1, 1)
  
  #regrex expression checks if values contains alpha numeric char, _, and .
  ex <- "[^[:alnum:]_.]" 
  repeat.param <- FALSE
  
  #check for repeat var
  print(allowRepeatVar)
  print(variable %in% currentVarList)
  if (variable %in% currentVarList && !allowRepeatVar) {
    print("var used")
    var.pass <- FALSE
    error.message <- paste0(variable, ": Variable is already used")
    error.code <- 1
  }
  #checks if the first letter of the variable is a number
  else if (grepl("^([0-9])", first.letter.of.var)) {
    var.pass <- FALSE
    error.message <- paste0(variable, ": Variables cannot start with number")
    error.code <- 2
  }
  #checks if variable contains punctuation other than . or _
  else if (grepl(ex, variable)) {
    var.pass <- FALSE
    error.message <- paste0(variable, ": Contains special characters")
    error.code <- 3
  }
  #check to see if variable starts with punctuation
  else if (grepl("^([[:punct:]])", variable)) {
    var.pass <- FALSE
    error.message <- paste0(variable, ": starts with punctuation")
    error.code <- 4
  }
  else if (variable %in% parameterList) {
    
    if (!allowRepeatParam) {
      var.pass <- FALSE
      error.message <- paste0(variable, 
                              ": Variable is already used in parameters")
      error.code <- 5
    }
    else {
      var.pass <- TRUE
      repeat.param <- TRUE
    }
    
  }
  #check to see if variable is blank space
  else if (grepl("^\\s*$", variable)) {
    var.pass <- FALSE
    error.message <- "Variable is missing..."
    error.code <- 6
  }
  
  out <- list(var.pass, error.message, error.code, repeat.param)
  return(out)
}

parameterCheck <- function(parameter, 
                           speciesList,
                           parameterList,
                           compartmentList,
                           allowRepeatParam = TRUE
) {
  #function checks if variable is good to use for model
  # Inputs: 
  #  @parameter - new parameter entry to check (whole list entry)
  #  @currentVarList - species RV (rv.SPECIES$species)
  #  @parameterList  - parameter RV(rv.PARAMETERS$parameters)
  #  @compartmentList - compartment RV (rv.COMPARTMENT$compartments)
  # Outputs:
  #  @var.pass - boolean, true if no conflicts, false if conflicts
  #  @error.message - message describing conflict
  #  @error.code - numeric code referring to type of conflict
  
  
  #Checks for: 
  # 1. Repeat Var Name
  # 2. Var starting with number
  # 3. Var containing punctuation that is not "_" or "."
  # 4. Check that variable does not start with punctuation
  #Returns:
  # 1. True if variable is okay, False if variable is not
  # 2. Error code of the problem
  # 3. Int value relating to error messages
  
  #Error Codes:
  # 0 - No Error
  # 1 - Variable name found in variable name vector
  # 2 - Variable name starts with number
  # 3 - Variable name contains special characters
  # 4 - Variable name starts with punctuation
  # 5 - Variable name found in parameter names
  # browser()
  
  # Extract name vectors 
  par.name      <- parameter$Name
  species.names <- unname(sapply(speciesList, get, x = "Name"))
  params.names  <- unname(sapply(parameterList, get, x = "Name"))
  compart.names <- unname(sapply(compartmentList, get, x = "Name"))
  
  var.pass <- TRUE
  error.message <- "None"
  error.code = 0 
  first.letter.of.var <- substr(par.name, 1, 1)

  #regrex expression checks if values contains alpha numeric char, _, and .
  ex <- "[^[:alnum:]_.]" 
  repeat.param <- FALSE
  
  #check for repeat var
  if (par.name %in% species.names) {
    var.pass <- FALSE
    error.message <- paste0(par.name, 
                            ": value is used as species already")
    error.code <- 1
  }
  else if (par.name %in% compart.names) {
    var.pass <- FALSE
    error.message <- paste0(par.name, 
                            ": value is used as compartment already")
    error.code <- 1
  }
  #checks if the first letter of the variable is a number
  else if (grepl("^([0-9])", first.letter.of.var)) {
    var.pass <- FALSE
    error.message <- paste0(par.name, ": Variables cannot start with number")
    error.code <- 2
  }
  #checks if variable contains punctuation other than . or _
  else if (grepl(ex, par.name)) {
    var.pass <- FALSE
    error.message <- paste0(par.name, ": Contains special characters")
    error.code <- 3
  }
  #check to see if variable starts with punctuation
  else if (grepl("^([[:punct:]])", par.name)) {
    var.pass <- FALSE
    error.message <- paste0(par.name, ": starts with punctuation")
    error.code <- 4
  }

  else if (par.name %in% params.names) {
    # Need to check to make sure parameter change can occur because base units 
    # are the same
    
    # Find idx of name, pull that entry and grab unit comparison
    new.unit.d <- parameter$UnitDescription
    idx <- which(params.names %in% par.name)
    old.unit.d <- parameterList[[idx]]$UnitDescription
    
    # Make sure unit description is the same of each. 
    if (old.unit.d != new.unit.d) {
      var.pass <- FALSE
      error.message <- paste0(par.name, 
                              ": parameter name is used and has a different 
                              unit definition than current context. Parameters
                              can share the same name if they share the same 
                              base units. ",
                              old.unit.d, " vs ", new.unit.d)
      error.code <- 5
    }
    
    # if (!allowRepeatParam) {
    #   var.pass <- FALSE
    #   error.message <- paste0(par.name, 
    #                           ": Variable is already used in parameters and 
    #                           parameter rename allowed is off.")
    #   error.code <- 5
    # }
    # else {
    #   var.pass <- TRUE
    #   repeat.param <- TRUE
    # }
    
  }
  #check to see if variable is blank space
  else if (grepl("^\\s*$", par.name)) {
    var.pass <- FALSE
    error.message <- "Variable is missing..."
    error.code <- 6
  }
  
  out <- list(var.pass, error.message, error.code, repeat.param)
  return(out)
}

FindId <- function(varName) {
  # Searches Id database to find ID corresponding to name
  if (!(is.na(varName) | is.null(varName))) {
    idx <- which(rv.ID$id.df[,2] %in% varName)
    var.id <- rv.ID$id.df[idx, 1]
  } else {
    var.id <- NA
  }
  
  return(var.id)
}

# UI Render --------------------------------------------------------------------
output$createVar_species_compartment_options <- renderUI({
  if (length(rv.COMPARTMENTS$compartments) > 1) {

  }
})

# Variables --------------------------------------------------------------------
## Add Variable Button ---------------------------------------------------------
observeEvent(input$createVar_add_variable_button, {
  # Find Base Naming Variables
  current.n <- length(rv.SPECIES$species) + 1
  base = "species"
  name.to.add <- paste0(base, "_", current.n)
  
  # Generate ID
  ids <- GenerateId(rv.ID$id.var.seed, "var")
  unique.id <- ids[[2]]
  rv.ID$id.var.seed <- ids[[1]]
  idx.to.add <- nrow(rv.ID$id.df) + 1
  rv.ID$id.df[idx.to.add, ] <- c(unique.id, paste0(base, "_", current.n))
  
  # Create List Entry
  to.add <- list(Name = paste0(base, "_", current.n),
                 ID = unique.id,
                 Value = 0,
                 Unit = paste0(rv.UNITS$units.selected$For.Var,
                               "/",
                               rv.UNITS$units.selected$Volume), 
                 UnitDescription = paste0("conc (",
                                          rv.UNITS$units.selected$For.Var, 
                                          ")",
                                          " <div> ",
                                          rv.UNITS$units.selected$Volume),
                 BaseUnit = paste0(rv.UNITS$units.base$For.Var,
                                   "/",
                                   rv.UNITS$units.base$Volume), 
                 BaseValue = 0,
                 Description = "",
                 Compartment = input$createVar_active_compartment,
                 Compartment.id = FindId(input$createVar_active_compartment),
                 BoundaryCondition = TRUE,
                 Reaction.ids = NA,
                 IO.ids = NA
                 )
  
  # Add Entry To RV
  rv.SPECIES$species[[current.n]] <- to.add
  names(rv.SPECIES$species)[current.n] <- unique.id
  
  # Build DataFrame
  rv.SPECIES$species.df <- bind_rows(rv.SPECIES$species)
  if (nrow(rv.SPECIES$species.df) > 0) {
    var.names <- rv.SPECIES$species.df %>% dplyr::select(Name)
    rv.SPECIES$species.names <- as.vector(unlist(var.names))
  } else {
    rv.SPECIES$species.names <- vector()
  }
  
})

observeEvent(input$createVar_add_variable_to_all_button, {
  toggleModal(session,
              "modal_create_variable",
              toggle = "open")
})

observeEvent(input$createVar_add_to_all_compartments, {
  if (input$createVar_add_to_all_compartments) {
    shinyjs::hide("createVar_add_variable_button")
    shinyjs::show("createVar_add_variable_to_all_button")
  } else {
    shinyjs::hide("createVar_add_variable_to_all_button")
    shinyjs::show("createVar_add_variable_button")
  }
})

## Add Var To All Compartments -------------------------------------------------
observeEvent(input$modal_createVariable_add_button, {
  # browser()
  # Find Values Multi Enter
  vector.of.vars <- strsplits(input$modal_variable_name, c(",", " "))
  
  for (i in seq(length(vector.of.vars))) {
    var <- vector.of.vars[i]
    
    # Create a variable for each compartment
    for (j in seq_along(rv.COMPARTMENTS$compartments)) {
      comp.name <- rv.COMPARTMENTS$compartments[[j]]$Name
      current.n <- length(rv.SPECIES$species) + 1
      base      <- var
      
      if (input$modal_variable_name_subset == "COMPNAME") {
        name.to.add <- paste0(base, "_", comp.name)
      } else if (input$modal_variable_name_subset == "COMPNUMBER") {
        name.to.add <- paste0(base, "_", j)
      }
      
      # Generate ID
      ids <- GenerateId(rv.ID$id.var.seed, "var")
      unique.id <- ids[[2]]
      rv.ID$id.var.seed <- ids[[1]]
      idx.to.add <- nrow(rv.ID$id.df) + 1
      rv.ID$id.df[idx.to.add, ] <- c(unique.id, name.to.add)
      
      # Create List Entry
      to.add <- list(Name = name.to.add,
                     ID = unique.id,
                     Value = 0,
                     Unit = paste0(rv.UNITS$units.selected$For.Var,
                                   "/",
                                   rv.UNITS$units.selected$Volume), 
                     UnitDescription = paste0("conc (",
                                              rv.UNITS$units.selected$For.Var, 
                                              ")",
                                              " <div> ",
                                              rv.UNITS$units.selected$Volume),
                     BaseUnit = paste0(rv.UNITS$units.base$For.Var,
                                       "/",
                                       rv.UNITS$units.base$Volume), 
                     BaseValue = 0,
                     Description = "",
                     Compartment = comp.name,
                     Compartment.id = FindId(comp.name),
                     BoundaryCondition = TRUE,
                     Reaction.ids = NA,
                     IO.ids = NA
      )
      
      # Add Entry To RV
      rv.SPECIES$species[[current.n]] <- to.add
      names(rv.SPECIES$species)[current.n] <- unique.id
    }
  }
  
  toggleModal(session,
              "modal_create_variable",
              toggle = "close")
})

# Cancel button in the variable modal is pressed
observeEvent(input$modal_createVariable_cancel_button, {
  toggleModal(session,
              "modal_create_variable",
              toggle = "close")
})

# Event: Confirm Delete from Modal----------------------------------------------
observeEvent(input$button_modal_delete_species, {
  
  # Set booleans
  varUsedInModel <- FALSE
  varUsedInEqns  <- FALSE
  varUsedInIO    <- FALSE
  
  # Get Variable Id
  var.to.delete <- input$PI_modal_delete_species
  var.id        <- FindId(var.to.delete)
  
  # Check if variable is used in eqns
  eqn.df.indices <- c()
  for (i in seq_along(rv.REACTIONS$reactions.df$Species.Id)) {
    my.ids <- strsplit(rv.REACTIONS$reactions.df$Species.Id[i], " ")[[1]]
    if (var.id %in% my.ids) {
      eqn.df.indices <- c(eqn.df.indices, i)
      varUsedInEqns <- TRUE
      varUsedInModel <- TRUE
    }
  }
  
  # Check if variable is used in IO
  io.df.indices <- c()
  for (i in seq_along(rv.IO$IO.df$species.id)) {
    my.ids <- strsplit(rv.IO$IO.df$species.id[i], " ")[[1]]
    if (var.id %in% my.ids) {
      io.df.indices <- c(io.df.indices, i)
      varUsedInIO <- TRUE
      varUsedInModel <- TRUE
    }
  }
  

  # If it is notify user they cannot delete it and where it is located
  if (varUsedInModel) {
    
    messageOut <- ""
    if (varUsedInEqns & varUsedInIO) {
      messageOut <- paste0("Variable can not be deleted. ",
                           var.to.delete,
                           "is being used in equation(s) number(s): ",
                           paste0(eqn.df.indices, collapse = ","),
                           " and in Input/Outputs: ",
                           paste0(io.df.indices, collapse = ","),
                           ".")
    } else if (varUsedInEqns) {
      messageOut <- paste0("Variable can not be deleted. ",
                           var.to.delete,
                           "is being used in equation(s) number(s): ",
                           paste0(eqn.df.indices, collapse = ","),
                           ".")
    } else if (varUsedInIO) {
      messageOut <- paste0("Variable can not be deleted. ",
                           var.to.delete,
                           "is being used in Input/Ouput number(s): ",
                           paste0(io.df.indices, collapse = ","),
                           ".")
    }
    
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = messageOut,
      type = "error"
    )
  } else {
    # If not remove variable from variable data structures.
    rv.SPECIES$species[[var.id]] <- NULL

    # Notify User of Successful Removal 
    sendSweetAlert(
      session = session, 
      title = "success !!",
      text = paste0(var.to.delete, " was successfully removed."),
      type = "success"
    )
  }
  
  
  # Close Modal 
  if (!input$checkbox_modal_keep_delete_var_open) {
    toggleModal(
      session = session, 
      modalId = "modal_delete_species",
      toggle = "close"
    )
  }
})

# Event: Delete Var, Create Modal-----------------------------------------------
observeEvent(input$createVar_deleteVarButton, {
  val.to.delete <- input$createVar_deleteVarPicker
  
  showModal(modalDialog(
    
    title = paste0("Are you sure you want to delete variable: ", 
                   val.to.delete,
                   "? Removing variables that are used in equations could be detremential to the program"),
    footer = tagList(actionButton("confirmDelete", "Delete"),
                     modalButton("Cancel")
    )
  ))
  
  #add check to see if variable is used in model.  Add warning.
  
  
})

# Table Render for Variables ---------------------------------------------------
output$myVariables_DT <- renderRHandsontable({
  
  # Table override value
  override <- rv.REFRESH$refresh.species.table 
  
  if (nrow(rv.SPECIES$species.df) == 0) {
    temp <- data.frame(c("Press addition button below to add species
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
    
    if (input$createVar_show_active_compartment_only) {
      #Extract variables of active compartment
      my.compartment <- input$createVar_active_compartment
      df.by.comp <- filter(rv.SPECIES$species.df, Compartment == my.compartment)
      df.by.comp <- select(df.by.comp, 
                           Name, 
                           Value, 
                           Unit, 
                           Compartment, 
                           Description)
    } else {
      df.by.comp <- select(rv.SPECIES$species.df, 
                           Name, 
                           Value, 
                           Unit, 
                           Compartment, 
                           Description)
    }
    df.by.comp <- as.data.frame(df.by.comp)
    colnames(df.by.comp) <- c("Name",
                              "Value",
                              "Unit",
                              "Compartment",
                              "Description"
    )
    rv.SPECIES$plotted.var.table <- df.by.comp
    
    rhandsontable(df.by.comp,
                  overflow = "visible",
                  # rowHeaders = NULL,
                  selectCallback = TRUE,
                  colHeaderWidth = 100,
                  stretchH = "all",
                  fillHandle = FALSE
    ) %>%
      hot_cols(
        colWidth = c(30, 20, 20, 30, 60),
        manualColumnMove = FALSE,
        manualColumnResize = TRUE,
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
      hot_col("Compartment", readOnly = TRUE) %>%
      hot_rows(rowHeights = 30) %>%
      hot_context_menu(allowRowEdit = FALSE,
                       allowColEdit = FALSE
      )
  }
})

# Variable Input Rhandsontable: cell Change ------------------------------------
observeEvent(input$myVariables_DT$changes$changes, {
  xi = input$myVariables_DT$changes$changes[[1]][[1]]
  yi = input$myVariables_DT$changes$changes[[1]][[2]]
  old = input$myVariables_DT$changes$changes[[1]][[3]]
  new = input$myVariables_DT$changes$changes[[1]][[4]]

    # Find which variable is being changed
  var.name  <- rv.SPECIES$plotted.var.table[xi+1, 1]
  search.id <- FindId(var.name)
  # If Name changed
  if (yi == 0) {
    # SPECIES NAME CHANNGE
    
    # Check if name change is a valid new name
    
    # Find id of variable name 
    # Find variable id and change corresponding name 
    idx.for.id <- which(rv.ID$id.df[, 2] %in% old)
    var.id <- rv.ID$id.df[idx.for.id, 1]
    rv.ID$id.df[idx.for.id, 2] <- new
    
    # Search Other Areas Affected by Var Name Change
    # Steps: 
    #  Search eqn df for id.
    # Rename Parameters Found in Reaction Lists
    # rv.REACTIONS <- replace_word_recursive(rv.REACTIONS, old, new)
    # browser()
    names.list <- names(rv.REACTIONS)
    for (name in names.list) {
      rv.REACTIONS[[name]] <- 
        replace_word_recursive(rv.REACTIONS[[name]], old, new)

      rv.REACTIONS[[name]] <- 
        replace_latex_variable_recursive(rv.REACTIONS[[name]], 
                                         Var2Latex(old), 
                                         Var2Latex(new))
    }
    
    names.list <- names(rv.IO)
    for (name in names.list) {
      rv.IO[[name]] <- 
        replace_word_recursive(rv.IO[[name]], old, new)
      rv.IO[[name]] <- 
        replace_latex_variable_recursive(rv.IO[[name]],
                                         Var2Latex(old), 
                                         Var2Latex(new))
    }
    
    # Change name in species list
    rv.SPECIES$species[[search.id]]$Name <- new
    
    
    # Reset differential equations with new name
    solveForDiffEqs()

  } else if (yi == 1) {
    # CHANGE SPECIES VALUE
    rv.SPECIES$species[[search.id]]$Value <- new
    
    # Change the base value of the value if needed.
    select.unit <- rv.SPECIES$species[[search.id]]$Unit
    base.unit   <- rv.SPECIES$species[[search.id]]$BaseUnit
    if (select.unit != base.unit) {
      # Perform Unit Conversion
      descriptor <- rv.SPECIES$species[[search.id]]$UnitDescription
      converted.value <- UnitConversion(descriptor,
                                        select.unit,
                                        base.unit,
                                        as.numeric(new))
      rv.SPECIES$species[[search.id]]$BaseValue <- converted.value
    } else {
      # Simply Overwrite BaseValue
      rv.SPECIES$species[[search.id]]$BaseValue <- new
    }
  } else if (yi == 2) {
    # CHANGE SPECIES UNIT
    descriptor <- rv.SPECIES$species[[search.id]]$UnitDescription
    
    # Check to make sure units entered are the right ones
    comparison <- UnitCompare(descriptor,
                              new,
                              rv.UNITS$units.choices)
    
    if (comparison$is.match) {
      
      # Change units
      rv.SPECIES$species[[search.id]]$Unit  <- new
      
      # Change base value of variable concentration if needed
      from.unit <- rv.SPECIES$species[[search.id]]$Unit
      to.unit   <- rv.SPECIES$species[[search.id]]$BaseUnit
      from.val  <- as.numeric(rv.SPECIES$species[[search.id]]$Value)
      
      if (from.unit != to.unit) {
        # Perform Unit Conversion
        new.value <- UnitConversion(descriptor,
                                    from.unit, 
                                    to.unit,
                                    from.val)
        
        rv.SPECIES$species[[search.id]]$BaseValue <- new.value
      } else {
        rv.SPECIES$species[[search.id]]$BaseValue <- from.val
      }
      
    } else {
      rv.SPECIES$species[[search.id]]$Unit  <- old
      rv.REFRESH$refresh.species.table <- rv.REFRESH$refresh.species.table + 1
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = comparison$message,
        type = "error"
      )
    }
    
  } else if (yi == 3) {
    # CHANGE SPECIES COMPARTMENT
    rv.SPECIES$species[[search.id]]$Compartment <- new
  } else if (yi == 4) {
    # CHANGE SPECIES DESCRIPTION
    rv.SPECIES$species[[search.id]]$Description <- new
  }
  
  rv.SPECIES$species.df <- bind_rows(rv.SPECIES$species)
  
})

observeEvent(input$myVariables_DT_select$select$r, {
  req(length(rv.SPECIES$species.names > 0))
  cat("Selected Row", input$myVariables_DT_select$select$r)
  cat('\nSelected Column:',input$myVariables_DT_select$select$c)
})

# Events that change on variable change ----------------------------------------
observeEvent(rv.SPECIES$species, {
  rv.SPECIES$species.df <- bind_rows(rv.SPECIES$species)
  if (nrow(rv.SPECIES$species.df) > 0) {
    var.names <- rv.SPECIES$species.df %>% dplyr::select(Name)
    rv.SPECIES$species.names <- as.vector(unlist(var.names))
  } else {
    rv.SPECIES$species.names <- vector()
  }
  
  updatePickerInput(
    session = session,
    inputId = "PI_modal_delete_species",
    choices = rv.SPECIES$species.names
  )
  
})
