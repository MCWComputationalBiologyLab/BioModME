########################### Parameter Server ################################
#add title line to parameters from equations (since I don't seem to be able to do it with the renderUi below)



# Functions --------------------------------------------------------------------


# On Application Load ----------------------------------------------------------
# Start with box removed on load
updateBox("parameter_info_box", action = "remove")

# Event Updates ----------------------------------------------------------------






# New Table Reactive Variables -------------------------------------------------
# Reactive variable that keeps track of parameters 
# Used when editing table values to keep track of whats changed
# parameter_table_values <- reactiveValues(table = data.frame(),
#                                          table.copy = data.frame()
#                                          )

# Parameter Filters ------------------------------------------------------------
observeEvent(input$parameters_filter_type, {

}) 

# Parameter Table RHandsontable ------------------------------------------------
output$parameters_DT <- renderRHandsontable({
  req(length(rv.PARAMETERS$parameters) > 0)
  
  # Override storage used to rerender table when table edits are rejected.
  override <- rv.REFRESH$refresh.param.table
  
  for.table <- rv.PARAMETERS$parameters.df
  
  # For now remove custom parameters from table
  for.table <- for.table %>% 
    filter(!Custom)
  # Apply Filters
  if (input$parameters_filter_type != "All") {
    for.table <- for.table %>%
      filter(Type == input$parameters_filter_type)
  }
  
  for.table <- for.table %>%
    select("Name", "Value", "Unit", "Description")
  
  # rhandsontable(for.table)
  rhandsontable(for.table,
                #rowHeaders = NULL,
                colHeaderWidth = 100,
                stretchH = "all"
                #overflow = "visible"
  ) %>%
    hot_cols(colWidth = c(30, 15, 15, 90),
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
    #hot_col("Parameter", readOnly = TRUE) %>%
    #hot_col("Description", halign = "htLeft", valign = "htMiddle") %>%
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(allowRowEdit = FALSE,
                     allowColEdit = FALSE
    ) %>%
    hot_validate_numeric(col = 2, min = 0)
})

# Event: Parameter table value changes -----------------------------------------
observeEvent(input$parameters_DT$changes$changes, {
  xi  = input$parameters_DT$changes$changes[[1]][[1]]
  yi  = input$parameters_DT$changes$changes[[1]][[2]]
  old = input$parameters_DT$changes$changes[[1]][[3]]
  new = input$parameters_DT$changes$changes[[1]][[4]]

  # Find parameter name that was changed
  
  plotted.table <- rv.PARAMETERS$parameters.df
  
  # Apply Filters
  
  plotted.table <- plotted.table %>% 
    filter(!Custom)
  
  if (input$parameters_filter_type != "All") {
    plotted.table <- plotted.table %>%
      filter(Type == input$parameters_filter_type)
  }
  
  plotted.table <- plotted.table %>%
    select("Name", "Value", "Unit", "Description")
  
  par.name <- unname(unlist(plotted.table[xi+1, 1]))
  par.id   <- FindId(par.name)
  
  if (yi == 0) {
    # PARAMETER NAME CHANGE
    rv.PARAMETERS$parameters[[par.id]]$Name <- new

    rv.LOGS$IO.logs <- RenameVarInVector(old, new, rv.LOGS$IO.logs)

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
    
    # If volume change in compartment data structure
    if (rv.PARAMETERS$parameters[[par.id]]$Type == "Compartment") {
      # Find which compartment has this volume
      for (i in seq(length(rv.COMPARTMENTS$compartments))) {
        # If the volume name == old volume name 
        if (rv.COMPARTMENTS$compartments[[i]]$Volume == old) {
          rv.COMPARTMENTS$compartments[[i]]$Volume = new
          break
        }
      }
    }
    
    # Change parameter name in ID database
    idx.for.id <- which(rv.ID$id.df[, 2] %in% old)
    var.id <- rv.ID$id.df[idx.for.id, 1]
    rv.ID$id.df[idx.for.id, 2] <- new
    
  } 
  else if (yi == 1) {
    # PARAMETER VALUE CHANGE
    # browser()
    # Set booleans
    conversion.needed <- FALSE
    
    # Parameter value change 
    rv.PARAMETERS$parameters[[par.id]]$Value <- new

    # Change base value of parameter if needed
    selected.unit <- rv.PARAMETERS$parameters[[par.id]]$Unit
    base.unit     <- rv.PARAMETERS$parameters[[par.id]]$BaseUnit
    
    if (is.na(selected.unit) || is.na(base.unit)) {
      # Account for parameters with no units
      rv.PARAMETERS$parameters[[par.id]]$BaseValue <- new
      
    } else {
      if (selected.unit != base.unit) {
        # Perform unit conversion
        conversion.needed <- TRUE
        descriptor <- rv.PARAMETERS$parameters[[par.id]]$UnitDescription
        converted.value <- UnitConversion(descriptor,
                                          selected.unit,
                                          base.unit,
                                          as.numeric(new))
        rv.PARAMETERS$parameters[[par.id]]$BaseValue <- converted.value
      } else {
        rv.PARAMETERS$parameters[[par.id]]$BaseValue <- new
      }
      
      # If volume change in compartment data structure
      if (rv.PARAMETERS$parameters[[par.id]]$Type == "Compartment") {
        # Find which compartment has this volume
        vol.name <- rv.PARAMETERS$parameters[[par.id]]$Name
        for (i in seq(length(rv.COMPARTMENTS$compartments))) {
          if (rv.COMPARTMENTS$compartments[[i]]$Volume == vol.name) {
            if (conversion.needed) {
              rv.COMPARTMENTS$compartments[[i]]$BaseValue <- converted.value
            } else {
              rv.COMPARTMENTS$compartments[[i]]$BaseValue <- new
            }
            rv.COMPARTMENTS$compartments[[i]]$Value <- new
            break
          }
        }
      }
    }
    
    
  } 
  else if (yi == 2) {
    # UNIT CHANGE
    
    # Check if no unit exists, then skip and reassign NA
    # Note Rhandsontable stores NA as NULL, hence the null check
    if (!is.null(old)) {
      # check if units are acceptable
      descriptor <- rv.PARAMETERS$parameters[[par.id]]$UnitDescription
      
      # Check to make sure units entered are the right ones
      comparison <- UnitCompare(descriptor,
                                new,
                                rv.UNITS$units.choices)
      
      if (comparison$is.match) {
        # Parameter unit change
        new <- Unit_Dict_Convert(UNIT_MAPPING, new)
        
        rv.PARAMETERS$parameters[[par.id]]$Unit <- new
        rv.REFRESH$refresh.param.table <- rv.REFRESH$refresh.param.table + 1
        
        # We take current value on table as unitvalue
        # We take current unit as the previous units
        # We take base unit as new Units
        # The converted value will be the new base unit value
        
        # Perform Conversion for base value if needed
        from.unit <- rv.PARAMETERS$parameters[[par.id]]$Unit
        to.unit   <- rv.PARAMETERS$parameters[[par.id]]$BaseUnit
        from.val  <- rv.PARAMETERS$parameters[[par.id]]$Value
        
        if (from.unit != to.unit) {
          # Perform unit conversion for base
          descriptor <- rv.PARAMETERS$parameters[[par.id]]$UnitDescription
          converted.value <- UnitConversion(descriptor,
                                            from.unit,
                                            to.unit,
                                            as.numeric(from.val))
          rv.PARAMETERS$parameters[[par.id]]$BaseValue <- converted.value
        } else {
          rv.PARAMETERS$parameters[[par.id]]$BaseValue <- from.val
        }
        
        # If volume change in compartment data structure change unit there
        if (rv.PARAMETERS$parameters[[par.id]]$Type == "Compartment") {
          # Find which compartment has this volume and change unit/basevalue
          vol.name <- rv.PARAMETERS$parameters[[par.id]]$Name
          for (i in seq(length(rv.COMPARTMENTS$compartments))) {
            if (rv.COMPARTMENTS$compartments[[i]]$Volume == vol.name) {
              rv.COMPARTMENTS$compartments[[i]]$Unit <- 
                rv.PARAMETERS$parameters[[par.id]]$Unit
              
              rv.COMPARTMENTS$compartments[[i]]$BaseValue <- 
                rv.PARAMETERS$parameters[[par.id]]$BaseValue
              break
            }
          }
        }
      } else {
        # if unit conversion isn't allowed
        rv.REFRESH$refresh.param.table <- rv.REFRESH$refresh.param.table + 1
        rv.PARAMETERS$parameters[[par.id]]$Unit <- old
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = comparison$message,
          type = "error"
        )
      }
    } else {
      # Reassign NA
      # 
      # I want to add the ability to add units to custom Parameters which are 
      # the NA ones listed here.  To do so we need to change:
      # $Unit
      # $UnitDescription
      # $BaseUnit
      # $BaseValue (possibly)
      # 
      # To add the unit we would have to take what the user puts in and verify
      # that it is valid.  Steps:
      # (1) Break entered units: umol/L -> c("umol", "div", "L")
      #   The fxn UnitBreak will give c("umol", "/", "L")
      # (2) Find which are units and what types. 
      # (3) Create Base unit term: mol/L
      # (4) Create UnitDescription Term: conc (mol) <div> volume
      # (5) Perform conversion to BaseValue umol/L -> mol/L
      # 
      # Break Term
      #browser()
      new.unit <- UnitBreak(new)
      # Find Unit Definition
      unit.definition <- UnitTermsToDefinition(new.unit, rv.UNITS$units.choices)
      ud.string <- paste0(unit.definition, collapse = "")
      # Check to make sure unit definition is valid: 
      if (is.null(unit.definition)) {
        rv.REFRESH$refresh.param.table <- rv.REFRESH$refresh.param.table + 1
        rv.PARAMETERS$parameters[[par.id]]$Unit <- NA
        
      } else {
        # Create base unit term
        base.unit <- ConvertToUnitTerm(unit.definition, rv.UNITS$units.base)
        # Perform Conversion to BaseUnits if necessary
        from.val  <- rv.PARAMETERS$parameters[[par.id]]$Value
        
        if (new != base.unit) {
          # Perform unit conversion for base
          descriptor <- unit.definition
          converted.value <- UnitConversion(descriptor,
                                            new.unit,
                                            base.unit,
                                            as.numeric(from.val))
          # Store base unit value
          rv.PARAMETERS$parameters[[par.id]]$BaseValue <- converted.value
        } else {
          rv.PARAMETERS$parameters[[par.id]]$BaseValue <- from.val
        }
        print(new)
        print(ud.string)
        print(base.unit)
        # Store unit definitions
        rv.PARAMETERS$parameters[[par.id]]$Unit <- new
        rv.PARAMETERS$parameters[[par.id]]$UnitDescription <- ud.string
        rv.PARAMETERS$parameters[[par.id]]$BaseUnit <- base.unit
      }
      
      rv.REFRESH$refresh.param.table <- rv.REFRESH$refresh.param.table + 1
      #rv.PARAMETERS$parameters[[par.id]]$Unit <- NA
    }
    
  } else if (yi == 3) {
    # Parameter description change
    rv.PARAMETERS$parameters[[par.id]]$Description <- new
  }
})

observeEvent(rv.PARAMETERS$parameters, {
  rv.PARAMETERS$parameters.df <- bind_rows(rv.PARAMETERS$parameters)
})

# Parameter Debug -------------------------------------------------------------- 

observeEvent(rv.PARAMETERS$parameters, {
  rv.PARAMETERS$parameters.df <- bind_rows(rv.PARAMETERS$parameters)
  if (nrow(rv.PARAMETERS$parameters.df) > 0) {
    parameters.names <- rv.PARAMETERS$parameters.df %>% dplyr::select(Name)
    rv.PARAMETERS$parameters.names <- as.vector(unlist(parameters.names))
  } else {
    rv.PARAMETERS$parameters.names <- vector()
  }
  
})
