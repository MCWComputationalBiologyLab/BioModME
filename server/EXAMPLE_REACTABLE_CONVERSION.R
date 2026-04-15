# Example: Converting myVariables_DT from Handsontable to Reactable
# This is a complete replacement for the relevant sections in server/01_species.R

# Install the package if needed (run once)
# install.packages("reactable")

library(reactable)

# Table Render for Variables (UPDATED FOR REACTABLE) ----------------------------
output$myVariables_DT <- renderReactable({
  # Table override value
  override <- rv.REFRESH$refresh.species.table 
  
  if (nrow(rv.SPECIES$species.df) == 0) {
    # Empty state message
    temp <- data.frame(Instructions = c("Press addition button below to add species to compartment."))
    
    reactable(
      temp,
      striped = TRUE,
      compact = TRUE,
      fullWidth = TRUE,
      theme = reactableTheme(
        borderColor = "#e0e0e0",
        stripedColor = "#f9f9f9"
      )
    )
  } else {
    
    if (input$createVar_show_active_compartment_only) {
      # Extract variables of active compartment
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
    
    reactable(
      df.by.comp,
      striped = TRUE,
      compact = TRUE,
      fullWidth = TRUE,
      editable = TRUE,  # Enable inline editing
      selection = "single",  # Allow row selection
      onClick = JS("function(rowInfo, column) {
        Shiny.setInputValue('myVariables_DT_select', 
          {r: rowInfo.index, c: column.id}, 
          {priority: 'event'});
      }"),
      columns = list(
        Name = colDef(
          minWidth = 120,
          editable = TRUE
        ),
        Value = colDef(
          minWidth = 80,
          editable = TRUE,
          format = colFormat(digits = 4)
        ),
        Unit = colDef(
          minWidth = 80,
          editable = TRUE
        ),
        Compartment = colDef(
          minWidth = 100,
          editable = FALSE  # Read-only, as in original
        ),
        Description = colDef(
          minWidth = 200,
          editable = TRUE
        )
      ),
      theme = reactableTheme(
        borderColor = "#e0e0e0",
        stripedColor = "#f9f9f9",
        rowSelectedStyle = list(
          backgroundColor = "#eee",
          boxShadow = "inset 2px 0 0 0 #0066cc"
        ),
        cellPadding = "8px 12px"
      )
    )
  }
})

# Variable Input Reactable: Cell Change (UPDATED FOR REACTABLE) -----------------
observeEvent(input$myVariables_DT_edit, {
  
  # Validate that edit info exists
  if (is.null(input$myVariables_DT_edit)) {
    return()
  }
  
  edit_info <- input$myVariables_DT_edit
  
  # Extract change information
  row_idx <- edit_info$row  # 1-indexed
  col_name <- edit_info$column  # Column name as string
  new <- edit_info$value  # New value
  
  # Get the old value from current displayed data
  old <- rv.SPECIES$plotted.var.table[row_idx, col_name]
  
  # Map column name to original column index (for compatibility with existing logic)
  col_mapping <- c("Name" = 0, "Value" = 1, "Unit" = 2, "Compartment" = 3, "Description" = 4)
  yi <- col_mapping[col_name]
  xi <- row_idx - 1  # Convert to 0-indexed for consistency with old code
  
  # Find which variable is being changed
  var.name  <- rv.SPECIES$plotted.var.table[row_idx, 1]
  search.id <- FindId(var.name)
  
  # If Name changed
  if (yi == 0) {
    # SPECIES NAME CHANGE
    
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
      new <- Unit_Dict_Convert(UNIT_MAPPING, new)
      rv.REFRESH$refresh.species.table <- rv.REFRESH$refresh.species.table + 1
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
    # Note: This is disabled in reactable (editable = FALSE)
    # but keeping logic for completeness
    rv.SPECIES$species[[search.id]]$Compartment <- new
    
  } else if (yi == 4) {
    # CHANGE SPECIES DESCRIPTION
    rv.SPECIES$species[[search.id]]$Description <- new
  }
  
  # Update the species dataframe
  rv.SPECIES$species.df <- bind_rows(rv.SPECIES$species)
  
})

# Row Selection Handler (UPDATED FOR REACTABLE) --------------------------------
observeEvent(input$myVariables_DT_select, {
  
  if (is.null(input$myVariables_DT_select)) {
    return()
  }
  
  row_idx <- input$myVariables_DT_select$r
  col_name <- input$myVariables_DT_select$c
  
  req(length(rv.SPECIES$species.names > 0))
  cat("Selected Row", row_idx)
  cat("\nSelected Column:", col_name)
})

# Events that change on variable change (NO CHANGES NEEDED) ----------------------
# Keep your existing observer as-is:
# observeEvent(rv.SPECIES$species, { ... })

