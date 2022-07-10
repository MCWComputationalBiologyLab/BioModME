

strsplits <- function(x, splits, ...)
#splits string on multiple inputs
#used strsplits(a, c(",", " ")) for space and comma splits of c
#returns vector of split variables
#https://stackoverflow.com/questions/10738729/r-strsplit-with-multiple-unordered-split-arguments
{
  for (split in splits)
  {
    x <- unlist(strsplit(x, split, ...))
  }
  return(x[!x == ""]) # Remove empty values
}

variableCheck <- function(variable, 
                          currentVarList, 
                          parameterList
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
  
  var.pass <- TRUE
  error.message <- "None"
  error.code = 0 
  first.letter.of.var <- substr(variable, 1, 1)
  ex <- "[^[:alnum:]_.]" #regrex expression checks if values contains alpha numeric char, _, and .
  
  #check for repeat var
  if (variable %in% currentVarList) {
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
    var.pass <- FALSE
    error.message <- paste0(variable, ": Variable is already used in parameters")
    error.code <- 5
  }
  #check to see if variable is blank space
  else if (grepl("^\\s*$", variable)) {
    var.pass <- FALSE
    error.message <- "Variable is missing..."
    error.code <- 6
  }
  
  out <- list(var.pass, error.message, error.code)
  return(out)
}

# Add Variable Button Action
observeEvent(input$createVar_addVarToList, {
  if (input$createVar_varInput == "")
  {
    #nothing happens if a blank space is added
  }
  # else if (input$createVar_varInput %in% vars$species) #var already exists in model, let user know
  # {
  #   session$sendCustomMessage(type = 'testmessage',
  #                             message = 'This variable is already used')
  #   updateTextInput(session = session
  #                   ,'createVar_varInput'
  #                   ,value = "")
  # }
  else
  {
    #split input
    vector.of.vars <- strsplits(input$createVar_varInput, c(",", " "))
    for (i in seq(length(vector.of.vars))) {
      var <- vector.of.vars[i]
      check.vars <- variableCheck(var, vars$species, params$vars.all)
      passed.check <- check.vars[[1]]
      error.message <- check.vars[[2]]
      if (passed.check) {
        vars$species <- append(vars$species, vector.of.vars[i])
        vars$descriptions <- append(vars$descriptions, "")
        #assign id to variable
        ids <- GenerateId(id$id.var.seed, "variable")
        unique.id <- ids[[2]]
        id$id.var.seed <- ids[[1]]
        idx.to.add <- nrow(id$id.variables) + 1
        id$id.variables[idx.to.add, ] <- c(unique.id, vector.of.vars[i])
        #add variable to variable table
        if (nrow(vars$table) == 0) {
          vars$table[1,] <- c(var, "")
        } else {
          row.to.df <- c(var, "")
          vars$table <- rbind(vars$table, row.to.df)
        }
        #add variable to ICs table
        if (nrow(ICs$ICs.table) == 0) {
          ICs$ICs.table[1,] <- c(var, 0, paste0("Initial Concentration of ", var))
          ICs$vals <- c(ICs$vals, 0)
          ICs$comments <- c(ICs$comments, paste0("Initial Concentration of ", var))
        } else {
          row.to.df <- c(var, 0, paste0("Initial Concentration of ", var))
          ICs$ICs.table <- rbind(ICs$ICs.table, row.to.df)
          ICs$vals <- c(ICs$vals, 0)
          ICs$comments <- c(ICs$comments, paste0("Initial Concentration of ", var))
        }
        loop$ICs <- ICs$ICs.table
      }
      else{
        # session$sendCustomMessage(type = 'testmessage',
        #                           message = error.message)
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = error.message,
          type = "error"
        )
      }
      
    }
    #store selected variable to list of variables
    #vars$species <- append(vars$species, input$createVar_varInput)
    #reset text input to blank when variable entered
    updateTextInput(session = session
                    ,'createVar_varInput'
                    ,value = "")
    
    updatePickerInput(session = session
                      ,"createVar_deleteVarPicker"
                      ,choices = vars$species)
  }
})

observeEvent(input$confirmDelete, {
  #find location of variable in var list (match or which function)
  value.to.find <- input$createVar_deleteVarPicker
  idx.of.value <- match(value.to.find, vars$species)
  #remove that location from species, description, and table
  vars$species <- vars$species[-idx.of.value]
  vars$descriptions <- vars$descriptions[-idx.of.value]
  vars$table <- vars$table[-idx.of.value, ]
  
  #move that location from all IC values 
  ICs$vals <- ICs$vals[-idx.of.value]
  ICs$comments <- ICs$comments[-idx.of.value]
  ICs$ICs.table <- ICs$ICs.table[-idx.of.value, ]
  
  
  removeModal()
  #reset pickerinputs for variables
  updatePickerInput(session
                    ,"InOut_selectVar"
                    ,choices = vars$species)
  updatePickerInput(session = session
                    ,"createVar_deleteVarPicker"
                    ,choices = vars$species)
})
#delete variable button action
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

output$createVar_displayVars <- renderText({
  if (length(vars$species > 0)) {
    paste(vars$species, collapse = "<br>")
  } else {
    paste("Added Variables will appear here")
  }
  
})


# ---Rhandsometable rendering---------------------------------------------------
output$myVariables_DT <- renderRHandsontable({
  colnames(vars$table) <- c("Variable Name", "Description")
  jPrint("num col")
  jPrint(nrow(vars$table))
  if (nrow(vars$table) == 0) {
    temp <- data.frame(c("<- Add Variable(s) to begin", " "))
    temp <- transpose(temp)
    colnames(temp) <- c("Variable Name", "Description")
    rhandsontable(temp,
                  rowHeaders = NULL,
                  colHeaderWidth = 100,
                  stretchH = "all",
                  readOnly = TRUE
    ) %>%
      hot_cols(colWidth = c(90, 30),
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
      hot_rows(rowHeights = 40) %>%
      hot_context_menu(allowRowEdit = FALSE,
                       allowColEdit = FALSE
      )
  } else {
    rhandsontable(vars$table,
                  rowHeaders = NULL,
                  colHeaderWidth = 100,
                  stretchH = "all"
    ) %>%
      hot_cols(colWidth = c(30, 90),
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
      hot_col("Variable Name", readOnly = TRUE) %>%
      hot_rows(rowHeights = 40) %>%
      hot_context_menu(allowRowEdit = FALSE,
                       allowColEdit = FALSE
      )
  }
  
  
})


observeEvent(input$myVariables_DT$changes$changes, {
  xi = input$myVariables_DT$changes$changes[[1]][[1]]
  yi = input$myVariables_DT$changes$changes[[1]][[2]]
  old = input$myVariables_DT$changes$changes[[1]][[3]]
  new = input$myVariables_DT$changes$changes[[1]][[4]]
  
  # Add check in here for variable changing name
  
  #copying table to dataframe
  vars$table[xi+1, yi+1]  <- new
  #vars$species[xi+1]      <- vars$table[xi+1, 1]
  vars$descriptions[xi+1] <- vars$table[xi+1, 2]
})


# button that displays info box on parameter page
observeEvent(input$create_var_info_button, {
  #if odd box appears, if even box disappears
  if (input$create_var_info_button %% 2 == 0) {
    updateBox("create_var_info_box", action = "remove")
  } else {
    updateBox("create_var_info_box", action = "restore")
  }
})

# observeEvent(input$view_ids, {
#   jPrint(id$id.variables)
#   jPrint(id$id.parameters)
#   jPrint(id$id.equations)
#   jPrint(id$id.diffeq)
#   jPrint(id$id.seed)
# })
