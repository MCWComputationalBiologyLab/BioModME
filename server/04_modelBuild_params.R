########################### Parameter Server ################################
#add title line to parameters from equations (since I don't seem to be able to do it with the renderUi below)



################################################################################
DeleteParameters <- function(paramToDelete) {
  idx <- match(paramToDelete, params$vars.all)
  params$vars.all <- params$vars.all[-idx]
  params$vals.all <- params$vals.all[-idx]
  params$comments.all <- params$comments.all[-idx]
  params$param.table <- data.frame(params$vars.all, params$vals.all, params$comments.all)
  colnames(params$param.table) <- c("Parameter", "Value", "Description")
  
  
  #check if it exists in other parameter instances
  idx <- match(paramToDelete, params$eqns.vars)
  if (!is.null(idx)) {
    params$eqns.vars <- params$eqns.vars[-idx]
    params$eqns.vals <- params$eqns.vals[-idx]
    params$eqns.comments <- params$eqns.comments[-idx]
  }
  idx <- match(paramToDelete, params$inputs.vars)
  if (!is.null(idx)) {
    params$inputs.vars <- params$inputs.vars[-idx]
    params$inputs.vals <- params$inputs.vals[-idx]
    params$inputs.comments <- params$inputs.comments[-idx]
  }
  idx <- match(paramToDelete, params$outputs.vars)
  if (!is.null(idx)) {
    params$outputs.vars <- params$outputs.vars[-idx]
    params$outputs.vals <- params$outputs.vals[-idx]
    params$outputs.comments <- params$outputs.comments[-idx]
  }
  idx <- match(paramToDelete, params$rate.eqn.vars)
  if (!is.null(idx)) {
    params$rate.eqn.vars <- params$rate.eqn.vars[-idx]
    params$rate.eqn.vals <- params$rate.eqn.vals[-idx]
    params$rate.eqn.comments <- params$rate.eqn.comments[-idx]
  }
  idx <- match(paramToDelete, params$time.dep.vars)
  if (!is.null(idx)) {
    params$time.dep.vars <- params$time.dep.vars[-idx]
    params$time.dep.values <- params$time.dep.values[-idx]
    params$time.dep.comments <- params$time.dep.comments[-idx]
  }
  updatePickerInput(session, "parameters_filter_type", selected = "Eqns")
  updatePickerInput(session, "parameters_filter_type", selected = "All")
}

#start with box removed on load
updateBox("parameter_info_box", action = "remove")

# button that displays info box on parameter page
observeEvent(input$parameter_info_button, {
  #if odd box appears, if even box disappears
  if (input$parameter_info_button %% 2 == 0) {
    updateBox("parameter_info_box", action = "remove")
  } else {
    updateBox("parameter_info_box", action = "restore")
  }
})

# Reactive variable that keeps track of parameters - used when editing table values to keep track of whats changed
parameter_table_values <- reactiveValues(table = data.frame(),
                                         table.copy = data.frame()
                                         )

param.reset.event <- reactive({
  list(input$eqnCreate_addEqnToVector,
       input$Inout_edit_addInVarToDf,
       input$Inout_addOutVarToDf_edit,
       input$Inout_addInVarToDf,
       input$Inout_addOutVarToDf,
       input$Inout_button_delete_IO_eqn,
       input$parameters_DT$changes$changes)
})

observeEvent(param.reset.event(), {
  
  updatePickerInput(
    session = session,
    inputId = "parameters_filter_type",
    selected = "Eqns"
  )
  updatePickerInput(
    session = session,
    inputId = "parameters_filter_type",
    selected = "All"
  )
  #input$parameters_filter_type <- "All"
})
# Filters parameter table based on what pickerinputs are requesting
observeEvent(input$parameters_filter_type, {
  if (input$parameters_filter_type == "All") {
    my.table <- params$param.table
  } else if (input$parameters_filter_type == "Eqns") {
    #subset table based on param eqn vars
    my.table <- params$param.table[params$param.table[,1] %in% params$eqns.vars,]
  } else if (input$parameters_filter_type == "Inputs") {
    my.table <- params$param.table[params$param.table[,1] %in% params$inputs.vars,]
  } else if (input$parameters_filter_type == "Outputs") {
    my.table <- params$param.table[params$param.table[,1] %in% params$outputs.vars,]
  }
  parameter_table_values$table <- my.table
  parameter_table_values$table.copy <- my.table
  
}) 

output$parameters_DT <- renderRHandsontable({
  rhandsontable(parameter_table_values$table,
                #rowHeaders = NULL,
                colHeaderWidth = 100,
                stretchH = "all",
                overflow = "visible"
  ) %>%
    hot_cols(colWidth = c(30, 30, 90),
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
    hot_rows(rowHeights = 40) %>%
    hot_context_menu(allowRowEdit = FALSE,
                     allowColEdit = FALSE
    ) %>%
    hot_validate_numeric(col = 2, min = 0)
})

observeEvent(input$parameters_DT$changes$changes, {
  xi  = input$parameters_DT$changes$changes[[1]][[1]]
  yi  = input$parameters_DT$changes$changes[[1]][[2]]
  old = input$parameters_DT$changes$changes[[1]][[3]]
  new = input$parameters_DT$changes$changes[[1]][[4]]
  
  if (yi == 1) {
    new <- as.character(new)
    jPrint(str_split(new, "")[[1]][1])
    if (str_split(new, "")[[1]][1] == ".") {
      new <- as.numeric(paste0("0", new))
      jPrint(new)
    }
  }
  
  
  if (input$parameters_filter_type == "All") {
    
    params$param.table[xi+1, yi+1] <- new
    params$vars.all[xi+1]          <- params$param.table[xi+1, 1]
    params$vals.all[xi+1]          <- params$param.table[xi+1, 2]
    params$comments.all[xi+1]      <- params$param.table[xi+1, 3]
    
  } else {
    row.changed <- xi+1
    col.changed <- yi+1
    
    #find name of variable that had a value changed
    name.changed <- parameter_table_values$table[xi+1, 1]
    #find the idx of that variable in main parameter table
    idx <- match(name.changed, params$param.table[,1])
    #change the changed value in main parameter table
    params$param.table[idx, col.changed] <- new
    
    #store it to its appropriate reactive variable
    params$vars.all     <- params$param.table[, 1] 
    params$vals.all     <- params$param.table[, 2]
    params$comments.all <- params$param.table[, 3]
    }
  
  loop$parameters <- params$param.table
  
  # If change of parameter name
  if (yi+1 == 1) {
    jPrint("Changing Parameters")
    params$vars.all          <- RenameParameterVector(old, new, params$vars.all)
    params$comments.all      <- RenameParameterVector(old, new, params$comments.all)
    params$eqns.vars         <- RenameParameterVector(old, new, params$eqns.vars)
    params$eqns.comments     <- RenameParameterVector(old, new, params$eqns.comments)
    params$inputs.vars       <- RenameParameterVector(old, new, params$inputs.vars)
    params$inputs.comments   <- RenameParameterVector(old, new, params$inputs.comments)
    params$outputs.vars      <- RenameParameterVector(old, new, params$outputs.vars)
    params$outputs.comments  <- RenameParameterVector(old, new, params$outputs.comments) 
    params$rate.eqn.vars     <- RenameParameterVector(old, new, params$rate.eqn.vars)
    params$rate.eqn.comments <- RenameParameterVector(old, new, params$rate.eqn.comments) 
    params$time.dep.vars     <- RenameParameterVector(old, new, params$time.dep.vars)
    params$time.dep.comments <- RenameParameterVector(old, new, params$time.dep.comments)
    eqns$main                <- RenameParameterVector(old, new, eqns$main)
    eqns$additional.eqns     <- RenameParameterVector(old, new, eqns$additional.eqns)
    eqns$rate.eqns           <- RenameParameterVector(old, new, eqns$rate.eqns)
    eqns$time.dep.eqns       <- RenameParameterVector(old, new, eqns$time.dep.eqns)
    logs$IO.logs             <- RenameParameterVector(old, new, logs$IO.logs)
    
    params$param.table       <- RenameParameterDF(old, new, params$param.table)
    eqns$eqn.info            <- RenameParameterDF(old, new, eqns$eqn.info)
    IO$IO.info               <- RenameParameterDF(old, new, IO$IO.info)
  }
  

})

observeEvent(params$vars.all, {
  updatePickerInput(session, "modal_params_to_delete", choices = params$vars.all)
})

# Modal for creating parameter
observeEvent(input$modal_create_param_button, {
  #create row for parameter df
  var <- input$modal_param_param_name
  check.vars <- variableCheck(var, vars$species, params$vars.all)
  passed.check <- check.vars[[1]]
  error.message <- check.vars[[2]]
  error.code <- check.vars[[3]]
  
  if (passed.check) {
    row.to.add <- c(input$modal_param_param_name,
                    input$modal_param_value,
                    input$modal_param_description)
    
    params$param.table[nrow(params$param.table)+1,] <- row.to.add
    updatePickerInput(session, "parameters_filter_type", selected = "Eqns")
    updatePickerInput(session, "parameters_filter_type", selected = "All")
    toggleModal(session, "modal_create_parameter", toggle =  "close")
  } else {
    session$sendCustomMessage(type = 'testmessage',
                              message = error.message)
  }
})

# Modal for deleting parameter
observeEvent(input$modal_delete_param_button, {
  var.to.delete <- input$modal_params_to_delete
  DeleteParameters(var.to.delete)
  toggleModal(session, "modal_delete_param", toggle =  "close")
})

#-------------------------------------------------------------------------------

# Parameter Debug  

#-------------------------------------------------------------------------------
  
observeEvent(input$param_view_parameters, {
  jPrint("params$vars.all")
  jPrint(params$vars.all)
  jPrint("Params$vals.all")
  jPrint(params$vals.all)
  jPrint("Params$eqns.vars")
  jPrint(params$eqns.vars)
  # jPrint("Params$eqns.vals")
  # jPrint(params$eqns.vals)
  jPrint("Params$inputs.vars")
  jPrint(params$inputs.vars)
  # jPrint("Params$inputs.vals")
  # jPrint(params$inputs.vals)
  jPrint("Params$outputs.vars")
  jPrint(params$outputs.vars)
  # jPrint("Params$outputs.vals")
  # jPrint(params$outputs.vals)
  jPrint("Params$comments.all")
  jPrint(params$comments.all)
  jPrint("Params$param.table")
  jPrint(params$param.table)
  # observe({print(ICs$vals)})
  # observe({print(vars$species)})
  # observe({print(paste(length(params$vars.all), length(params$vals.all), length(params$comments.all)))})
  # observe({print(paste(length(params$param.eqns), length(params$eqns.vals), length(params$eqns.comments)))})
  # observe({print(paste(length(params$param.inputs), length(params$inputs.vals), length(params$inputs.comments)))})
  # observe({print(paste(length(params$param.outputs), length(params$outputs.vals), length(params$outputs.comments)))})
  # observe({print(paste(length(params$rate.eqn.vars), length(params$rate.eqn.vals), length(params$rate.eqn.comments)))})
})

observeEvent(input$param_remove_duplicate_parameters, {
  params$vars.all <- unique(params$vars.all)
  params$param.eqns <- unique(params$param.eqns)
  params$param.inputs <- unique(params$param.inputs)
  params$param.outputs <- unique(params$param.outputs)
  params$rate.eqn.vars <- unique(params$rate.eqn.vars)
  # observe({print(paste(length(params$vars.all), length(params$vals.all), length(params$comments.all)))})
  # observe({print(paste(length(params$param.eqns), length(params$param.eqnsparams$comments.allvalues), length(params$param.eqnsparams$comments.allcomments)))})
  # observe({print(paste(length(params$param.inputs), length(params$param.inputsparams$comments.allvalues), length(params$param.inputsparams$comments.allcomments)))})
  # observe({print(paste(length(params$param.outputs), length(params$param.outputsparams$comments.allvalues), length(params$param.outputsparams$comments.allcomments)))})
  # observe({print(paste(length(params$rate.eqn.vars), length(params$rate.eqn.varsparams$comments.allvalues), length(params$rate.eqn.varsparams$comments.allcomments)))})
  # 
})




