##############################################################################

# Compare Model
# This server controls the "Compare Mode" in the plotting module

##############################################################################

#set up waiter for data processing
w.compare <- Waiter$new(id = "Lineplot_Compare",
                           html = tagList(
                             spin_loaders(32),
                             h4("Solving Models To Compare..."))
)

# Adds parameters to picker input for comparing
observeEvent(rv.PARAMETERS$vars.all, {
  updatePickerInput(session,
                    "compare_models_select_vars",
                    choices = rv.PARAMETERS$vars.all, 
                    selected = NULL)
})

# setup optimal subplots based on number of models selected
observeEvent(input$model_compare_num_models, {
  if (input$model_compare_num_models == 2) {
    updateTextInput(session, "compare_models_num_row", value = 1)
    updateTextInput(session, "compare_models_num_col", value = 2)
  } else if (input$model_compare_num_models == 3) {
    updateTextInput(session, "compare_models_num_row", value = 3)
    updateTextInput(session, "compare_models_num_col", value = 1)
  } else if (input$model_compare_num_models == 4) {
    updateTextInput(session, "compare_models_num_row", value = 2)
    updateTextInput(session, "compare_models_num_col", value = 2)
  }
  
})
#storage for compare model values to be put into a datatable
compareModel <- reactiveValues(
  df = data.frame(matrix(ncol = 5,
                         nrow = 0,
                         dimnames = list(NULL, c("Variable",
                                                 "Model 1",
                                                 "Model 2",
                                                 "Model 3", 
                                                 "Model 4")))),
  no.values = TRUE,
  model.1 = data.frame(),
  model.2 = data.frame(),
  model.3 = data.frame(),
  model.4 = data.frame()
)

# This event builds the compare model datatable with user selected variables
observeEvent(input$compare_models_select_vars, {
  req(length(input$compare_models_select_vars) > 0)
  #check to add vars to table if they aren't in
  for (var in input$compare_models_select_vars) {
    df.2.vec <- pull(compareModel$df, "Variable") #grabs the variable column
    #We add variables user selected in picker input to this vector
    if (!(var %in% df.2.vec)) {
      #find vars parameter value and adds it to the model df
      idx <- match(var, rv.PARAMETERS$parameters.df$Name)
      value <- rv.PARAMETERS$parameters.df$Name[idx]
      row.to.df <- c(var, value, value, value, value)
      #add parameter to df
      if (compareModel$no.values) {
        compareModel$no.values = FALSE
        compareModel$df[1,] <- row.to.df
      } else {
        compareModel$df <- rbind(compareModel$df, row.to.df)
      }
    }
  }
  #check to see if current variables are no longer in pickerinput
  df.2.vec <- pull(compareModel$df, "Variable")
  for (var in df.2.vec) {
    if (!(var %in% input$compare_models_select_vars)) {
      idx = match(var, df.2.vec)
      #remove row from RV
      compareModel$df <- compareModel$df[-idx, 1:ncol(compareModel$df)]
    }
  }
})

#create data table that shows the compared models

output$compare_models_DT <- renderRHandsontable({
  
  #select number of columns in the datatable to show
  num.models <- input$model_compare_num_models
  data.to.show <- compareModel$df[, 0:num.models+1]
  
  rhandsontable(data.to.show, stretchH = "all", rowHeaders = NULL) %>%
    hot_col("Variable", readOnly = TRUE)
})

observeEvent(input$compare_models_DT$changes$changes, {
  xi = input$compare_models_DT$changes$changes[[1]][[1]]
  yi = input$compare_models_DT$changes$changes[[1]][[2]]
  old = input$compare_models_DT$changes$changes[[1]][[3]]
  new = input$compare_models_DT$changes$changes[[1]][[4]]
  
  compareModel$df[xi+1, yi+1] <- new
})

# output$compare_models_DT <- renderDT({
#   #select number of columns in the datatable to show
#   num.models <- input$model_compare_num_models
#   data.to.show <- compareModel$df[, 0:num.models+1]
#   
#   DT::datatable(data.to.show,
#                 editable = list(target = "column", disable = list(columns = 0)),
#                 class = "cell-border stripe",
#                 options = list(autoWidth = TRUE,
#                                pageLength = -1,
#                                ordering = FALSE,
#                                dom = 't')
#   )
# })
# 
# proxy_compare_models_DT = dataTableProxy("compare_models_DT")
# 
# observeEvent(input$compare_models_DT_cell_edit, {
#   
#   info = input$compare_models_DT_cell_edit
#   compareModel$df <- editData(compareModel$df, info)
#   replaceData(proxy_compare_models_DT, compareModel$df, resetPaging = FALSE)
# })

# ------------------------------------------------------------------------------

# Solving compared models on button press based on changes in parameters

# ------------------------------------------------------------------------------
observeEvent(input$run_compared_model, {
  w.compare$show()
  # ------------------------------------------------------------------------------
  # Variables that are shared between the compared models
  # ------------------------------------------------------------------------------
  time.in <- as.numeric(input$execute_time_start)
  time.out <- as.numeric(input$execute_time_end)
  time.step <- as.numeric(input$execute_time_step)
  times <- seq(time.in, time.out, by = time.step)
  diff_eqns <- diffeq_to_text(rv.DE$de.eqns, rv.SPECIES$species.names)
  rate_eqns <- CustomEqnsToText(rv.REACTIONS$additional.eqns)
  state <- output_ICs_for_ode_solver(rv.SPECIES$species.names ,ICs$vals)
  d_of_var <- output_var_for_ode_solver(rv.SPECIES$species.names)
  if (input$execute_turnOn_time_scale_var) {
    d_of_var = paste0(input$execute_time_scale_var, "*", d_of_var)
  }
  params.to.change <- pull(compareModel$df, "Variable")
  param.vars <- VectorizeListValue(rv.PARAMETERS$parameters, "Name")
  param.vals <- VectorizeListValue(rv.PARAMETERS$parameters, 
                                   "Value", 
                                   init.mode = "numeric")
  solver <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      eval(parse(text = rate_eqns))
      eval(parse(text = diff_eqns))
      list(eval(parse(text = d_of_var)))
    })
  }
  # ------------------------------------------------------------------------------
  # Variables that are changed based on table values
  # Run Models and Store them to respective RVs
  # ------------------------------------------------------------------------------
  
  # Model 1
  # Find and change parameter values
  new.values <- compareModel$df[,2]  #copy original param tables
  param.vals <- rv.PARAMETERS$parameters.df$Value
  count = 1
  for (var in params.to.change) {
    # find idx matching parameter to change
    idx <- match(var, rv.PARAMETERS$parameters.df$Name) 
    # use above index to change param value for the model
    param.vals[idx] <- new.values[count]
    count = count + 1
  }
  
  parameters <- as.numeric(param.vals)
  names(parameters) <- param.vars
  compareModel$model.1 <- ode(y = state, 
                              times = times, 
                              func = solver, 
                              parms = parameters
  )
  
  # Model 2---------------------------------------------------------------------
  new.values <- compareModel$df[,3]
  param.vals <- rv.PARAMETERS$vals.all
  count = 1
  for (var in params.to.change) {
    # find idx matching parameter to change
    idx <- match(var, param.vars) 
    # use above index to change param value for the model
    param.vals[idx] <- new.values[count]
    count = count + 1
  }
  
  parameters <- output_param_for_ode_solver(param.vars, param.vals)
  compareModel$model.2 <- ode(y = state, 
                              times = times, 
                              func = solver, 
                              parms = parameters
  )
  
  # Model 3---------------------------------------------------------------------
  new.values <- compareModel$df[,4]
  count = 1
  param.vals <- rv.PARAMETERS$vals.all
  for (var in params.to.change) {
    # find idx matching parameter to change
    idx <- match(var, param.vars) 
    # use above index to change param value for the model
    param.vals[idx] <- new.values[count]
    count = count + 1
  }
  
  parameters <- output_param_for_ode_solver(param.vars, param.vals)
  compareModel$model.3 <- ode(y = state, 
                              times = times, 
                              func = solver, 
                              parms = parameters
  )
  
  # Model 4---------------------------------------------------------------------
  new.values <- compareModel$df[,5]
  count = 1
  param.vals <- rv.PARAMETERS$vals.all
  for (var in params.to.change) {
    # find idx matching parameter to change
    idx <- match(var, param.vars) 
    # use above index to change param value for the model
    param.vals[idx] <- new.values[count]
    count = count + 1
  }
  
  parameters <- output_param_for_ode_solver(param.vars, param.vals)
  compareModel$model.4 <- ode(y = state, 
                              times = times, 
                              func = solver, 
                              parms = parameters
  )
  
  w.compare$hide()
})

# ------------------------------------------------------------------------------

# Set up and execute plots for the compared models

# ------------------------------------------------------------------------------

output$Lineplot_Compare <- renderPlot({
  num.col <- as.numeric(input$compare_models_num_col)
  num.row <- as.numeric(input$compare_models_num_row)
  
  #create ggarrange based on number of plots to be made
  if (input$model_compare_num_models == 2) {
    p1 <- plotLineplotInput(gatherData(compareModel$model.1))
    p2 <- plotLineplotInput(gatherData(compareModel$model.2))
    to.plot <- ggarrange(p1, p2, 
                         ncol = num.col,
                         nrow = num.row, 
                         common.legend = TRUE, 
                         legend = "bottom")
  } else if (input$model_compare_num_models == 3) {
    p1 <- plotLineplotInput(gatherData(compareModel$model.1))
    p2 <- plotLineplotInput(gatherData(compareModel$model.2))
    p3 <- plotLineplotInput(gatherData(compareModel$model.3))
    to.plot <- ggarrange(p1, p2, p3,
                         ncol = num.col,
                         nrow = num.row, 
                         common.legend = TRUE, 
                         legend = "bottom")
  } else if (input$model_compare_num_models == 4) {
    p1 <- plotLineplotInput(gatherData(compareModel$model.1))
    p2 <- plotLineplotInput(gatherData(compareModel$model.2))
    p3 <- plotLineplotInput(gatherData(compareModel$model.3))
    p4 <- plotLineplotInput(gatherData(compareModel$model.4))
    to.plot <- ggarrange(p1, p2, p3, p4,
                         ncol = num.col,
                         nrow = num.row, 
                         common.legend = TRUE, 
                         legend = "bottom")
  }

  print(to.plot)
})
