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

build_compare_model_df <- function(selected.vars) {
  empty.df <- data.frame(
    Variable = character(),
    "Model 1" = numeric(),
    "Model 2" = numeric(),
    "Model 3" = numeric(),
    "Model 4" = numeric(),
    check.names = FALSE
  )

  selected.raw <- as.character(unlist(selected.vars, use.names = FALSE))
  selected.raw <- trimws(selected.raw)

  # Pull names + values directly from the parameter list rather than
  # parameters.df — some parameter entries carry vector fields (Used.In, Type)
  # which can make bind_rows drop or nest the Name column.
  params.list <- rv.PARAMETERS$parameters
  if (is.null(params.list) || length(params.list) == 0) {
    cat("[compare_model] parameters list is empty\n")
    return(empty.df)
  }

  param.names <- trimws(vapply(params.list, function(p) {
    nm <- p$Name
    if (is.null(nm) || length(nm) == 0) NA_character_ else as.character(nm)[1]
  }, character(1)))
  param.vals <- vapply(params.list, function(p) {
    v <- p$Value
    if (is.null(v) || length(v) == 0) NA_real_ else suppressWarnings(as.numeric(v)[1])
  }, numeric(1))

  cat("[compare_model] selected.raw = ", paste(selected.raw, collapse = " | "), "\n", sep = "")
  cat("[compare_model] param.names  = ", paste(param.names,  collapse = " | "), "\n", sep = "")

  matched <- intersect(selected.raw, param.names)

  if (length(matched) == 0) {
    cat("[compare_model] no intersection — returning empty df\n")
    return(empty.df)
  }

  idx    <- match(matched, param.names)
  values <- param.vals[idx]

  data.frame(
    Variable = matched,
    "Model 1" = values,
    "Model 2" = values,
    "Model 3" = values,
    "Model 4" = values,
    check.names = FALSE
  )
}

# Initialize compare picker from parameter table names.
# Preserve any current selection so downstream observers don't see the picker
# flip to empty every time rv.PARAMETERS$parameters.df is reassigned.
observeEvent(rv.PARAMETERS$parameters.df, {
  if (is.null(rv.PARAMETERS$parameters.df) || nrow(rv.PARAMETERS$parameters.df) == 0) {
    return(NULL)
  }

  param.names <- as.character(rv.PARAMETERS$parameters.df$Name)
  preserved   <- intersect(as.character(input$compare_models_select_vars), param.names)

  updatePickerInput(session,
                    "compare_models_select_vars",
                    choices = param.names,
                    selected = preserved)
}, ignoreNULL = FALSE)

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
  df = data.frame(
    Variable = character(),
    "Model 1" = numeric(),
    "Model 2" = numeric(),
    "Model 3" = numeric(),
    "Model 4" = numeric(),
    check.names = FALSE
  ),
  no.values = TRUE,
  model.1 = data.frame(),
  model.2 = data.frame(),
  model.3 = data.frame(),
  model.4 = data.frame()
)

# This event builds the compare model datatable with user selected variables
observeEvent(input$compare_models_select_vars, {
  compareModel$df <- build_compare_model_df(input$compare_models_select_vars)
  compareModel$no.values <- nrow(compareModel$df) == 0
  cat("[compare_model] observer wrote compareModel$df: nrow=", nrow(compareModel$df),
      " ncol=", ncol(compareModel$df),
      " cols=", paste(colnames(compareModel$df), collapse = "|"), "\n", sep = "")
}, ignoreNULL = FALSE)

#create data table that shows the compared models
output$compare_models_DT <- renderDT({
  num.models <- suppressWarnings(as.numeric(input$model_compare_num_models))
  if (is.na(num.models) || num.models < 2 || num.models > 4) num.models <- 2

  # Always render the same shape (Variable + N Model columns) so the DT client
  # binding doesn't get stuck between empty and populated states.
  model.cols <- paste0("Model ", seq_len(num.models))
  data.to.show <- data.frame(Variable = character(), check.names = FALSE)
  for (col in model.cols) data.to.show[[col]] <- numeric()

  src <- compareModel$df
  if (!is.null(src) && nrow(src) > 0) {
    keep.cols <- intersect(c("Variable", model.cols), colnames(src))
    data.to.show <- src[, keep.cols, drop = FALSE]
  }

  cat("[compare_model] renderDT fired. nrow=", nrow(data.to.show),
      " ncol=", ncol(data.to.show),
      " num_models=", num.models, "\n", sep = "")

  datatable(
    data.to.show,
    rownames = FALSE,
    editable = list(target = "cell", disable = list(columns = 0)),
    selection = "none",
    class = "cell-border stripe",
    options = list(
      dom = "t",
      paging = FALSE,
      ordering = FALSE,
      searching = FALSE,
      info = FALSE,
      language = list(
        zeroRecords = "Select parameters above to compare.",
        emptyTable  = "Select parameters above to compare."
      )
    )
  )
}, server = FALSE)

observeEvent(input$compare_models_DT_cell_edit, {
  info <- input$compare_models_DT_cell_edit
  xi <- as.integer(info$row)
  yi <- as.integer(info$col) + 1

  # Keep variable names read-only (column 0 in DT, which is column 1 in R)
  if (as.integer(info$col) == 0) {
    return(NULL)
  }

  old <- compareModel$df[xi, yi, drop = TRUE]
  new <- DT::coerceValue(info$value, old)
  compareModel$df[xi, yi] <- new
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
  time.in <- as.numeric(input$plot_execute_time_start)
  time.out <- as.numeric(input$plot_execute_time_end)
  time.step <- as.numeric(input$plot_execute_time_step)
  times <- seq(time.in, time.out, by = time.step)
  diff_eqns <- diffeq_to_text(rv.DE$de.eqns.for.solver, names(rv.SPECIES$species))
  rate_eqns <- CustomEqnsToText(rv.CUSTOM.EQNS$ce.equations)
  state <- output_ICs_for_ode_solver(rv.SPECIES$species)
  d_of_var <- output_var_for_ode_solver(names(rv.SPECIES$species))
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
  new.values <- as.numeric(compareModel$df[,2])  #copy original param tables
  param.vals <- as.numeric(rv.PARAMETERS$parameters.df$Value)
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
  new.values <- as.numeric(compareModel$df[,3])
  param.vals <- as.numeric(rv.PARAMETERS$parameters.df$Value)
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
  compareModel$model.2 <- ode(y = state, 
                              times = times, 
                              func = solver, 
                              parms = parameters
  )
  
  # Model 3---------------------------------------------------------------------
  new.values <- as.numeric(compareModel$df[,4])
  param.vals <- as.numeric(rv.PARAMETERS$parameters.df$Value)
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
  compareModel$model.3 <- ode(y = state, 
                              times = times, 
                              func = solver, 
                              parms = parameters
  )
  
  # Model 4---------------------------------------------------------------------
  new.values <- as.numeric(compareModel$df[,5])
  param.vals <- as.numeric(rv.PARAMETERS$parameters.df$Value)
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

  # Guard rails: require solved model data and selected variables before plotting.
  if (is.null(input$lineplot_yvar) || length(input$lineplot_yvar) == 0) {
    plot(1, 1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    text(1, 1, "Select variables to plot in the Variables dropdown.", cex = 1.2)
    return(invisible(NULL))
  }

  if (nrow(compareModel$model.1) == 0) {
    plot(1, 1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    text(1, 1, "Run compared models to display side-by-side plots.", cex = 1.2)
    return(invisible(NULL))
  }
  
  #create ggarrange based on number of plots to be made
  if (input$model_compare_num_models == 2) {
    p1 <- plotLineplotInput(gatherData(compareModel$model.1, input$lineplot_yvar))
    p2 <- plotLineplotInput(gatherData(compareModel$model.2, input$lineplot_yvar))
    to.plot <- ggarrange(p1, p2, 
                         ncol = num.col,
                         nrow = num.row, 
                         common.legend = TRUE, 
                         legend = "bottom")
  } else if (input$model_compare_num_models == 3) {
    p1 <- plotLineplotInput(gatherData(compareModel$model.1, input$lineplot_yvar))
    p2 <- plotLineplotInput(gatherData(compareModel$model.2, input$lineplot_yvar))
    p3 <- plotLineplotInput(gatherData(compareModel$model.3, input$lineplot_yvar))
    to.plot <- ggarrange(p1, p2, p3,
                         ncol = num.col,
                         nrow = num.row, 
                         common.legend = TRUE, 
                         legend = "bottom")
  } else if (input$model_compare_num_models == 4) {
    p1 <- plotLineplotInput(gatherData(compareModel$model.1, input$lineplot_yvar))
    p2 <- plotLineplotInput(gatherData(compareModel$model.2, input$lineplot_yvar))
    p3 <- plotLineplotInput(gatherData(compareModel$model.3, input$lineplot_yvar))
    p4 <- plotLineplotInput(gatherData(compareModel$model.4, input$lineplot_yvar))
    to.plot <- ggarrange(p1, p2, p3, p4,
                         ncol = num.col,
                         nrow = num.row, 
                         common.legend = TRUE, 
                         legend = "bottom")
  } else {
    plot(1, 1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    text(1, 1, "Choose 2, 3, or 4 models for side-by-side comparison.", cex = 1.2)
    return(invisible(NULL))
  }

  print(to.plot)
})
