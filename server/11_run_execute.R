############################# Run Model Server ###############################

# Waiters ----------------------------------------------------------------------
# create our watier
w_execute <- Waiter$new(id = "box3")

# Functions --------------------------------------------------------------------
ModelFxn <- function(t, 
                     state, 
                     parameters,
                     extraEqns,
                     differentialEqns,
                     vars){
  with(as.list(c(state, parameters)), {
    eval(parse(text = extraEqns))
    eval(parse(text = differentialEqns))
    list(eval(parse(text = vars)))
  })
}

# Update UI, Renders, Animations, etc... ---------------------------------------

observeEvent(input$execute_time_unit, {
  # Store Time Unit RV
  rv.UNITS$units.selected$Duration <- input$execute_time_unit
  
})

observeEvent(rv.UNITS$units.selected$Duration, {
  
  if (rv.UNITS$units.selected$Duration != input$execute_time_unit) {
    updatePickerInput(
      session = session,
      "execute_time_unit",
      selected = rv.UNITS$units.selected$Duration
    )
  }
  
  # if (rv.UNITS$units.selected$Duration != input$GO_base_duration) {
  #   updatePickerInput(
  #     session = session,
  #     "GO_base_duration",
  #     selected = rv.UNITS$units.selected$Duration
  #   )
  # }
})

observeEvent(rv.UNITS$units.selected$Count, {
  
  updatePickerInput(
    session = session, 
    "execute_results_unit",
    selected = rv.UNITS$units.selected$Count
  )
})

# Store Model Options ----------------------------------------------------------
observeEvent(input$execute_run_model, {
  rv.SOLVER.OPTIONS$time.start       <- input$execute_time_start
  rv.SOLVER.OPTIONS$time.end         <- input$execute_time_end
  rv.SOLVER.OPTIONS$time.step        <- input$execute_time_step
  rv.SOLVER.OPTIONS$time.scale.bool  <- input$execute_turnOn_time_scale_var
  rv.SOLVER.OPTIONS$time.scale.value <- input$execute_time_scale_var
  rv.SOLVER.OPTIONS$ode.solver.type  <- input$execute_ode_solver_type
})


# Event: Solve Model -----------------------------------------------------------
model_output <- eventReactive(input$execute_run_model, {
  # browser()
  # Resolve for newest version if differential equations (prob not needed)
  solveForDiffEqs()
  
  # Error Checks for button
  w_execute$show()
  # browser()
  #set up time for solver
  error.found   <- FALSE
  error.message <- "Model failed to solve. "
  error.time    <- FALSE

  time_in   <- as.numeric(input$execute_time_start)
  time_out  <- as.numeric(input$execute_time_end)
  time_step <- as.numeric(input$execute_time_step)
  
  # Error Checking Time Values
  if (is.na(time_in) | is.na(time_out) | is.na(time_step)) {
    error.found <- TRUE
    error.time <- TRUE
    error.message <- paste0(error.message, 
                            "Time values are not numerical values. ")
  } else if (time_out < time_in) {
    error.found <- TRUE
    error.time <- TRUE
    error.message <- paste0(error.message, 
                            "Time out is a lower value than time in. ")
  }
  if (!error.time) {
    converted.time <- FALSE
    times <- seq(time_in, time_out, by = time_step)
    selected.time.unit <- rv.UNITS$units.selected$Duration
    rv.RESULTS$results.time.units <- selected.time.unit
    base.time.unit <- rv.UNITS$units.base$Duration
    if (selected.time.unit != base.time.unit) {
      converted.time <- TRUE
      # Convert it with same number of steps
      conv.time.in <- UnitConversion("time",
                                     selected.time.unit,
                                     base.time.unit,
                                     time_in)
      conv.time.out <- UnitConversion("time",
                                      selected.time.unit,
                                      base.time.unit,
                                      time_out)
      time.breaks <- length(times)
      times <- seq(conv.time.in, conv.time.out, length.out = time.breaks)
    }
    if (length(times) < 10) {
      error.found <- TRUE
      error.message <- paste0(error.message, 
                              "Step size not small enough. 
                              Must have at least 10 units of time. ")
    }
  }
  
  # Preping Terms for ODE Solver
  #initialize parameters
  parameters <- output_param_for_ode_solver(rv.PARAMETERS$parameters)
  
  #initialize initial conditions
  state <- output_ICs_for_ode_solver(rv.SPECIES$species)

  #Extract diffeqs from solver
  diff.eqns.vector <- rv.DE$de.eqns.for.solver
  
  #set up differential equations input string form
  diff_eqns <- diffeq_to_text(diff.eqns.vector, 
                              names(rv.SPECIES$species))

  d_of_var <- output_var_for_ode_solver(names(rv.SPECIES$species))
  
  custom.eqns <- CustomEqnsToText(rv.CUSTOM.EQNS$ce.equations)

  if (input$execute_turnOn_time_scale_var) {
    d_of_var = paste0(input$execute_time_scale_var, "*", d_of_var)
  }

  # print("Before Solver")
  # print(rv.DE)
  # print(diff.eqns.vector)
  # print("into solver")
  # print(parameters)
  # print(state)
  # print(diff_eqns)
  # print(d_of_var)
  
  # Solve ODEs

  out <- ode(y = state, 
             times = times, 
             func =   ModelFxn, 
             parms = parameters,
             extraEqns = custom.eqns,
             differentialEqns = diff_eqns,
             vars = d_of_var
             #,method = input$execute_ode_solver_type
  )
  
  if (converted.time) {
    result.time <- out[,1]
    conv.time.in <- UnitConversion("time",
                                   base.time.unit,
                                   selected.time.unit,
                                   result.time)
    out[,1] <- conv.time.in
  }

  
  # Save Results to Appropriate Places
  rv.RESULTS$results.model <- out #store model to reactive var
  rv.RESULTS$results.model.final <- out
  rv.RESULTS$results.model.has.been.solved <- TRUE
  rv.RESULTS$results.model.units.view <- out
  
  # Generate viewing table 
  # u1 <- units$base.units$Count
  # u2 <- input$execute_results_unit
  # sub.df <- data.frame(rv.RESULTS$results.model[,2:ncol(rv.RESULTS$results.model)])
  # unit.convert <- data.frame(lapply(sub.df, measurements::conv_unit, u1, u2))
  # converted.df <- cbind(rv.RESULTS$results.model[,1], unit.convert)
  # rv.RESULTS$results.model.units.view <- converted.df
  # colnames(rv.RESULTS$results.model.units.view) <- colnames(rv.RESULTS$results.model)
  # if (input$execute_results_unit != units$base.units$Count) {
  #   # Perform conversion on results dataframe
  # 
  #   # Remove first column of df
  #   u1 <- units$base.units$Count
  #   u2 <- input$execute_results_unit
  #   sub.df <- data.frame(out[,2:ncol(out)])
  #   unit.convert <- data.frame(lapply(sub.df, measurements::conv_unit, u1, u2))
  #   converted.df <- cbind(out[1,], unit.convert)
  #   
  #   colnames(converted.df) <- colnames(out)
  #   rv.RESULTS$results.model.units.view <- converted.df
  #   
  # } else {
  #   rv.RESULTS$results.model.units.view <- out
  # }
  
  
  # Initialize other plotting modes with this model
  # rv.PLOT.LOOP$loop.model.results <- out
  # compareModel$model.1 <- out
  # compareModel$model.2 <- out
  # compareModel$model.3 <- out
  # compareModel$model.4 <- out
  #this is meant to prepare a previous version of save file that didn't have
  #these properly done
  if (is.null(rv.RESULTS$results.is.pp)) rv.RESULTS$results.is.pp = FALSE
  if (is.null(rv.RESULTS$results.pp.eqns)) rv.RESULTS$results.pp.eqns = vector()
  if (is.null(rv.RESULTS$results.pp.vars)) rv.RESULTS$results.pp.vars = vector()
  if (is.null(rv.RESULTS$results.pp.model)) rv.RESULTS$results.pp.model = data.frame()
  if (is.null(rv.RESULTS$results.pp.eqns.col)) rv.RESULTS$results.pp.eqns.col = vector()
  
  w_execute$hide()
  
  return(out)
})

observeEvent(input$execute_results_unit, {
  req(rv.RESULTS$results.model.has.been.solved)
  
  if (input$execute_results_unit != rv.UNITS$units.base$Count) {
    # Perform conversion on results dataframe
    # Remove first column of df
    u1 <- rv.UNITS$units.base$Count
    u2 <- input$execute_results_unit
    
    sub.df <- 
      data.frame(rv.RESULTS$results.model[,2:ncol(rv.RESULTS$results.model)])
    
    unit.convert <- data.frame(lapply(sub.df, measurements::conv_unit, u1, u2))
    converted.df <- cbind(rv.RESULTS$results.model[,1], unit.convert)
    rv.RESULTS$results.model.units.view <- converted.df
    
    colnames(rv.RESULTS$results.model.units.view) <- 
      colnames(rv.RESULTS$results.model)
  } else {
    rv.RESULTS$results.model.units.view <- rv.RESULTS$results.model
  }
})

# DT Table Button Addons  ------------------------------------------------------

callModule(tableDownloadButtons,
           "module_execute_buttons",
           rv.RESULTS$results.model.final)

# Results Table Render ---------------------------------------------------------
output$execute_table_for_model <- DT::renderDataTable({
  req(rv.RESULTS$results.model.has.been.solved)
  m <- rv.RESULTS$results.model.units.view
  
  # print(head(m))
  isolate(time_step <- input$execute_time_step)
  
  # m <- custom_round_df(m,
  #                      ignore_first_col = TRUE)
  
  to_sci <- input$execute_view_scientific_notation
  ignore_first_col <- FALSE
  first_col_digits <- get_decimal_places(time_step)
  all_sci <- FALSE
  ignore_rounding <- input$execute_view_round_values
  
  if (to_sci) {
    if (input$PI_execute_sci_not_options == "ALL") {
      all_sci <- TRUE
    } 
  } 
  
  if (input$execute_view_round_values) {
    ignore_rounding <- FALSE
    if (input$CBI_execute_first_col_use_time) {
      first_col_digits <- get_decimal_places(time_step)
    } else {
      first_col_digits <- input$execute_view_round_digits_first_col
    }
  } else {
    ignore_rounding <- TRUE
  }

  m <-
    custom_round_df(
      df = as.data.frame(m),
      digits = input$execute_view_round_digits,
      zero_as_plain = TRUE,
      to_sci = to_sci,
      ignore_first_col = ignore_first_col,
      first_col_digits = first_col_digits,
      all_sci = all_sci,
      ignore_rounding = ignore_rounding
      )
  
  time.w.units <- paste0("Time (", rv.RESULTS$results.time.units, ")")
  # time.w.units <- "time (min)"
  colnames(m)[1] <- time.w.units

  DT::datatable(m,
                options = list(
                  autoWidth = TRUE,
                  ordering = FALSE,
                  dom = "ltipr",
                  columnDefs = list(list(className = "dt-center",
                                         targets = "_all")),
                  lengthMenu = list(c(5, 15, 100, -1),
                                    c('5', '15', "100", 'All')),
                  pageLength = 100)
                )
})
