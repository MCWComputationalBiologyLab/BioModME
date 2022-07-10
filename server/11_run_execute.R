############################# Run Model Server ###############################
# create our watier
w_execute <- Waiter$new(id = "box3")

#add options for ode server

#add table views for equations, parameters, and ICs
output$execute_equations_show <- renderText({
  paste(DE$eqns, collapse = "<br>")
})

output$execute_parameters_show <- renderText({
  paste(params$vals.all, collapse = "<br>")
})

output$execute_ICS_show <- renderText({
  paste(ICs$vals, collapse = "<br>")
})

# Store relevant model information to be saved for future runs
observeEvent(input$execute_run_model, {
  options$time.start <- input$execute_time_start
  options$time.end <- input$execute_time_end
  options$time.step <- input$execute_time_step
  options$time.scale.bool <- input$execute_turnOn_time_scale_var
  options$time.scale.value <- input$execute_time_scale_var
  options$ode.solver.type <- input$execute_ode_solver_type
})


#need to create an event reactive here that stores the model that is being run
model_output <- eventReactive(input$execute_run_model, {
  # Error Checks for button
  
  w_execute$show()
  #set up time for solver
  
  error.found <- FALSE
  error.message <- "Model failed to solve. "
  error.time <- FALSE

  time_in <- as.numeric(input$execute_time_start)
  time_out <- as.numeric(input$execute_time_end)
  time_step <- as.numeric(input$execute_time_step)
  
  #-----------------------------------------------------------------------------
  # Error Checking for time values
  #-----------------------------------------------------------------------------
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
    times <- seq(time_in, time_out, by = time_step)
    if (length(times) < 10) {
      error.found <- TRUE
      error.message <- paste0(error.message, 
                              "Step size not small enough. 
                              Must have at least 10 units of time. ")
    }
  }

  #-----------------------------------------------------------------------------
  # Error Checking for  missing values from Equation information
  #-----------------------------------------------------------------------------
  #for error checking for parameters and variables we neeed to check that all
  # existing values in the equations exist in var$species and params$vars.all
  # vars.in.eqns <- c()
  # p <- c()
  # eqn.df <- eqns$eqn.info
  # chem.df <- eqns$eqn.chem
  # enz.df <- eqns$eqn.enzyme
  # syn.df <- eqns$eqn.syn
  # 
  # for (row in 1:nrow(eqn.df)) {
  #   
  #   #Gather all variables from eqns
  #   LHS_var <-  str_split(eqn.df[row,3], " ")[[1]]
  #   RHS_var <-  str_split(eqn.df[row,5], " ")[[1]]
  #   enzyme  <-  eqn.df[row,12]
  #   FR      <-  eqn.df[row,14]
  #   RR      <-  eqn.df[row,17]
  #   vars.in.eqns <- c(vars.in.eqns, LHS_var, RHS_var, enzyme, FR, RR)
  #   
  #   #find parameters in eqns
  #   kf   <- enz.df[row,7]
  #   kr   <- enz.df[row,8]
  #   kcat <- enz.df[row,9]
  #   Vmax <- enz.df[row,10]
  #   Km   <- enz.df[row,11]
  #   fr   <- eqn.df[row,15]
  #   rr   <- eqn.df[row,18]
  #   p    <- c(p, kf, kr, kcat, Vmax, Km, fr, rr)
  # }
  # # Remove all duplicates in vectors    
  # vars.in.eqns <- unique(vars.in.eqns)
  # p <- unique(p)
  # # Replace string NA with actual NA
  # vars.in.eqns <- dplyr::na_if(vars.in.eqns, "NA")
  # p <- dplyr::na_if(p, "NA")
  # # Remove NA from vectors
  # vars.in.eqns <- vars.in.eqns[!is.na(vars.in.eqns)]
  # p <- p[!is.na(p)]
  # # check to see if differences exist in lists
  # diff.var <- setdiff(vars.in.eqns, vars$species)
  # diff.p <- setdiff(p, params$vars.all)
  # #Throw error if there are differences
  # if (length(diff.var) != 0) {
  #   error.found <- TRUE
  #   error.message <- paste0(error.message, "The following variables were found to 
  #                           be used in equations but not found in the 
  #                           species list: ", 
  #                           paste0(diff.var, collapse = ","),
  #                           ". ")
  # }
  # if (length(diff.p) != 0) {
  #   error.found <- TRUE
  #   error.message <- paste0(error.message, "The following parameters were found to 
  #                           be used in equations but not found in the parameter
  #                           list: ", 
  #                           paste0(diff.p, collapse = ","),
  #                           ". ")
  # }
  
  #-----------------------------------------------------------------------------
  # Error Checking for  missing values from IO
  #-----------------------------------------------------------------------------
  # vars.r <- c()
  # p    <- c()
  # I.df <- IO$input.info
  # O.df <- IO$output.info
  # #Search Input Dataframe
  # for (row in 1:nrow(I.df)) {
  #   species <-  I.df[row,2]
  #   enz     <-  I.df[row,7]
  #   vars.r  <- c(vars.r, species, enz)
  # 
  #   #find parameters in eqns
  #   RC     <- I.df[row,3]
  #   Vmax   <- I.df[row,5]
  #   kcat   <- I.df[row,6]
  #   p      <- c(p, RC, Vmax, kcat)
  # }
  # # Search Output Dataframe
  # for (row in 1:nrow(O.df)) {
  #   species <-  O.df[row,2]
  #   enz     <-  O.df[row,7]
  #   vars.r  <- c(vars.r, species, enz)
  # 
  #   #find parameters in eqns
  #   RC     <- O.df[row,3]
  #   Vmax   <- O.df[row,5]
  #   kcat   <- O.df[row,6]
  #   p      <- c(p, RC, Vmax, kcat)
  # }
  # vars.r <- dplyr::na_if(unique(vars.r), "NA")
  # p <- dplyr::na_if(unique(p), "NA")
  # diff.var <- setdiff(vars.r[!is.na(vars.r)], vars$species)
  # diff.p <- setdiff(p[!is.na(p)], params$vars.all)
  # #Throw error if there are differences
  # if (length(diff.var) != 0) {
  #   error.found <- TRUE
  #   error.message <- paste0(error.message, "The following variables were found to
  #                           be used in Inputs/Outputs but not found in species list: ",
  #                           paste0(diff.var, collapse = ","),
  #                           ". ")
  # }
  # if (length(diff.p) != 0) {
  #   error.found <- TRUE
  #   error.message <- paste0(error.message, "The following parameters were found
  #                           to be used in Inputs/Outputs but not found in
  #                           parameter list: ",
  #                           paste0(diff.p, collapse = ","),
  #                           ". ")
  # }
  
  #-----------------------------------------------------------------------------
  # Solving model using ODE solver
  #-----------------------------------------------------------------------------
  #initialize parameters
  parameters <- output_param_for_ode_solver(params$vars.all,
                                            params$vals.all)
  #initialize initial conditions
  state <- output_ICs_for_ode_solver(vars$species ,ICs$vals)

  #set up differential equations input string form
  diff_eqns <- diffeq_to_text(DE$eqns, vars$species)

  d_of_var <- output_var_for_ode_solver(vars$species)
  
  rate_eqns <- rateEqns_to_text(eqns$additional.eqns)

  if (input$execute_turnOn_time_scale_var) {
    d_of_var = paste0(input$execute_time_scale_var, "*", d_of_var)
  }

  Lorenz <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      eval(parse(text = rate_eqns))
      eval(parse(text = diff_eqns))
      list(eval(parse(text = d_of_var)))
    })
  }
  jPrint("Before ode solver")
  #out <- ode(y=state, times=times, func=model, parms=parameters)
  
  # if (error.found) {
  #   out <- data.frame()
  #   cat(error.message)
  #   sendSweetAlert(session,
  #                  "Error...",
  #                  text = error.message,
  #                  type = "error")
  #   # session$sendCustomMessage(type = 'testmessage',
  #   #                           message = HTML(paste(error.message, collapse="<br>")))
  # } else {
    # out <- ode(y = state, 
    #            times = times, 
    #            func = Lorenz, 
    #            parms = parameters
    #            #,method = input$execute_ode_solver_type
    # )
    # 
    # jPrint("After ode solver")
    # 
    # 
    # results$model <- out #store model to reactive var
    # results$model.has.been.solved <- TRUE
    # 
    # # Initialize other plotting modes with this model
    # loop$model.results <- out
    # compareModel$model.1 <- out
    # compareModel$model.2 <- out
    # compareModel$model.3 <- out
    # compareModel$model.4 <- out
    # 
    # #this is meant to prepare a previous version of save file that didn't have
    # #these properly done
    # if (is.null(results$is.pp)) results$is.pp = FALSE
    # if (is.null(results$pp.eqns)) results$pp.eqns = vector()
    # if (is.null(results$pp.vars)) results$pp.vars = vector()
    # if (is.null(results$pp.model)) results$pp.model = data.frame()
    # if (is.null(results$pp.eqns.col)) results$pp.eqns.col = vector()
    # jPrint("All this if statements")
    # jPrint(head(out))
  # }
  out <- ode(y = state, 
             times = times, 
             func = Lorenz, 
             parms = parameters
             #,method = input$execute_ode_solver_type
  )
  
  jPrint("After ode solver")
  
  
  results$model <- out #store model to reactive var
  results$model.has.been.solved <- TRUE
  # Initialize other plotting modes with this model
  loop$model.results <- out
  compareModel$model.1 <- out
  compareModel$model.2 <- out
  compareModel$model.3 <- out
  compareModel$model.4 <- out
  #this is meant to prepare a previous version of save file that didn't have
  #these properly done
  if (is.null(results$is.pp)) results$is.pp = FALSE
  if (is.null(results$pp.eqns)) results$pp.eqns = vector()
  if (is.null(results$pp.vars)) results$pp.vars = vector()
  if (is.null(results$pp.model)) results$pp.model = data.frame()
  if (is.null(results$pp.eqns.col)) results$pp.eqns.col = vector()
  w_execute$hide()
  return(out)
})

#hook up table to result of event reactive above
# output$execute_table_for_model <- renderDT({
#   
#   rhandsontable(model_output(),
#                 readOnly = TRUE, 
#                 contextMenu = FALSE,
#                 maxRoxs = 10)
# })

output$download_model_results <- downloadHandler(
  filename = function(){"model_results.csv"},
  content = function(con){
    write.csv(results$model.final, con, row.names = FALSE)
  }
)

output$execute_table_for_model <- DT::renderDataTable({
  req(results$model.has.been.solved)
  m <- results$model.final
  rounded.model <- round(m[1:nrow(m), 1:ncol(m)], digits = 3)
  DT::datatable(rounded.model,
                options = list(autoWidth = TRUE,
                               ordering = FALSE,
                               dom = "ltipr",
                               lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                               pageLength = -1) 
                )
  
})

