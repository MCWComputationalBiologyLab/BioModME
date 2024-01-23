# We need to create an objective function that read in our ode solution on the parameters
ssd_objective <- function(par.to.estimate, 
                          par.in.model,
                          ics.in.model,
                          var.in.model,
                          time,
                          rateEqns,
                          diffEqns,
                          d_of_var,
                          observed.data) {
  # Calculates the residuals of our model with comparison data uploaded by the
  # user
  # Inputs:
  # (1) par.to.estimate - list of parameters we want to estimate
  # (2) par.in.model - list of all parameters in model
  # (3) ics.in.model - list of all initial conditions and values in model
  # (4) var.in.model - vector of variables in model
  # (5) time - time vector that the model is ran for
  # (6) observed.data - data frame containing time in first column and observed
  #                     data in the subsequent columns. Column names should 
  #                     match variables in model
  # Outputs: 
  # (1) out - vector of residuals to be analyzed with a minimization function
  
  # Unpack parameters 
  par.to.run <- listReplace(par.to.estimate, par.in.model)
  
  # Rework time variable to include time points in obv data
  times <- time
  tvs   <- sort(unique(c(times, as.numeric(unlist(observed.data[,1])))))
  
  # Run ODE solver to get concentration values
  Lorenz <- function(t, state, parameters, rateEqns, diffEqns, d_of_var){
    with(as.list(c(state, parameters)), {
      eval(parse(text = rateEqns))
      eval(parse(text = diffEqns))
      list(eval(parse(text = d_of_var)))
    })
  }
  out <- ode(y = ics.in.model, 
             times = tvs, 
             func = myModel, 
             parms = par.to.run,
             rateEqns = rateEqns,
             diffEqns = diffEqns,
             d_of_var = d_of_var)
  
  # Remove time points from simulated data to match observed
  df <- data.frame(out)
  df <- df[df[,1] %in% as.numeric(unlist(observed.data[,1])),]
  
  # Get columns for observed data (remove time column)
  # df.expected <- observed.data[,-1]
  df.expected <- observed.data %>% select(-1)
  expected.names <- colnames(df.expected)
  
  # Remove excess rows and columns from predicted data
  # df.pred <- df[,expected.names]
  df.pred <- df %>% select(all_of(expected.names))
  # Evaluate predicted data - expected
  ssqres <- (df.pred - df.expected)
  # Sum all terms
  out <- unname(unlist(ssqres))
  return(out)
}


listReplace <- function(list1, list2) {
  # Takes the values in list1, finds them in list2, and replaces them with list1
  # values.  Returns the list with replace values.
  # Inputs:
  #   (1) list1 - list of variables with values wanting to replace
  #   (2) list2 - list of variables to replace with list1 vars
  # Outputs:
  #   (1) out - list2 with replaced list1 values.
  
  list1.names <- names(list1)
  list2.names <- names(list2)
  
  for (i in seq_along(list1.names)) {
    if (list1.names[i] %in% list2.names) {
      eval(parse(text=paste0("list2[['", list1.names[i], "']] <- list1[[i]]"))) 
    }
  }
  
  return(list2)
}


myModel <- function (t, 
                     state,
                     parameters, 
                     rateEqns,
                     diffEqns, 
                     d_of_var) {
  
    with(as.list(c(state, parameters)), {
      eval(parse(text = rateEqns))
      eval(parse(text = diffEqns))
      list(eval(parse(text = d_of_var)))
    })

}
# Begin main server functions for parameter estimation.

# Create waiter for calculations
w.pe <- Waiter$new(html =  tagList(
                     div(
                       style = "color:black",
                       spin_whirly(),
                       hr(),
                       h4("Performing Parameter Estimation...")
                     )
                   ),
                   color = transparent(0.7))



# Read imported data -----------------------------------------------------------
data.for.estimation <- reactive({
  # req(input$pe_obs_data)
  if (is.null(input$pe_obs_data)) {
    return(NULL)
  }
  
  #fread(input$data$datapath, na.strings=c("", NA))
  if(endsWith(input$pe_obs_data$datapath, ".csv")){
     out <- read.csv(input$pe_obs_data$datapath)
  } else if(endsWith(input$pe_obs_data$datapath, ".txt")){
    out <- read.table(input$pe_obs_data$datapath,header = T)
  }else if(endsWith(input$pe_obs_data$datapath, ".xls")){
    out <- read_excel(input$pe_obs_data$datapath)
  } else if(endsWith(input$pe_obs_data$datapath, ".xlsx")){
    out <- read_xlsx(input$pe_obs_data$datapath,sheet=1)
  }
  
  species <- colnames(out)[-1]
  rv.PAR.ESTIMATION$pe.loaded.species <- species
  
  return(out)
})

# Fill pickerinput with parameters to estimate options -------------------------
observeEvent(rv.PARAMETERS$parameters.names, {
  updatePickerInput(session = session,
                    "pe_select_par",
                    choices = rv.PARAMETERS$parameters.names)
})

# Function to update PE RV for selected parameters -----------------------------
observeEvent(input$pe_select_par, {
  # This event occurs when the "Parameters to Estimate:" pickerinput changes
  # and more parameters are added to be estimated. It serves as a stoarge 
  # transfer, adding/removing changes to the parameters that will be reflected
  # in the generated parameter table.
  
  pars <- input$pe_select_par
  
  # Remove vars from RV that are no longer selected
  idx.to.remove <- c()
  for (i in seq_along(rv.PAR.ESTIMATION$pe.parameters)) {
    if (!(rv.PAR.ESTIMATION$pe.parameters[i] %in% pars)) {
      idx.to.remove <- c(idx.to.remove, i)
    }
  }
  
  if (length(idx.to.remove > 0)) {
    rv.PAR.ESTIMATION$pe.parameters <- 
      rv.PAR.ESTIMATION$pe.parameters[-idx.to.remove]
    
    rv.PAR.ESTIMATION$pe.initial.guess <- 
      rv.PAR.ESTIMATION$pe.initial.guess[-idx.to.remove]
    
    rv.PAR.ESTIMATION$pe.lb <- rv.PAR.ESTIMATION$pe.lb[-idx.to.remove]
    rv.PAR.ESTIMATION$pe.ub <- rv.PAR.ESTIMATION$pe.ub[-idx.to.remove]
    
    rv.PAR.ESTIMATION$pe.calculated.values <- 
      rv.PAR.ESTIMATION$pe.calculated.values[-idx.to.remove]
  }
  
  # Add parameters that are not in RV
  for (x in pars) {
    if (!(x %in% rv.PAR.ESTIMATION$pe.parameters)) {
      rv.PAR.ESTIMATION$pe.parameters <- c(rv.PAR.ESTIMATION$pe.parameters, x)
      
      rv.PAR.ESTIMATION$pe.initial.guess <- 
        c(rv.PAR.ESTIMATION$pe.initial.guess, 1)
      
      rv.PAR.ESTIMATION$pe.lb <- c(rv.PAR.ESTIMATION$pe.lb, -Inf)
      rv.PAR.ESTIMATION$pe.ub <- c(rv.PAR.ESTIMATION$pe.ub, Inf)
      
      rv.PAR.ESTIMATION$pe.calculated.values <- 
        c(rv.PAR.ESTIMATION$pe.calculated.values, "-")
    }
  }
})

# Generate Rhandsontable for parameters to estimate ----------------------------
output$pe_parameter_value_table <- renderRHandsontable({
  print("PE table gen")
  # Make first column uneditable
  # Make second column needs to be numeric between certain values
  # Bound columns need to be same as last with Infs
  # Make Calculated Column uneditable
  
  # Create df from parameter estimation (pe) RV
  df <- data.frame(rv.PAR.ESTIMATION$pe.parameters,
                   rv.PAR.ESTIMATION$pe.initial.guess,
                   rv.PAR.ESTIMATION$pe.lb,
                   rv.PAR.ESTIMATION$pe.ub,
                   rv.PAR.ESTIMATION$pe.calculated.values)
  colnames(df) <- c("Parameters", 
                    "Initial Guess",
                    "Lower Bound",
                    "Upper Bound", 
                    "Calculated")
  
  rhandsontable(df) %>%
    hot_col(col = c(1, 5), readOnly = TRUE) %>%
    hot_validate_numeric(cols = 2, min = 0) %>%
    hot_validate_numeric(cols = c(3,4))
})

# Edit and save changed to pe parameter value table (rhandontable) -------------
observeEvent(input$pe_parameter_value_table$changes$changes, {

  # xi, yi are table coordinates (remember js starts at 0, so we add 1 for R)
  # xi is 
  # old, new are the old value in that cell and the new value in the cell
  xi  <- input$pe_parameter_value_table$changes$changes[[1]][[1]] + 1
  yi  <- input$pe_parameter_value_table$changes$changes[[1]][[2]] + 1
  old <- input$pe_parameter_value_table$changes$changes[[1]][[3]]
  new <- input$pe_parameter_value_table$changes$changes[[1]][[4]] 

  # Change effect based on which row is changed (remember js starts at 0)
  
  if (yi == 2) {
    # Store initial guess
    rv.PAR.ESTIMATION$pe.initial.guess[xi] <- new
  } else if (yi == 3) {
    # Store lower bound
    rv.PAR.ESTIMATION$pe.lb[xi] <- new
    # if (is.numeric(new)) {
    #   if (new == "-Inf" | new == "Inf") {new = -Inf}
    #   rv.PAR.ESTIMATION$pe.lb[xi] <- new
    # } else {
    #   rv.PAR.ESTIMATION$pe.lb[xi] <- old
    # }
    
  } else if (yi == 4) {
    # Store upper bound 
    rv.PAR.ESTIMATION$pe.ub[xi] <- new
    # if (is.numeric(new)) {
    #   rv.PAR.ESTIMATION$pe.ub[xi] <- new 
    # } else {
    #   rv.PAR.ESTIMATION$pe.ub[xi] <- old
    # }
  }
})

# Turn on/off rhandsontable ----------------------------------------------------
observe({
  n <- length(input$pe_select_par)
  if (n == 0) {
    # hide table
    shinyjs::hide("pe_parameter_value_table")
  } else {
    # show table
    shinyjs::show("pe_parameter_value_table")
  }
})
# Parameter Estimate Output Table ----------------------------------------------
output$pe_import_data_table <- renderRHandsontable({
  
  rows.in.table <- nrow(data.for.estimation())
  
  # Set table message if no data loaded
  if (rows.in.table == 0) {
  } else {
    # Load parameter table with appropriate parameters
    rhandsontable(data.for.estimation(),
                  readOnly = TRUE)
  }

})

# Show Logs For PE Run ---------------------------------------------------------
output$pe_logs <- renderPrint({
  return(rv.PAR.ESTIMATION$pe.log.of.run)
})

# Run parameter estimation when button is pressed ------------------------------
observeEvent(input$pe_run_parameter_estimation, {
  # browser()
  w.pe$show()

  # Resolve for DiffEqs just to avoid any nonsense errors
  solveForDiffEqs()
  
  # Perform Basic Check that Data is observed
  data  <- data.for.estimation()
  if (is.null(data)) {
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = "No data loaded for estimation.",
      type = "error"
    )
    w.pe$hide()
    return(NULL)
  }
  
  # browser()
  error.result <- tryCatch({
    # Grab information needed for parameter estimation
    time_in   <- as.numeric(input$execute_time_start)
    time_out  <- as.numeric(input$execute_time_end)
    time_step <- as.numeric(input$execute_time_step)
    times     <- seq(time_in, time_out, by = time_step)

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
    
    rate_eqns <- CustomEqnsToText(rv.REACTIONS$additional.eqns)
    
    if (input$execute_turnOn_time_scale_var) {
      d_of_var = paste0(input$execute_time_scale_var, "*", d_of_var)
    }
    
    # Perform parameter estimation
    #   -- Grab parameters from data upload
    pars <- rv.PAR.ESTIMATION$pe.parameters
    vals <- rv.PAR.ESTIMATION$pe.initial.guess
    p.0 <- as.numeric(vals)
    names(p.0) <- pars
    
    lower <- rv.PAR.ESTIMATION$pe.lb
    upper <- rv.PAR.ESTIMATION$pe.ub
    txt <- capture.output(nls.out <- nls.lm(par = p.0,
                                            lower = lower,
                                            upper = upper,
                                            fn = ssd_objective,
                                            par.in.model = parameters,
                                            ics.in.model = state,
                                            var.in.model = names(state),
                                            time = times,
                                            rateEqns = rate_eqns,
                                            diffEqns = diff_eqns,
                                            d_of_var = d_of_var,
                                            observed.data = data,
                                            control = nls.lm.control(nprint=1)),
                          type = "output")
    print(txt)
    rv.PAR.ESTIMATION$pe.log.of.run <- ""
    rv.PAR.ESTIMATION$pe.log.of.run <- txt
    #  --Run ssd objective
    # withConsoleRedirect("pe_logs", {
    #   nls.out <- nls.lm(par = p.0,
    #                     lower = lower,
    #                     upper = upper,
    #                     fn = ssd_objective,
    #                     par.in.model = parameters,
    #                     ics.in.model = state,
    #                     var.in.model = names(state),
    #                     time = times,
    #                     rateEqns = rate_eqns,
    #                     diffEqns = diff_eqns,
    #                     d_of_var = d_of_var,
    #                     observed.data = data,
    #                     control = nls.lm.control(nprint=1))
    # })
    # Store estimation data to its respective place
    new.pars <- rv.PAR.ESTIMATION$pe.parameters
    for (i in seq_along(pars)) {
      # rv.PAR.ESTIMATION$pe.parameters[[i]] <- unname(unlist(nls.out$par[i]))
      new.pars[[eval(parse(text="pars[i]"))]] <- as.numeric(
        unname(unlist(nls.out$par[i])))
      # new.pars[[i]] <- unname(unlist(nls.out$par[i]))
      rv.PAR.ESTIMATION$pe.calculated.values[i] <- 
        as.numeric(unname(unlist(nls.out$par[i])))
    }
    new.pars <- listReplace(new.pars, parameters)
    # for (i in seq_along(new.pars)) {
    #   new.pars[[i]] <- as.numeric(new.pars[[i]])
    # }
    new.pars <- as.numeric(new.pars)
    names(new.pars) <- names(parameters)
    
    # Rerun 
    out <- ode(y = state, 
               times = times, 
               func = myModel, 
               parms = new.pars,
               rateEqns = rate_eqns,
               diffEqns = diff_eqns,
               d_of_var = d_of_var)
    
    
    # Pass information to graph in some way
    rv.PAR.ESTIMATION$pe.solved.model <- out
    rv.PAR.ESTIMATION$pe.successful.run <- TRUE
  }, error = function(err) {
    is.error <- TRUE
    # print("An error has occured")
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = err,
      type = "error"
    )
  }, warning = function(w) {
    is.error <- TRUE
    print("A warning is taking place")
    sendSweetAlert(
      session = session,
      title = "Error...",
      text = w,
      type = "warning"
    )
    
  }, finally = {
    w.pe$hide()
  }
  )
  
})

# Result Plot for Parameter Estimation -----------------------------------------
output$pe_parameter_estimation_plot <- renderPlot({
  
  # browser()
  # Observed enter data
  data <- data.for.estimation()
  colnames(data)[1] <- "time"
  
  data.m <- reshape2::melt(data, id.vars="time")
  
  # df <- data.frame(t,A,B,P)
  # df.m <- reshape2::melt(df, id.vars="t")
  
  p <- ggplot(NULL, aes(col=variable)) +
    #geom_line(data = df.m, aes(t, value)) +
    geom_point(data = data.m, 
               aes(time, value),
               size = 3.5)
  if (rv.PAR.ESTIMATION$pe.successful.run) {
    # Pull Results from data
    to.pull <- rv.PAR.ESTIMATION$pe.loaded.species
    
    df <- data.frame(rv.PAR.ESTIMATION$pe.solved.model)
    to.plot <- df[c("time", to.pull)]

    df.m <- reshape2::melt(to.plot, id.vars="time")
    p <- p + geom_line(data = df.m, 
                       aes(time, value),
                       size = 2)
  }
  
  p <- p + theme_classic()
  
  return(p)
  
})

# Store Estimated Parameters As Main Parameters --------------------------------
observeEvent(input$pe_store_estimated_parameters, {
  # Find calculated parameters and their values
  new.pars <- rv.PAR.ESTIMATION$pe.parameters
  new.vals <- rv.PAR.ESTIMATION$pe.calculated.values
  
  # For each par, id lookup.
  for (i in seq_along(new.pars)) {
    par.id <- FindId(new.pars[i])
    rv.PARAMETERS$parameters[[par.id]]$Value <- as.numeric(new.vals[i])
    
    # Perform base unit calculations if necessary
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
    
    # Check if parameter is compartment volume and change in respective tables
    if (rv.PARAMETERS$parameters[[par.id]]$Type == "Compartment") {
      # Find which compartment has this volume
      vol.name    <- rv.PARAMETERS$parameters[[par.id]]$Name
      par.val     <- rv.PARAMETERS$parameters[[par.id]]$Value 
      par.unit    <- rv.PARAMETERS$parameters[[par.id]]$Unit 
      par.baseval <- rv.PARAMETERS$parameters[[par.id]]$BaseValue
      
      for (i in seq(length(rv.COMPARTMENTS$compartments))) {
        if (rv.COMPARTMENTS$compartments[[i]]$Volume == vol.name) {
          rv.COMPARTMENTS$compartments[[i]]$Value     <- par.val
          rv.COMPARTMENTS$compartments[[i]]$Unit      <- par.unit
          rv.COMPARTMENTS$compartments[[i]]$BaseValue <- par.baseval
          break
        }
      }
    }
  }

  print("Store PE was pressed")
  # Shiny alert of succesful transfer
  sendSweetAlert(
    session = session,
    title = "Success !!",
    text = "Parameters Overwritten",
    type = "success"
  )
  # Rerun model with new parameters?
})


output$uiOut_pe_precheck_error_message <- renderUI({

    # Check to see if combo file exists/not null
    if (is.null(data.for.estimation())) {
      tags$h6(
        HTML(
          paste0(
            "<font color=\"#FF0000\">",
            "Warning: No data imported for estimation.",
            "</font>"
          )
        )
      )
    }

})
