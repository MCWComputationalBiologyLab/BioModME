##############################################################################

#LooP

##############################################################################

w.loop <- Waiter$new(id = c("LinePlot_loop", "lineplot_loop_plotly"),
                     html = tagList(
                       spin_loaders(32),
                       h4("Soving Model...")
                     ))
w.loop.store <- Waiter$new(id = c("LinePlot_loop", "lineplot_loop_plotly"),
                           html = tagList(
                             spin_loaders(32),
                             h4("Overwriting Parameters"))
)


# When parameters change, above reactive variables take the value of parms$param.table and ICs$ICs.table

#load parameter table
output$loop_mode_parameters <- renderRHandsontable({
  rhandsontable(rv.PLOT.LOOP$loop.parameters, stretchH = "all") %>%
    hot_col("Parameter", readOnly = TRUE)
})

# Account for changes in parameter table
observeEvent(input$loop_mode_parameters$changes$changes, {
  rv.PLOT.LOOP$loop.parameters <- hot_to_r(input$loop_mode_parameters)
})

#load initial conditions table
output$loop_mode_ICs <- renderRHandsontable({
  rhandsontable(rv.PLOT.LOOP$loop.ICs, stretchH = "all") %>%
    hot_col("Variable", readOnly = TRUE)
})

# account for changes in IC table
observeEvent(input$loop_mode_ICs$changes$changes, {
  rv.PLOT.LOOP$loop.ICs <- hot_to_r(input$loop_mode_ICs)
})

#load plots
output$LinePlot_loop <- renderPlot({
  print(plotLineplotInput(gatherData(rv.PLOT.LOOP$loop.model.results)))
})


output$lineplot_loop_plotly <- renderPlotly(
  ggplotly(plotLineplotInput(gatherData(rv.PLOT.LOOP$loop.model.results)), 
           tooltip = c("x", "y", "colour"))
)


# Update Time panels when they change
observeEvent(input$execute_time_start, {
  updateTextInput(session, "loop_start_time", value = input$execute_time_start)
  rv.PLOT.LOOP$loop.time.start <- input$execute_time_start 
})
observeEvent(input$execute_time_end, {
  updateTextInput(session, "loop_end_time", value = input$execute_time_end)
  rv.PLOT.LOOP$loop.time.end <- input$execute_time_end
})
observeEvent(input$execute_time_step, {
  updateTextInput(session, "loop_time_step", value = input$execute_time_step)
  rv.PLOT.LOOP$loop.time.step <- input$execute_time_step
})
observeEvent(input$loop_start_time, {
  rv.PLOT.LOOP$loop.time.start <- input$loop_start_time
})
observeEvent(input$loop_end_time, {
  rv.PLOT.LOOP$loop.time.end <- input$loop_end_time
})
observeEvent(input$loop_time_step, {
  rv.PLOT.LOOP$loop.time.step <- input$loop_time_step
})


#hook up execute model button
observeEvent(input$loop_mode_execute, {
  w.rv.PLOT.LOOP$loop.show()
  #extract ICs for loop model
  IC.vars <- rv.PLOT.LOOP$loop.ICs[,1]
  IC.vals <- rv.PLOT.LOOP$loop.ICs[,2]

  # Extract parameters for loop model
  param.vars <- rv.PLOT.LOOP$loop.parameters[,1]
  param.vals <- rv.PLOT.LOOP$loop.parameters[,2]
  parameters <- as.numeric(param.vals)
  names(parameters) <- param.vars
  
  #run the model 
  #set up time for solver
  time.in <- as.numeric(rv.PLOT.LOOP$loop.time.start)
  time.out <- as.numeric(rv.PLOT.LOOP$loop.time.end)
  time.step <- as.numeric(rv.PLOT.LOOP$loop.time.step)
  times <- seq(time.in, time.out, by=time.step)
  
  #initialize initial conditions
  state <- output_ICs_for_ode_solver(IC.vars ,IC.vals)
  
  #set up differential equations input string form
  diff_eqns <- diffeq_to_text(rv.DE$de.eqns, rv.SPECIES$species.names)
  d_of_var <- output_var_for_ode_solver(rv.SPECIES$species.names)
  
  Lorenz <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      eval(parse(text=diff_eqns))
      list(eval(parse(text=d_of_var)))
    })
  }
  
  out <- ode(y=state, 
             times=times, 
             func = Lorenz, 
             parms = parameters)
  
  rv.PLOT.LOOP$loop.model.results <- out
  w.rv.PLOT.LOOP$loop.hide()
})

#hook up store variables button
observeEvent(input$loop_mode_store_variables, {
  w.loop.store$show()
  Sys.sleep(0.5)
  # waiter_show(id = "loop_mode_store_variables",
  #             html = div(spin_1(), "Storing Variables"))
  #store time
  updateTextInput(session, "execute_time_start", value = input$loop_start_time)
  updateTextInput(session, "execute_time_end",   value = input$loop_end_time)
  updateTextInput(session, "execute_time_step",  value = input$loop_time_step)
  rv.SOLVER.OPTIONS$time.start <- rv.PLOT.LOOP$loop.time.start
  rv.SOLVER.OPTIONS$time.end   <- rv.PLOT.LOOP$loop.time.end
  rv.SOLVER.OPTIONS$time.step  <- rv.PLOT.LOOP$loop.time.step
    
  #store initial conditions
  ICs$ICs.table <- rv.PLOT.LOOP$loop.ICs
  ICs$vals <- rv.PLOT.LOOP$loop.ICs[,2]
  ICs$comments <- rv.PLOT.LOOP$loop.ICs[,3]
  
  #store parameter
   
  
  #reset parameter table view 
  updatePickerInput(session, "parameters_filter_type", selected = "Eqns")
  updatePickerInput(session, "parameters_filter_type", selected = "All")
  w.loop.store$hide()
  # waiter_hide(id = "loop_mode_store_variables")
})


