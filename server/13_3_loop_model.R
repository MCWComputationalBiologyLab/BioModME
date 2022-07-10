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
# Reactive variables to store loop model variables
loop <- reactiveValues(
  parameters = data.frame(matrix(ncol=3,
                                 nrow=0,
                                 dimnames = list(NULL, c("Parameter",
                                                         "Value",
                                                         "Description")))),
  ICs = data.frame(matrix(ncol = 3,
                          nrow = 0,
                          dimnames = list(NULL, c("Variable"
                                                  ,"Value"
                                                  ,"Description")))),
  time.start = 0,
  time.end = 100, 
  time.step = 1,
  model.results = data.frame()
)

# When parameters change, above reactive variables take the value of parms$param.table and ICs$ICs.table

#load parameter table
output$loop_mode_parameters <- renderRHandsontable({
  rhandsontable(loop$parameters, stretchH = "all") %>%
    hot_col("Parameter", readOnly = TRUE)
})

# Account for changes in parameter table
observeEvent(input$loop_mode_parameters$changes$changes, {
  loop$parameters <- hot_to_r(input$loop_mode_parameters)
})

#load initial conditions table
output$loop_mode_ICs <- renderRHandsontable({
  rhandsontable(loop$ICs, stretchH = "all") %>%
    hot_col("Variable", readOnly = TRUE)
})

# account for changes in IC table
observeEvent(input$loop_mode_ICs$changes$changes, {
  loop$ICs <- hot_to_r(input$loop_mode_ICs)
})

#load plots
output$LinePlot_loop <- renderPlot({
  print(plotLineplotInput(gatherData(loop$model.results)))
})


output$lineplot_loop_plotly <- renderPlotly(
  ggplotly(plotLineplotInput(gatherData(loop$model.results)), 
           tooltip = c("x", "y", "colour"))
)


# Update Time panels when they change
observeEvent(input$execute_time_start, {
  updateTextInput(session, "loop_start_time", value = input$execute_time_start)
  loop$time.start <- input$execute_time_start 
})
observeEvent(input$execute_time_end, {
  updateTextInput(session, "loop_end_time", value = input$execute_time_end)
  loop$time.end <- input$execute_time_end
})
observeEvent(input$execute_time_step, {
  updateTextInput(session, "loop_time_step", value = input$execute_time_step)
  loop$time.step <- input$execute_time_step
})
observeEvent(input$loop_start_time, {
  loop$time.start <- input$loop_start_time
})
observeEvent(input$loop_end_time, {
  loop$time.end <- input$loop_end_time
})
observeEvent(input$loop_time_step, {
  loop$time.step <- input$loop_time_step
})


#hook up execute model button
observeEvent(input$loop_mode_execute, {
  w.loop$show()
  #extract ICs for loop model
  IC.vars <- loop$ICs[,1]
  IC.vals <- loop$ICs[,2]

  # Extract parameters for loop model
  param.vars <- loop$parameters[,1]
  param.vals <- loop$parameters[,2]

  #run the model 
  #set up time for solver
  time.in <- as.numeric(loop$time.start)
  time.out <- as.numeric(loop$time.end)
  time.step <- as.numeric(loop$time.step)
  times <- seq(time.in, time.out, by=time.step)
  
  #initialize parameters
  parameters <- output_param_for_ode_solver(param.vars, param.vals)
  
  #initialize initial conditions
  state <- output_ICs_for_ode_solver(IC.vars ,IC.vals)
  
  #set up differential equations input string form
  diff_eqns <- diffeq_to_text(DE$eqns, vars$species)
  d_of_var <- output_var_for_ode_solver(vars$species)
  
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
  
  loop$model.results <- out
  w.loop$hide()
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
  options$time.start <- loop$time.start
  options$time.end   <- loop$time.end
  options$time.step  <- loop$time.step
    
  #store initial conditions
  ICs$ICs.table <- loop$ICs
  ICs$vals <- loop$ICs[,2]
  ICs$comments <- loop$ICs[,3]
  
  #store parameter
  params$vars.all <- loop$parameters[,1]
  params$vals.all <- loop$parameters[,2]
  params$comments.all <- loop$parameters[,3]
  params$param.table <- loop$parameters
  #reset parameter table view 
  updatePickerInput(session, "parameters_filter_type", selected = "Eqns")
  updatePickerInput(session, "parameters_filter_type", selected = "All")
  w.loop.store$hide()
  # waiter_hide(id = "loop_mode_store_variables")
})


