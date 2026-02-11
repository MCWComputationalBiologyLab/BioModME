
checkForLoadedValue <- function(loadedValue, initValue) {
  #this function is meant to perform checks on if loaded value is null
  #between versions there are often things that get added that save as null.  
  #these checks are meant to stop breakdown between old models and new app versions
  # Inputs:
  #  @loadedValue - the value loaded from the model
  #  @initi value - what the initialzied value should be if it is null
  if (is.null(loadedValue)) {
    out <- initValue
  } else {
    out <- loadedValue
  }
  return(out)
}
################################ Load Server #################################


w_load <- Waiter$new(
  html =  spin_pong(),
  color = transparent(0.9)
)

#when load model button is pressed, the .rds file is loaded in and its components are broken apart and added to the model
#some of these loads for reactive variables use a "is.null" check to make sure they exist.  These variables were added
# after specific models were made and this adds functionality to those models that would otherwise have issues.
observeEvent(input$load_model, {
  #w_load$show()
  waiter_show(html = waiting_screen)
  Sys.sleep(1)
  model.load <- readRDS(input$load_model$datapath)
  
  #-----------------------------------------------------------------------------
  
  # Load Variable Section
  
  #-----------------------------------------------------------------------------
  vars$species <- model.load$species
  ifelse(!is.null(model.load$descriptions), 
         vars$descriptions <- model.load$descriptions, 
         vars$descriptions <- vector())
  if (!is.null(model.load$table)) {
    vars$table <- model.load$table
  } else {
    vars$table <- data.frame(vars$species, vars$descriptions)
    colnames(vars$table) <- c("Variable Name", "Description")
  }
  #vars$table <- ifelse(exists(model.load$table), model.load$table, )
  #-----------------------------------------------------------------------------
  
  # Load Equations Section
  
  #-----------------------------------------------------------------------------
  eqns$main           <- model.load$main
  eqns$eqn.main.latex <- model.load$eqn.main.latex
  if (!is.null(model.load$eqn.descriptions)) {
    eqns$eqn.descriptions <- model.load$eqn.descriptions
  } else {
    eqns$eqn.descriptions <- rep("", each = model.load$n.eqns)
  }
  eqns$n.eqns.no.del   <- checkForLoadedValue(model.load$n.eqns.no.del, 0)
  eqns$n.eqns          <- length(model.load$main) #number of equations in model (not including rates)
  eqns$n.eqns.chem     <- checkForLoadedValue(model.load$n.eqns.chem, 0)
  eqns$n.eqns.enz      <- checkForLoadedValue(model.load$n.eqns.enz, 0)
  eqns$n.eqns.syn      <- checkForLoadedValue(model.load$n.eqns.syn, 0)
  eqns$n.eqns.deg      <- checkForLoadedValue(model.load$n.eqns.deg, 0)
  eqns$rate.eqns       <- model.load$rate.eqns #load rate equations
  eqns$time.dep.eqns   <- model.load$time.dep.eqns #load all time dependent eqns
  eqns$additional.eqns <- model.load$additional.eqns #load all additional eqns -time, rate, etc...
  eqns$first.run       <- model.load$first.run
  eqns$eqn.info        <- model.load$eqn.info
  eqns$eqn.chem        <- model.load$eqn.chem
  eqns$eqn.enzyme      <- model.load$eqn.enzyme
  eqns$eqn.syn         <- model.load$eqn.syn
  eqns$eqn.deg         <- model.load$eqn.deg
  
  #-----------------------------------------------------------------------------
  
  # Load Parameter Section
  
  #-----------------------------------------------------------------------------
  #load total parameters from eqns, inputs, outputs (sum of vectors)
  params$vars.all <- model.load$vars.all
  params$vals.all <- model.load$vals.all
  params$comments.all <- model.load$comments.all

  # if (!is.null(model.load$param.table)) {
  #   params$param.table <- model.load$param.table
  # } else {
  #   params$param.table <- data.frame(params$vars.all, params$vals.all, params$comments.all)
  #   colnames(params$param.table) <- c("Parameter", "Value", "Description")
  # }
  params$param.table <- data.frame(params$vars.all, params$vals.all, params$comments.all)
  colnames(params$param.table) <- c("Parameter", "Value", "Description")
  #load parameters from equations
  params$eqns.vars = model.load$eqns.vars
  params$eqns.vals = model.load$eqns.vals
  params$eqns.comments = model.load$eqns.comments
  params$first.param.eqn.stored = model.load$first.param.eqn.stored
  #load parameters for input variables
  params$inputs.vars = model.load$inputs.vars
  params$inputs.vals = model.load$inputs.vals
  params$inputs.comments = model.load$inputs.comments
  params$first.inputs.stored = model.load$first.inputs.stored
  #load parameters for output variables
  params$outputs.vars = model.load$outputs.vars
  params$outputs.vals = model.load$outputs.vals
  params$outputs.comments = model.load$outputs.comments
  params$first.outputs.stored = model.load$first.outputs.stored
  #load parameters from rate variables
  params$rate.eqn.vars = model.load$rate.eqn.vars
  params$rate.eqn.vals = model.load$rate.eqn.vals
  params$rate.eqn.comments = model.load$rate.eqn.comments
  params$first.rate.eqn.stored = model.load$first.rate.eqn.stored
  params$rate.params = model.load$rate.params
  #load parameterts from time dependent equations
  params$time.dep.vars = model.load$time.dep.vars
  params$time.dep.values = model.load$time.dep.values
  params$time.dep.comments = model.load$time.dep.comments
  params$first.time.dep.stored = model.load$first.time.dep.stored
  params$parameters.based.on.other.values <- model.load$parameters.based.on.other.values #list of parameters used in rate equations on LHS
  
  #-----------------------------------------------------------------------------
  
  # Load Initial Condition Section
  
  #-----------------------------------------------------------------------------
  #load initial condition variables
  ICs$vals <- model.load$vals
  ICs$comments <- model.load$comments
  if (!is.null(model.load$ICs.table)) {
    ICs$ICs.table <- data.frame(vars$species, ICs$vals, ICs$comments)
    colnames(ICs$ICs.table) <- c("Variable", "Value", "Description")
  } else {
    ICs$ICs.table <- data.frame(vars$species, ICs$vals, ICs$comments)
    colnames(ICs$ICs.table) <- c("Variable", "Value", "Description")
  }
  ICs$first.IC.stored <- model.load$first.IC.stored
  #load other items
  
  #-----------------------------------------------------------------------------
  
  # Load Differential Equations Section
  
  #-----------------------------------------------------------------------------
  DE$eqns              <- checkForLoadedValue(model.load$eqns, vector()) 
  DE$eqn.in.latex      <- checkForLoadedValue(model.load$eqn.in.latex, vector())
  DE$custom.diffeq.var <- model.load$custom.diffeq.var
  DE$custom.diffeq     <- model.load$custom.diffeq
  DE$custom.diffeq.df  <- model.load$custom.diffeq.df
  #-----------------------------------------------------------------------------
  
  # Load Input/Outputs Section
  
  #-----------------------------------------------------------------------------
  IO$n.IO <- model.load$n.IO
  IO$bool.IO.exists <- model.load$bool.IO.exists
  IO$bool.IO.added <- model.load$bool.IO.added #boolean to tell differential solver to look for input outputs
  IO$IO.info <- model.load$IO.info
  IO$n.inputs = checkForLoadedValue(model.load$n.inputs, 0)
  IO$n.outputs = checkForLoadedValue(model.load$n.outputs, 0)
  IO$bool.input.exists = checkForLoadedValue(model.load$bool.input.exists, TRUE)
  IO$bool.output.exists = checkForLoadedValue(model.load$bool.output.exists, TRUE)
  IO$bool.input.added = checkForLoadedValue(model.load$bool.input.added, FALSE)
  IO$bool.output.added = checkForLoadedValue(model.load$bool.output.added, FALSE)
  IO$input.info = checkForLoadedValue(model.load$input.info, data.frame(matrix(ncol = 7, nrow = 0,
                                                                               dimnames = list(NULL, c("Type", 
                                                                                                       "Species", 
                                                                                                       "RateConstant",
                                                                                                       "RateBySpecies", 
                                                                                                       "Vmax", 
                                                                                                       "Kcat", 
                                                                                                       "Enzyme")))))
  IO$output.info = checkForLoadedValue(model.load$output.info, data.frame(matrix(ncol = 7, nrow = 0,
                                                                               dimnames = list(NULL, c("Type", 
                                                                                                       "Species", 
                                                                                                       "RateConstant",
                                                                                                       "RateBySpecies", 
                                                                                                       "Vmax", 
                                                                                                       "Kcat", 
                                                                                                       "Enzyme")))))
  

  #-----------------------------------------------------------------------------
  
  # Load Counts Section
  
  #-----------------------------------------------------------------------------
  counts$loading.model <- counts$loading.model + 1

  
  #-----------------------------------------------------------------------------
  
  # Load Options Section
  
  #-----------------------------------------------------------------------------
  options$time.start <- model.load$time.start
  options$time.end <- model.load$time.end
  options$time.step <- model.load$time.step
  options$time.scale.bool <- model.load$time.scale.bool
  options$time.scale.value <- model.load$time.scale.value
  options$ode.solver.type <- model.load$ode.solver.type
  
  #-----------------------------------------------------------------------------
  
  # Load Results Section
  
  #-----------------------------------------------------------------------------
  results$model       <- model.load$model
  results$is.pp       <- model.load$is.pp
  results$pp.eqns     <- model.load$pp.eqns
  results$pp.eqns.col <- model.load$pp.eqns.col
  results$pp.vars     <- model.load$pp.vars
  results$pp.model    <- model.load$pp.model
  results$model.final <- checkForLoadedValue(model.load$model.final, data.frame())
  results$model.has.been.solved <- checkForLoadedValue(model.load$model.has.been.solved,
                                                       FALSE)
  #-----------------------------------------------------------------------------
  
  # Load Logs Section
  
  #-----------------------------------------------------------------------------
  logs$IO.logs     <- checkForLoadedValue(model.load$IO.logs, vector())
  logs$input.logs  <- checkForLoadedValue(model.load$input.logs, vector())
  logs$output.logs <- checkForLoadedValue(model.load$output.logs, vector())
  
  
  #-----------------------------------------------------------------------------
  
  # ID for variable Section
  
  #-----------------------------------------------------------------------------
  id$id.variables <- checkForLoadedValue(model.load$id.variables, data.frame(matrix(ncol = 2
                                                                                    ,nrow = 0,
                                                                                    dimnames = list(NULL, c("id", "idName")))))
  id$id.parameters <- checkForLoadedValue(model.load$id.parameters, data.frame(matrix(ncol = 2
                                                                                      ,nrow = 0,
                                                                                      dimnames = list(NULL, c("id", "idName")))))
  id$id.equations <- checkForLoadedValue(model.load$id.equations, data.frame(matrix(ncol = 2
                                                                                    ,nrow = 0,
                                                                                    dimnames = list(NULL, c("id", "idName")))))
  id$id.diffeq <- checkForLoadedValue(model.load$id.diffeq, data.frame(matrix(ncol = 2
                                                                              ,nrow = 0,
                                                                              dimnames = list(NULL, c("id", "idName")))))
  
  id$id.var.seed    <- checkForLoadedValue(model.load$id.var.seed, 1)
  id$id.eqn.seed    <- checkForLoadedValue(model.load$id.eqn.seed, 1)
  id$id.param.seed  <- checkForLoadedValue(model.load$id.param.seed, 1)
  id$id.diffeq.seed <- checkForLoadedValue(model.load$id.diffeq.seed, 1)
  
  #Generates seeds for an older model that does not use the id system yet
  # if (id$id.var.seed == 1) {
  #   #generate ids
  #   ids <- GenerateIdsForOldModel(vars$species, params$vars.all, eqns$main, DE$eqns)
  #   id$id.variables <- ids$var
  #   id$id.parameters <- ids$par
  #   id$id.equations <- ids$eqn
  #   id$id.diffeq <- ids$dif
  #   id$id.seed <- ids$seed
  # }
  #solveForDiffEqs()
  
  # Load things for loop mode
  loop$parameters <- params$param.table
  loop$ICs <- ICs$ICs.table
  loop$model.results <- results$model.final
  loop$time.start <- options$time.start 
  loop$time.end <- options$time.end 
  loop$time.step <- options$time.step 

  #initialize things for compare mode
  compareModel$model.1 <- results$model.final
  compareModel$model.2 <- results$model.final
  compareModel$model.3 <- results$model.final
  compareModel$model.4 <- results$model.final
  
  #-----------------------------------------------------------------------------
  
  # Update all UI that need values from load
  
  #-----------------------------------------------------------------------------
  # The next two reset the parameter table
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
  
  parameter_table_values$table <- params$param.table
  my.choices <- paste0(seq(eqns$n.eqns), ") ", eqns$main)
  
  # updatePickerInput(session, 
  #                   "compare_models_select_vars",
  #                   choices = params$vars.all)
  updatePickerInput(session,
                    "eqnCreate_selectEqnForDescription",
                    choices = my.choices)
  
  updatePickerInput(session = session
                    ,"createVar_deleteVarPicker"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session, 
                    "eqnCreate_rate_firstvar",
                    choices = params$vars.all)
  
  updatePickerInput(session
                    ,"InOut_selectVar"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session
                    ,"Inout_delete_IO_eqn"
                    ,choices = seq(IO$n.IO))

  updatePickerInput(session,
                    'eqnCreate_edit_select_equation'
                    ,choices = seq(length(eqns$main)))
  
  updatePickerInput(session
                    ,"enzyme_deg_enzyme"#updates output enzyme choices for enzyme degradation
                    ,choices = sort(vars$species))
  
  updatePickerInput(session,
                    "MA_species"
                    ,choices = sort(vars$species))
  
  updatePickerInput(session #updates output substrate choices for enzyme degradation
                    ,"enzyme_deg_substrate"
                    ,choices = sort(vars$species))

  # Update Model Options -------------------------------------------------------
  updateTextInput(session,
                  "execute_time_start",
                  value = options$time.start)
  updateTextInput(session,
                  "execute_time_end",
                  value = options$time.end)
  updateTextInput(session,
                  "execute_time_step",
                  value = options$time.step)
  updateCheckboxInput(session,
                      "execute_turnOn_time_scale_var",
                      value = options$time.scale.bool)
  updateTextInput(session,
                  "execute_time_scale_var",
                  value = options$time.scale.value)
  updatePickerInput(session,
                    "execute_ode_solver_type",
                    selected = options$ode.solver.type)

  if (ncol(results$model.final) != 0) {
    updatePickerInput(session
                      ,"lineplot_xvar"
                      ,choices = colnames(results$model.final[1]))
  }
  
  updateSelectizeInput(session,
                       "lineplot_yvar"
                       ,choices  = colnames(results$model.final)[2:ncol(results$model.final)]
                       ,selected = colnames(results$model.final)[2:ncol(results$model.final)])
  updateTextInput(session, "loop_start_time", value = input$execute_time_start)
  updateTextInput(session, "loop_end_time", value = input$execute_time_end)
  updateTextInput(session, "loop_time_step", value = input$execute_time_step)

  # w_load$hide()
  waiter_hide()
})