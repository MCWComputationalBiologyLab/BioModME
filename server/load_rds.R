
# Functions --------------------------------------------------------------------
LoadCheck <- function(loadedValue, initValue) {
  #this function is meant to perform checks on if loaded value is null
  #between versions there are often things that get added that save as null.
  #these checks are meant to stop breakdown between old models and new app versions
  # Inputs:
  #  @loadedValue - the value loaded from the model
  #  @initi value - what the initialzied value should be if it is null
  if (!isTruthy(loadedValue)) {
    out <- initValue
  } else {
    out <- loadedValue
  }
  return(out)
}

load_event_trigger <- reactive({
  list(input$file_input_load_rds,
       input$bttn_load_model_from_base_repo)
})

observeEvent(input$file_input_load_rds, {
  rv.LOADBUTTONS$LB.button.name <- "Load_user_model"
  rv.LOADBUTTONS$LB.count <- rv.LOADBUTTONS$LB.count + 1
})

# Event: Load model ------------------------------------------------------------
observeEvent(rv.LOADBUTTONS$LB.count, {
  req(rv.LOADBUTTONS$LB.button.name != "")
  
  waiter_show(html = waiting_screen)
  Sys.sleep(1)
  
  if (rv.LOADBUTTONS$LB.button.name == "Load_user_model") {
    model <- readRDS(input$file_input_load_rds$datapath)
  } else if (rv.LOADBUTTONS$LB.button.name == "Load_base_model") {
    path.to.model <- file.path("base_models", input$SI_repos_base_choices)
    model <- readRDS(path.to.model)
  }
  
  # browser()
  # Load Model Information -----------------------------------------------------
  rv.MODEL.INFO$model.name <- model$model.name
  rv.MODEL.INFO$model.description <- model$model.description
  
  # Load Compartments ----------------------------------------------------------
  rv.COMPARTMENTS$compartments       <- model$compartments
  rv.COMPARTMENTS$compartments.df    <- model$compartments.df
  rv.COMPARTMENTS$compartments.names <- model$compartments.names
  
  # Load Species ---------------------------------------------------------------
  
  rv.SPECIES$species            <- model$species
  rv.SPECIES$species.df         <- model$species.df
  rv.SPECIES$species.names      <- model$species.names
  
  rv.SPECIES$df.by.compartment  <- model$df.by.compartment
  rv.SPECIES$plotted.var.table  <- model$plotted.var.table
  
  # Load Equations--------------------------------------------------------------
  
  rv.REACTIONS$reactions             <- model$reactions
  rv.REACTIONS$massAction            <- model$massAction
  rv.REACTIONS$massActionwReg        <- model$massActionwReg
  rv.REACTIONS$michaelisMenten       <- model$michaelisMenten
  rv.REACTIONS$synthesis             <- model$synthesis
  rv.REACTIONS$degradation.by.rate   <- model$degradation.by.rate
  rv.REACTIONS$degradation.by.enzyme <- model$degradation.by.enzyme
  
  rv.REACTIONS$reactions.df             <- model$reactions.df  
  rv.REACTIONS$massAction.df            <- model$massAction.df  
  rv.REACTIONS$massActionwReg.df        <- model$massActionwReg.df  
  rv.REACTIONS$michaelisMenten.df       <- model$michaelisMenten.df  
  rv.REACTIONS$synthesis.df             <- model$synthesis.df  
  rv.REACTIONS$degradation.by.rate.df   <- model$degradation.by.rate.df  
  rv.REACTIONS$degradation.by.enzyme.df <- model$degradation.by.enzyme.df  
  
  rv.REACTIONS$reaction.id.counter      <- model$reaction.id.counter
  
  # Load Input/Output ----------------------------------------------------------
  rv.IO$InputOutput            <- model$InputOutput
  rv.IO$IO.df                  <- model$IO.df
  rv.IO$IO.logs                <- model$IO.logs
  
  rv.IO$Flow.In                <- model$Flow.In
  rv.IO$Flow.Out               <- model$Flow.Out
  rv.IO$Flow.Between           <- model$Flow.Between
  rv.IO$Clearance              <- model$Clearance
  rv.IO$Simple.Diffusion       <- model$Simple.Diffusion
  rv.IO$Facillitated.Diffusion <- model$Facillitated.Diffusion
  rv.IO$IO.id.counter          <- model$IO.id.counter
  
  # Load Parameters ------------------------------------------------------------
  rv.PARAMETERS$parameters        <- model$parameters
  rv.PARAMETERS$parameters.df     <- model$parameters.df
  rv.PARAMETERS$parameters.names  <- model$parameters.names
  rv.PARAMETERS$non.constant.pars <- model$non.constant.pars
  
  # Load Differential Equations ------------------------------------------------
  rv.DE$de.equations.list  <- model$de.equations.list

  rv.DE$custom.diffeq.var  <- model$custom.diffeq.var
  rv.DE$custom.diffeq      <- model$custom.diffeq
  rv.DE$custom.diffeq.df   <- model$custom.diffeq.df
  
  rv.DE$de.eqns.for.solver <- unname(sapply(rv.DE$de.equations.list,
                                            get,
                                            x = "ODE.for.solver"))
  
  # Load Options ---------------------------------------------------------------
  rv.SOLVER.OPTIONS$time.start       <- model$time.start
  rv.SOLVER.OPTIONS$time.end         <- model$time.end
  rv.SOLVER.OPTIONS$time.step        <- model$time.step
  rv.SOLVER.OPTIONS$time.scale.bool  <- model$time.scale.bool
  rv.SOLVER.OPTIONS$time.scale.value <- model$time.scale.value
  rv.SOLVER.OPTIONS$time.unit        <- model$time.unit
  rv.SOLVER.OPTIONS$ode.solver.type  <- model$ode.solver.type
  
  # Load Results ---------------------------------------------------------------
  rv.RESULTS$results.model       <- model$results.model
  rv.RESULTS$results.is.pp       <- model$results.is.pp
  rv.RESULTS$results.pp.eqns     <- model$results.pp.eqns
  rv.RESULTS$results.pp.eqns.col <- model$results.pp.eqns.col
  rv.RESULTS$results.pp.vars     <- model$results.pp.vars
  rv.RESULTS$results.pp.model    <- model$results.pp.model
  rv.RESULTS$results.model.final <- model$results.model.final
  rv.RESULTS$results.model.has.been.solved <- model$results.model.has.been.solved
  rv.RESULTS$results.model.units.view <- model$results.model.units.view
  rv.RESULTS$results.time.units <- model$results.time.units
  rv.RESULTS$results.concentration.units <- model$results.concentration.units
  
  # Program Info ---------------------------------------------------------------
  rv.PROGRAM.INFO$version.number <- model$version.number
  
  # Load Logs ------------------------------------------------------------------
  rv.LOGS$variable.debug.button <- model$variable.debug.button
  rv.LOGS$variable.debug.table  <- model$variable.debug.table
  
  # Load IDs -------------------------------------------------------------------
  rv.ID$id.df           <- model$id.df
  rv.ID$id.var.seed     <- model$id.var.seed
  rv.ID$id.param.seed   <- model$id.param.seed
  rv.ID$id.eqn.seed     <- model$id.eqn.seed
  rv.ID$id.io.seed      <- model$id.io.seed
  rv.ID$id.comp.seed    <- model$id.comp.seed
  rv.ID$id.custeqn.seed <- model$id.custeqn.seed

  # Parameter Estimation -------------------------------------------------------
  rv.PAR.ESTIMATION$pe.loaded.species    <- model$pe.loaded.species
  rv.PAR.ESTIMATION$pe.parameters        <- model$pe.parameters
  rv.PAR.ESTIMATION$pe.initial.guess     <- model$pe.initial.guess
  rv.PAR.ESTIMATION$pe.lb                <- model$pe.lb
  rv.PAR.ESTIMATION$pe.ub                <- model$pe.ub
  rv.PAR.ESTIMATION$pe.calculated.values <- model$pe.calculated.values
  rv.PAR.ESTIMATION$pe.solved.model      <- model$pe.solved.model
  rv.PAR.ESTIMATION$pe.successful.run    <- model$pe.successful.run
  rv.PAR.ESTIMATION$pe.previous.values   <- model$pe.previous.values
  rv.PAR.ESTIMATION$pe.log.of.run        <- model$pe.log.of.run
  
  # Load Units -----------------------------------------------------------------
  # Dont need to load types, base.units, or possible.units
  rv.UNITS$units.selected <- model$units.selected

  # Load Custom Reaction Laws --------------------------------------------------
  rv.CUSTOM.LAWS$cl.reaction <- model$cl.reaction

  # Load Custom Equations ------------------------------------------------------
  rv.CUSTOM.EQNS$ce.equations <- LoadCheck(model$ce.equations, list())
  
  # Load Reaction Laws ---------------------------------------------------------
  rv.REACTIONLAWS$laws <- model$laws
  
  rv.COUNTS$loading.model <- rv.COUNTS$loading.model + 1
  # Plot - Compare Mode --------------------------------------------------------
  # compareModel$model.1 <- results$model.final
  # compareModel$model.2 <- results$model.final
  # compareModel$model.3 <- results$model.final
  # compareModel$model.4 <- results$model.final
  
  
  # Update terms from older loads ----------------------------------------------
  
  # If parameters don't have custom

  for (i in seq_along(rv.PARAMETERS$parameters)) {
    if (is.null(rv.PARAMETERS$parameters[[i]]$Custom)) {
      rv.PARAMETERS$parameters[[i]]$Custom <- FALSE
    }
  }
  
  # If files don't have content.ml
  if (!purrr::is_empty(rv.REACTIONS$reactions)) {
    if (!isTruthy(rv.REACTIONS$reactions[[1]]$Content.MathMl)) {
      for (i in seq_along(rv.REACTIONS$reaction)) {
        rv.REACTIONS$reaction[[i]]$Content.MathMl <-
          paste0("<math xmlns=\"http://www.w3.org/1998/Math/MathML\">",
                 string2mathml(rv.REACTIONS$reaction[[i]]$String.Rate.Law),
                 "</math>")
      }
    }
  }

  
  # Update UI w/ Loaded Values -------------------------------------------------
  #Update Model Info withe saved data
  model.name <- ifelse(is.null(rv.MODEL.INFO$model.name),
                       "New Model",
                       rv.MODEL.INFO$model.name)
  
  model.desc <- ifelse(is.null(rv.MODEL.INFO$model.description),
                       "No description given.",
                       rv.MODEL.INFO$model.description)
  
  updateTextInput(
    session = session,
    inputId = "TI_model_name",
    value = model.name
    ) 
  updateTextAreaInput(
    session = session,
    inputId = "TAI_model_description",
    value = model.desc
  )
  
  # The next two reset the parameter table
  updatePickerInput(session = session,
                    inputId = "parameters_filter_type",
                    selected = "Eqns")
  updatePickerInput(session = session,
                    inputId = "parameters_filter_type",
                    selected = "All")
  
  updatePickerInput(
    session = session,
    "createVar_deleteVarPicker",
    choices = sort(names(rv.SPECIES$species))
  )
  
  updatePickerInput(session,
                    "eqnCreate_rate_firstvar",
                    choices = names(rv.PARAMETERS$parameters))
  
  updatePickerInput(session,
                    "InOut_selectVar",
                    choices = sort(names(rv.SPECIES$species)))
  
  updatePickerInput(session,
                    "Inout_delete_IO_eqn",
                    choices = seq(rv.IO$n.IO))

  updatePickerInput(session,
                    'eqnCreate_edit_select_equation',
                    choices = seq(length(rv.REACTIONS$reactions)))
  
  #updates output enzyme choices for enzyme degradation
  updatePickerInput(session,
                    "enzyme_deg_enzyme",
                    choices = sort(names(rv.SPECIES$species)))
  
  updatePickerInput(session,
                    "MA_species",
                    choices = sort(names(rv.SPECIES$species)))
  
  #updates output substrate choices for enzyme degradation
  updatePickerInput(session, 
                    "enzyme_deg_substrate",
                    choices = sort(names(rv.SPECIES$species)))
  
  # Update Model Options -------------------------------------------------------
  updateTextInput(session,
                  "execute_time_start",
                  value = rv.SOLVER.OPTIONS$time.start)
  updateTextInput(session,
                  "execute_time_end",
                  value = rv.SOLVER.OPTIONS$time.end)
  updateTextInput(session,
                  "execute_time_step",
                  value = rv.SOLVER.OPTIONS$time.step)
  updateCheckboxInput(session,
                      "execute_turnOn_time_scale_var",
                      value = rv.SOLVER.OPTIONS$time.scale.bool)
  updateTextInput(session,
                  "execute_time_scale_var",
                  value = rv.SOLVER.OPTIONS$time.scale.value)
  updatePickerInput(session,
                    "execute_ode_solver_type",
                    selected = rv.SOLVER.OPTIONS$ode.solver.type)
  
  if (ncol(rv.RESULTS$results.model.final) != 0) {
    updatePickerInput(session,
                      "lineplot_xvar",
                      choices = colnames(rv.RESULTS$results.model.final[1]))
  }
  
  updateSelectizeInput(
    session,
    "lineplot_yvar",
    choices  = colnames(
      rv.RESULTS$results.model.final)[2:ncol(rv.RESULTS$results.model.final)],
    selected = colnames(
      rv.RESULTS$results.model.final)[2:ncol(rv.RESULTS$results.model.final)]
  )
  updateTextInput(session, "loop_start_time", value = input$execute_time_start)
  updateTextInput(session, "loop_end_time", value = input$execute_time_end)
  updateTextInput(session, "loop_time_step", value = input$execute_time_step)
  # browser()
  # Update Reaction laws to load properly
  updatePickerInput(session, "eqnCreate_type_of_equation", selected = "All")
  option.names <- rv.REACTIONLAWS$laws %>% pull(Name)
  options      <- rv.REACTIONLAWS$laws %>% pull(BackendName)
  names(options) <- option.names
  updatePickerInput(session, "eqnCreate_reaction_law", choices = options)
  

  
  # w_load$hide()
  waiter_hide()
})

