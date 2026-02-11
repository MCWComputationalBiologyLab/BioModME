
reset_all_storage_variables <- function() {
  print("resetting all variables in model")
  # rv.MODEL.INFO ----------------------------------------------------------------
  # rv.MODEL.INFO <- reactiveValues(
  #   model.name = "New Model",
  #   model.description = "No description given."
  # )
  # 
  # # rv.COMPARTMENTS --------------------------------------------------------------
  # rv.COMPARTMENTS <- reactiveValues(
  #   compartments = list(),
  #   compartments.df = data.frame(),
  #   compartments.names = vector()
  # )
  # 
  # # rv.SPECIES -------------------------------------------------------------------
  # rv.SPECIES <- reactiveValues(
  #   species = list(),
  #   species.df = data.frame(),
  #   species.names = vector(),
  #   df.by.compartment = data.frame(),
  #   plotted.var.table = data.frame()
  # )
  # 
  # # rv.REACTIONS -----------------------------------------------------------------
  # rv.REACTIONS <- reactiveValues(
  #   reactions = list(),
  #   massAction = list(),
  #   massActionwReg = list(),
  #   michaelisMenten = list(),
  #   synthesis = list(),
  #   degradation.by.rate = list(),
  #   degradation.by.enzyme = list(),
  #   reactions.df = data.frame(),
  #   massAction.df = data.frame(),
  #   massActionwReg.df = data.frame(),
  #   michaelisMenten.df = data.frame(),
  #   synthesis.df = data.frame(),
  #   degradation.by.rate.df = data.frame(),
  #   degradation.by.enzyme.df = data.frame(),
  #   reaction.id.counter = 0
  # )
  # 
  # # rv.IO ------------------------------------------------------------------------
  # rv.IO <- reactiveValues(
  #   InputOutput = list(),
  #   IO.df = data.frame(),
  #   IO.logs = vector(),
  #   Flow.In  = list(),
  #   Flow.Out = list(),
  #   Flow.Between = list(),
  #   Clearance = list(),
  #   Simple.Diffusion = list(),
  #   Facilitated.Diffusion = list(),
  #   IO.id.counter = 1
  # )
  # 
  # # rv.PARAMETERS ----------------------------------------------------------------
  # rv.PARAMETERS <- reactiveValues(
  #   parameters = list(), 
  #   parameters.df = data.frame(),
  #   parameters.names = vector(),
  #   non.constant.pars = list()
  # )
  # 
  # # rv.DE (Differential Equations)------------------------------------------------
  # rv.DE <- reactiveValues(
  #   de.equations.list = list(),
  #   de.string.eqns     = vector(),
  #   de.latex.eqns      = vector(),
  #   de.mathjax.eqns    = vector(),
  #   de.eqns.for.solver = vector(),
  #   custom.diffeq.var = vector(),
  #   custom.diffeq = vector(),
  #   custom.diffeq.df = data.frame(matrix(ncol = 2, nrow = 0))
  # )
  # 
  # # rv.SOLVER.OPTIONS ------------------------------------------------------------
  # rv.SOLVER.OPTIONS <- reactiveValues(
  #   time.start = 0,
  #   time.end = 100,
  #   time.step = 1,
  #   time.scale.bool = FALSE,
  #   time.scale.value = 0,
  #   time.unit = "min",
  #   ode.solver.type = "lsoda"
  # )
  # 
  # # rv.RESULTS -------------------------------------------------------------------
  # rv.RESULTS <- reactiveValues(
  #   results.model = data.frame(),
  #   results.model.units.view = data.frame(),
  #   results.is.pp = FALSE,
  #   results.pp.eqns = vector(),
  #   results.pp.eqns.col = vector(),
  #   results.pp.vars = vector(),
  #   results.pp.model = data.frame(),
  #   results.model.final = data.frame(),
  #   results.model.has.been.solved = FALSE,
  #   results.time.units = "min",
  #   results.concentration.units = "mol"
  # )
  # 
  # # rv.PROGRAM.INFO --------------------------------------------------------------
  # rv.PROGRAM.INFO <- reactiveValues(
  #   version.number = 1.2
  # )
  # 
  # # rv.LOGS ----------------------------------------------------------------------
  # rv.LOGS <- reactiveValues(
  #   variable.debug.button = "",
  #   variable.debug.table = data.frame()
  # )
  # 
  # # rv.ID ------------------------------------------------------------------------
  # rv.ID <- reactiveValues(
  #   id.df = data.frame(
  #     matrix(ncol = 2, nrow = 0, dimnames = list(NULL, c("id", "idName")))
  #   ),
  #   id.var.seed = 1,
  #   id.eqn.seed = 1,
  #   id.param.seed = 1,
  #   id.comp.seed = 1,
  #   id.io.seed = 1,
  #   id.custeqn.seed = 1,
  #   id.custeqnaddional.seed = 1
  # )
  # 
  # # rv.COUNTS --------------------------------------------------------------------
  # rv.COUNTS <- reactiveValues(
  #   loading.model = 0
  # )
  # 
  # # rv.PAR.ESTIMATION ------------------------------------------------------------
  # rv.PAR.ESTIMATION <- reactiveValues(
  #   pe.loaded.species = vector(),
  #   pe.parameters = vector(),
  #   pe.initial.guess = vector(),
  #   pe.lb = vector(),
  #   pe.ub = vector(),
  #   pe.calculated.values = vector(),
  #   pe.solved.model = data.frame(),
  #   pe.successful.run = FALSE,
  #   pe.previous.values = vector(),
  #   pe.log.of.run = "Parameter Estimation Iterations will appear here"
  # )
  # 
  # # rv.PLOT.LOOP -----------------------------------------------------------------
  # rv.PLOT.LOOP <- reactiveValues(
  #   loop.parameters = data.frame(matrix(ncol=3, nrow=0, dimnames = list(NULL, c("Parameter", "Value", "Description")))),
  #   loop.ICs = data.frame(matrix(ncol = 4, nrow = 0, dimnames = list(NULL, c("Variable", "Value", "Units", "Description")))),
  #   loop.time.start = 0,
  #   loop.time.end = 100, 
  #   loop.time.step = 1,
  #   loop.model.results = data.frame()
  # )
  # 
  # # rv.UNITS ---------------------------------------------------------------------
  # rv.UNITS <- reactiveValues(
  #   units.types = c("Duration", "Energy", "Length", "Mass", "Volume", "Flow", "Count"), 
  #   units.base = list("Duration" = "min", "Energy" ="kJ", "Length" = "m", "Mass" = "g", "Volume" = "L", "Flow" = "l_per_min", "Count" = "mol", "For.Var" = "mol"),
  #   units.choices = list("Duration" = measurements::conv_unit_options$duration,
  #                        "Energy" =  measurements::conv_unit_options$energy,
  #                        "Length" =  measurements::conv_unit_options$length,
  #                        "Mass" =    measurements::conv_unit_options$mass,
  #                        "Volume" =  measurements::conv_unit_options$volume,
  #                        "Flow" =    measurements::conv_unit_options$flow,
  #                        "Count" =   measurements::conv_unit_options$count,
  #                        "For.Var" = measurements::conv_unit_options$count),
  #   units.selected = list("Duration" = "min",
  #                         "Energy" ="kJ",
  #                         "Length" = "m",
  #                         "Mass" = "g",
  #                         "Volume" = "l",
  #                         "Flow" = "l_per_min",
  #                         "Count" = "mol",
  #                         "For.Var" = "mol")
  # )
  # 
  # # rv.REFRESH -------------------------------------------------------------------
  # # rv.REFRESH <- reactiveValues(
  # #   refresh.compartment.table = 1,
  # #   refresh.species.table = 1,
  # #   refresh.param.table = 1,
  # #   refresh.eqn.table = 1
  # # )
  # 
  # 
  # rv.REACTIONLAWS <- reactiveValues(
  #   laws = data.frame(
  #     Name = c("Mass Action", "Mass Action (Regulated)", "Synthesis", "Degradation (Rate)", "Degradation (Enzyme)", "Michaelis Menten"),
  #     BackendName = c("mass_action", "mass_action_w_reg", "synthesis", "degradation_rate", "degradation_by_enzyme", "michaelis_menten"), 
  #     Type = c("chemical", "chemical", "chemical", "chemical", "chemical", "enzyme")
  #   ),
  #   current.selected.law = ""
  # )
  # 
  # # rv.DEBUG ---------------------------------------------------------------------
  # rv.DEBUG <- reactiveValues(
  #   variable.debug.button = "compartments"
  # )
  # 
  # # rv.CUSTOM.LAWS ---------------------------------------------------------------
  # rv.CUSTOM.LAWS <- reactiveValues(
  #   cl.reaction = list(),
  #   reaction.names = vector()
  # )
  # 
  # # rv.CUSTOM.EQNS ---------------------------------------------------------------
  # rv.CUSTOM.EQNS <- reactiveValues(
  #   ce.equations = list()
  # )
  # 
  # rv.LOADBUTTONS <- reactiveValues(
  #   LB.button.name = "",
  #   LB.count = 1
  # )
  # 
  # rv.sbml.temp <- reactiveValues(
  #   need.compartment.conversion = FALSE,
  #   need.species.conversion = FALSE,
  #   need.parameter.conversion = FALSE,
  #   comp.df.conv = data.frame(),
  #   species.df.conv = data.frame(),
  #   parameter.df.conv = data.frame(),
  #   id.df = data.frame(
  #     matrix(ncol = 2,nrow = 0, dimnames = list(NULL, c("id", "idName")))
  #   ),
  #   id.comp.seed = 1,
  #   id.var.seed = 1,
  #   id.param.seed = 1,
  #   id.custeqn.seed = 1,
  #   id.eqn.seed = 1,
  #   id.custeqnaddional.seed = 1,
  #   compartments = list(),
  #   compartments.df = data.frame(),
  #   species = list(),
  #   species.df = data.frame(),
  #   parameters = list(),
  #   parameters.df = data.frame(),
  #   cl.reaction = list(),
  #   ce.equations = list(),
  #   laws = data.frame(
  #     Name = c("Mass Action", "Mass Action (Regulated)", "Synthesis", "Degradation (Rate)", "Degradation (Enzyme)", "Michaelis Menten"),
  #     BackendName = c("mass_action", "mass_action_w_reg", "synthesis", "degradation_rate", "degradation_by_enzyme", "michaelis_menten"), 
  #     Type = c("chemical", "chemical", "chemical", "chemical", "chemical", "enzyme")
  #   ),
  #   reactions = list(),
  #   refresh.species.table = 1
  # )
  # 
  # rv <- reactiveValues(error.in.conversion = FALSE)
  # 
  # rv.TIMELINK <- reactiveValues(
  #   start = 0,
  #   end = 10,
  #   step = 1,
  #   unit = "mol"
  # )
  
  rv.COMPARTMENTS$compartments <- list()
  rv.COMPARTMENTS$compartments.df <- data.frame()
  rv.COMPARTMENTS$compartments.names <- vector()
  
  rv.SPECIES$species <- list()
  rv.SPECIES$species.df <- data.frame()
  rv.SPECIES$species.names <- vector()
  rv.SPECIES$df.by.compartment <- data.frame()
  rv.SPECIES$plotted.var.table <- data.frame()
  
  rv.REACTIONS$reactions = list()
  rv.REACTIONS$massAction = list()
  rv.REACTIONS$massActionwReg = list()
  rv.REACTIONS$michaelisMenten = list()
  rv.REACTIONS$synthesis = list()
  rv.REACTIONS$degradation.by.rate = list()
  rv.REACTIONS$degradation.by.enzyme = list()
  rv.REACTIONS$reactions.df = data.frame()
  rv.REACTIONS$massAction.df = data.frame()
  rv.REACTIONS$massActionwReg.df = data.frame()
  rv.REACTIONS$michaelisMenten.df = data.frame()
  rv.REACTIONS$synthesis.df = data.frame()
  rv.REACTIONS$degradation.by.rate.df = data.frame()
  rv.REACTIONS$degradation.by.enzyme.df = data.frame()
  rv.REACTIONS$reaction.id.counter = 0
  
  rv.IO$InputOutput = list()
  rv.IO$IO.df = data.frame()
  rv.IO$IO.logs = vector()
  rv.IO$Flow.In = list()
  rv.IO$Flow.Out = list()
  rv.IO$Flow.Between = list()
  rv.IO$Clearance = list()
  rv.IO$Simple.Diffusion = list()
  rv.IO$Facilitated.Diffusion = list()
  rv.IO$IO.id.counter = 1
  
  rv.PARAMETERS$parameters = list()
  rv.PARAMETERS$parameters.df = data.frame()
  rv.PARAMETERS$parameters.names = vector()
  rv.PARAMETERS$non.constant.pars = list()
  
  rv.DE$de.equations.list = list()
  rv.DE$de.string.eqns = vector()
  rv.DE$de.latex.eqns = vector()
  rv.DE$de.mathjax.eqns = vector()
  rv.DE$de.eqns.for.solver = vector()
  rv.DE$custom.diffeq.var = vector()
  rv.DE$custom.diffeq = vector()
  rv.DE$custom.diffeq.df = data.frame(matrix(ncol = 2, nrow = 0))
  
  rv.RESULTS$results.model = data.frame()
  rv.RESULTS$results.model.units.view = data.frame()
  rv.RESULTS$results.is.pp = FALSE
  rv.RESULTS$results.pp.eqns = vector()
  rv.RESULTS$results.pp.eqns.col = vector()
  rv.RESULTS$results.pp.vars = vector()
  rv.RESULTS$results.pp.model = data.frame()
  rv.RESULTS$results.model.final = data.frame()
  rv.RESULTS$results.model.has.been.solved = FALSE
  rv.RESULTS$results.time.units = "min"
  rv.RESULTS$results.concentration.units = "mol"
  
  rv.ID$id.df = data.frame(
    matrix(ncol = 2, nrow = 0, dimnames = list(NULL, c("id", "idName")))
  )
  rv.ID$id.var.seed = 1
  rv.ID$id.eqn.seed = 1
  rv.ID$id.param.seed = 1
  rv.ID$id.comp.seed = 1
  rv.ID$id.io.seed = 1
  rv.ID$id.custeqn.seed = 1
  rv.ID$id.custeqnaddional.seed = 1
  
  rv.PAR.ESTIMATION$pe.loaded.species = vector()
  rv.PAR.ESTIMATION$pe.parameters = vector()
  rv.PAR.ESTIMATION$pe.initial.guess = vector()
  rv.PAR.ESTIMATION$pe.lb = vector()
  rv.PAR.ESTIMATION$pe.ub = vector()
  rv.PAR.ESTIMATION$pe.calculated.values = vector()
  rv.PAR.ESTIMATION$pe.solved.model = data.frame()
  rv.PAR.ESTIMATION$pe.successful.run = FALSE
  rv.PAR.ESTIMATION$pe.previous.values = vector()
  rv.PAR.ESTIMATION$pe.log.of.run = "Parameter Estimation Iterations will appear here"
  
  rv.PLOT.LOOP$loop.parameters = data.frame(matrix(ncol = 3, nrow = 0, dimnames = list(NULL, c("Parameter", "Value", "Description"))))
  rv.PLOT.LOOP$loop.ICs = data.frame(matrix(ncol = 4, nrow = 0, dimnames = list(NULL, c("Variable", "Value", "Units", "Description"))))
  rv.PLOT.LOOP$loop.time.start = 0
  rv.PLOT.LOOP$loop.time.end = 100
  rv.PLOT.LOOP$loop.time.step = 1
  rv.PLOT.LOOP$loop.model.results = data.frame()
  
  rv.REACTIONLAWS$laws = data.frame(
    Name = c("Mass Action", "Mass Action (Regulated)", "Synthesis", "Degradation (Rate)", "Degradation (Enzyme)", "Michaelis Menten"),
    BackendName = c("mass_action", "mass_action_w_reg", "synthesis", "degradation_rate", "degradation_by_enzyme", "michaelis_menten"), 
    Type = c("chemical", "chemical", "chemical", "chemical", "chemical", "enzyme")
  )
  rv.REACTIONLAWS$current.selected.law = ""
  
  rv.CUSTOM.LAWS$cl.reaction = list()
  rv.CUSTOM.LAWS$reaction.names = vector()
  
  rv.CUSTOM.EQNS$ce.equations = list()
  
  # shinyjs::click("createVar_add_compartment_button")
  # 
  rv.REFRESH$refresh.compartment.table <- 
    rv.REFRESH$refresh.compartment.table + 1
  
  rv.REFRESH$refresh.species.table <- 
    rv.REFRESH$refresh.species.table + 1
  
  rv.REFRESH$refresh.param.table <- 
    rv.REFRESH$refresh.param.table + 1
  
  rv.REFRESH$refresh.eqn.table <- 
    rv.REFRESH$refresh.eqn.table + 1
}