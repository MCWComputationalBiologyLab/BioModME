
#these are the main values that will be used to run and generate the model
vars <- reactiveValues(
  species = vector() #stores model species
  ,descriptions = vector() #stores descriptions of Model
  ,table = data.frame(matrix(ncol = 2
                            ,nrow = 0,
                            dimnames = list(NULL, c("Variable Name"
                                                    ,"Description"))))
)


eqns <- reactiveValues(
   first.run = TRUE #determine if first equation is added yet or not
  ,main = vector() #stores eqn type in model
  ,eqn.main.latex = vector() #latex versions of equations to print
  ,eqn.main.mathjax = vector() #mathjax version of equations to print in app
  ,n.eqns.no.del = 0 #This is used to keep track of how many eqns were made (specifically keeping track of pregenerated rate constant naming)
  ,n.eqns = 0 #stores number of total equations in model (used to autofill names of some var)
  ,n.eqns.chem = 0
  ,n.eqns.enz = 0
  ,n.eqns.syn = 0
  ,n.eqns.deg = 0
  ,additional.eqns = vector() #stores all additional eqns -time, rate, etc...
  ,rate.eqns = vector() #stores all the elements of the rate equations to be added to the model
  ,time.dep.eqns = vector() #stores all time dependent eqns
  
  ,lr.eqns = vector() #stores all rate eqns
  ,eqn.descriptions = vector() #stores all eqn descriptions
  ,eqn.info = data.frame(
    matrix(
      ncol = 7, 
      nrow = 0, 
      dimnames = list(NULL, 
                      c("ID",            # (1)  Specific equation ID
                        "EqnType",       # (2)  Type of equation (chem, enz)
                        "Law",           # (3)  Law that the equation uses
                        "Species",       # (4)  Species in equations
                        "RateConstants", # (5)  Parameters in equation
                        "Compartment",   # (6)  Compartment reaction occurs in
                        "Description"    # (7)  Equation Description
                      )
      )))
  ,eqn.chem = data.frame(
    matrix(
      ncol = 15, 
      nrow = 0,
      dimnames = list(NULL,
                      c("ID",         # (1)  Specific equation ID
                        "Law",        # (2)  Chemical Law
                        "LHS_coef",   # (3)  LHS Coefs (3 in 3A --> 2B)
                        "LHS_var",    # (4)  LHS Vars (A in 3A --> 2B)
                        "RHS_coef",   # (5)  Coefficients on RHS of equation
                        "RHS_var",    # (6)  Variables on RHS of equation
                        "arrow_type", # (7)  Reversible or forward only
                        "kf",         # (8)  Forward Reaction Coefficient
                        "kr",         # (9)  Reverse Reaction Coefficient
                        "FM_bool",    # (10) Boolean if forward regulator exists
                        "FMs",        # (11) Forward Regulators (Modifiers)
                        "FM_rateC",   # (12) Corresponding rate constants for FM
                        "RM_bool",    # (13) Boolean if reverse regulator exists
                        "RMs",        # (14) Reverse Regulators (Modifiers)
                        "RM_rateC"    # (15) Corresponding rate constants for RM
                      )
                      )))

  ,eqn.enzyme = data.frame(
    matrix(
      ncol = 8,
      nrow = 0,
      dimnames = list(NULL,
                      c("ID",        # (1)  ID of enzyme reaction
                        "Law",       # (2)  Law that enzyme reaction follows
                        "Substrate", # (3)  Substrate that enzyme acts upon
                        "Product",   # (4)  Product of the enzyme reaction
                        "Enzyme",    # (5)  Enzyme in reaction
                        "kcat",      # (6)  Catalytic RC for enzyme reaction
                        "Km",        # (7)  Michelis Menton Constant
                        "Vmax"       # (8)  Maximum Velocity for enz reaction
                        )
                      )))
  ,eqn.syn = data.frame(
    matrix(
      ncol = 5,
      nrow = 0,
      dimnames = list(NULL,
                      c("ID",        # (1)  ID of enzyme reaction
                        "Law",       # (2)  Law that enzyme reaction follows
                        "VarSyn",    # (3)  Variable being synthesized
                        "RC",        # (4)  Rate Constant for synthesis reaction
                        "Factor"    # (5)  Factor causing synthesis of VarSyn
                      )
      )))
  ,eqn.deg = data.frame(
    matrix(
      ncol = 9,
      nrow = 0,
      dimnames = list(NULL,
                      c("ID",        # (1)  ID of enzyme reaction
                        "Law",       # (2)  Law that enzyme reaction follows
                        "VarDeg",    # (3)  Variable being degraded
                        "ConcDep",   # (4)  Bool is rate is concentration dependent
                        "RC",        # (5)  Rate Constant for Degradation reaction
                        "Km",        # (6)  Michaelis Menton Constant
                        "Enz",       # (7)  Enzyme causing the degradation
                        "Vmax",      # (8)  Maximum Velocity of enzyme degradation
                        "Prods"      # (9)  Products made from degradation if degradation turns into a product
                      )
      )))
)


IO <- reactiveValues(
  n.IO = 0 #stores the number of total Input and Outputs
  ,n.inputs = 0
  ,n.outputs = 0
  ,bool.IO.exists = TRUE #determines if In/out input has been given yet.  Avoids adding to df error
  ,bool.IO.added = FALSE
  ,bool.input.exists = TRUE
  ,bool.output.exists = TRUE
  ,bool.input.added = FALSE
  ,bool.output.added = FALSE
  ,input.info = data.frame(matrix(ncol = 7, nrow = 0,
                                  dimnames = list(NULL, c("Type", 
                                                          "Species", 
                                                          "RateConstant",
                                                          "RateBySpecies", 
                                                          "Vmax", 
                                                          "Kcat", 
                                                          "Enzyme"))
                                  ))
  ,output.info = data.frame(matrix(ncol = 7, nrow = 0,
                                  dimnames = list(NULL, c("Type", 
                                                          "Species", 
                                                          "RateConstant",
                                                          "RateBySpecies", 
                                                          "Vmax", 
                                                          "Kcat", 
                                                          "Enzyme"))
  ))
  ,IO.info = data.frame(matrix(ncol = 8, nrow = 0,
                                      dimnames = list(NULL, c("In_or_Out", 
                                                              "Type", 
                                                              "Species", 
                                                              "RateConstant",
                                                              "RateBySpecies", 
                                                              "Vmax", 
                                                              "Kcat", 
                                                              "Enzyme"))))
  #(1) in_or_out = value to tell if this column is an input or output: "input" or "output"
  #(2) Type = gets the type of the input (rate, diffusion, synthesis, etc)
  #(3) Species = actual name of the species going in or out
  #(4) RateConstant = if type rate, name of the rate constant 
  #(5) Vmax = if type enzyme, Vmax of enzyme reaction
  #(6) Kcat = f type enzyme and Vmax not used, kcat of reaction (note Vmax = kcat*enzyme)
  #(7) Enzyme = if type enzyme and Vmax not used, enzyme concentration of reaction
  #(8) RateBySpecies = if rate equation, boolean to tell user to multiply the rate by the concentration of the rate species 
)
ICs <- reactiveValues(
  vals = vector() #store initial condition value
  ,comments = vector() #store comments for ICs
  ,ICs.table = data.frame(matrix(ncol = 3
                                 ,nrow = 0,
                                 dimnames = list(NULL, c("Variable"
                                                         ,"Value"
                                                         ,"Description"))))
  ,first.IC.stored = FALSE #if IC stored, this parameter is used to render values
)

params <- reactiveValues(
   vars.all = vector() #store parameter variable
  ,vals.all = vector() #store parameter value
  ,comments.all = vector() #store comments of parameters
  ,param.table = data.frame(matrix(ncol = 3
                                   ,nrow = 0,
                                   dimnames = list(NULL, c("Parameter"
                                                           ,"Value"
                                                           ,"Description"))))
  #store parameters from equations
  ,eqns.vars = vector() #param variable
  ,eqns.vals = vector() #param variable values
  ,eqns.comments = vector() #param comments
  ,first.param.eqn.stored = FALSE #if parameter stored button hit then this will update parameter values based on those stored and not reset them all to zero
  #store parameters for input variables
  ,inputs.vars = vector()
  ,inputs.vals = vector()
  ,inputs.comments = vector()
  ,first.inputs.stored = FALSE
  #store parameters for output variables
  ,outputs.vars = vector()
  ,outputs.vals = vector()
  ,outputs.comments = vector()
  ,first.outputs.stored = FALSE
  #store parameters from rate variables
  ,rate.eqn.vars = vector()
  ,rate.eqn.vals = vector()
  ,rate.eqn.comments = vector()
  ,first.rate.eqn.stored = FALSE
  ,rate.params = vector()
  #store parameters from rate variables
  ,time.dep.vars = vector()
  ,time.dep.values = vector()
  ,time.dep.comments = vector()
  ,first.time.dep.stored = FALSE
  
  ,parameters.based.on.other.values = vector() #stores all vectors that are not based on other values and not given a hard value (ie k1 = 5*k2+k3 not simply k1 = 5)
  
)

DE <- reactiveValues(
  eqns = vector() #store differential equations
  ,eqns.in.latex  = vector() #store differential equations as latex eqns to print
  ,custom.diffeq.var = vector() #keeps track of indices of custom differential eqns
  ,custom.diffeq = vector() #keeps track of custom entered diffeq
  ,custom.diffeq.df = data.frame(matrix(ncol = 2, nrow = 0))
)

options <- reactiveValues(time.start = 0 
                          ,time.end = 100
                          ,time.step = 1
                          ,time.scale.bool = FALSE
                          ,time.scale.value = 0
                          ,ode.solver.type = "lsoda"
)

results <- reactiveValues(model = data.frame()
                                ,is.pp = FALSE #lets system know if post processing has occured
                                ,pp.eqns = vector() # keeeps tack of equations in text print form.
                                ,pp.eqns.col = vector() # keeps track of equation in processing form
                                ,pp.vars = vector() #vars to add
                                ,pp.model = data.frame() #new model with post processing
                                ,model.final = data.frame() #final data frame
                                ,model.has.been.solved = FALSE
)

info <- reactiveValues(
  version.number = 1.2
)

logs <- reactiveValues(IO.logs = vector() #record the log for which inputs are added or not
                       ,input.logs = vector()
                       ,output.logs = vector()
)

counts <- reactiveValues(loading.model = 0)

#-----------------------------------------------------------------------------

# ID for variable Section

#-----------------------------------------------------------------------------
id <- reactiveValues(
  id.variables = data.frame(matrix(ncol = 2
                                ,nrow = 0,
                                dimnames = list(NULL, c("id", "idName")))),
  id.parameters = data.frame(matrix(ncol = 2
                                 ,nrow = 0,
                                 dimnames = list(NULL, c("id", "idName")))),
  id.equations = data.frame(matrix(ncol = 2
                                 ,nrow = 0,
                                 dimnames = list(NULL, c("id", "idName")))),
  id.diffeq = data.frame(matrix(ncol = 2
                                 ,nrow = 0,
                                 dimnames = list(NULL, c("id", "idName")))),
  id.var.seed = 1,
  id.eqn.seed = 1,
  id.param.seed = 1,
  id.diffeq.seed = 1
)
