# Edit Tab Controlling the editing of equations

# Exponential growth edit builder
output$equationBuilder_exponential_growth_edit <- renderUI({
  div(
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId = "PI_exp_growth_species_edit",
          label   = "Growing Species",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          selected = input$PI_exp_growth_species_edit,
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      )
    ),
    fluidRow(
      column(
        width = 4,
        textInput(
          inputId = "TI_exp_growth_mu_edit",
          label = "Growth Rate Parameter (mu)",
          value = if (is.null(input$TI_exp_growth_mu_edit)) "mu" else input$TI_exp_growth_mu_edit
        )
      ),
      column(
        width = 3,
        numericInput(
          inputId = "NI_exp_growth_mu_value_edit",
          label = "Value",
          value = if (is.null(input$NI_exp_growth_mu_value_edit)) 0.7 else input$NI_exp_growth_mu_value_edit,
          min = 0,
          step = 0.01
        )
      )
    )
  )
})

# Monod growth edit builder
output$equationBuilder_monod_growth_edit <- renderUI({
  div(
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId = "PI_monod_species_edit",
          label   = "Growing Species (X)",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          selected = input$PI_monod_species_edit,
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      ),
      column(
        width = 4,
        pickerInput(
          inputId = "PI_monod_substrate_edit",
          label   = "Substrate (S)",
          choices = sort(rv.SPECIES$df.by.compartment$Name),
          selected = input$PI_monod_substrate_edit,
          options = pickerOptions(liveSearch = TRUE,
                                  liveSearchStyle = "startsWith")
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        textInput(
          inputId = "TI_monod_mu_max_edit",
          label = "mu_max",
          value = if (is.null(input$TI_monod_mu_max_edit)) "mu_max" else input$TI_monod_mu_max_edit
        )
      ),
      column(
        width = 3,
        numericInput(
          inputId = "NI_monod_mu_max_value_edit",
          label = "Value",
          value = if (is.null(input$NI_monod_mu_max_value_edit)) 0.7 else input$NI_monod_mu_max_value_edit,
          min = 0,
          step = 0.01
        )
      ),
      column(
        width = 3,
        textInput(
          inputId = "TI_monod_K_s_edit",
          label = "K_s (half-saturation)",
          value = if (is.null(input$TI_monod_K_s_edit)) "K_s" else input$TI_monod_K_s_edit
        )
      ),
      column(
        width = 3,
        numericInput(
          inputId = "NI_monod_K_s_value_edit",
          label = "Value",
          value = if (is.null(input$NI_monod_K_s_value_edit)) 0.5 else input$NI_monod_K_s_value_edit,
          min = 0.0001,
          step = 0.01
        )
      )
    )
  )
})

# Competitive Monod growth edit builder - moved to 02_equations_renderUI.R

# Left Box: Equation Edit Options ----------------------------------------------
output$eqnCreate_edit_rendering_sidebar <- renderUI({
# browser()
  # Find equation in data structure
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- rv.REACTIONS$reactions[[eqn.num]]
  
  # Unpack Equation Information
  eqn.ID               <- eqn.row$ID            
  eqn.display.type     <- eqn.row$Eqn.Display.Type 
  eqn.reaction.law     <- eqn.row$Reaction.Law    
  eqn.species          <- eqn.row$Species          
  eqn.reactants        <- eqn.row$Reactants        
  eqn.products         <- eqn.row$Products         
  eqn.Modifiers        <- eqn.row$Modifiers  
  eqn.parameters       <- eqn.row$Parameters       
  eqn.compartment      <- eqn.row$Compartment      
  eqn.description      <- eqn.row$Description      
  eqn.species.id       <- eqn.row$Species.id      
  eqn.reactants.id     <- eqn.row$Reactants.id     
  eqn.products.id      <- eqn.row$Products.id      
  eqn.modifiers.id     <- eqn.row$Modifiers.id     
  eqn.parameters.id    <- eqn.row$Parameters.id   
  eqn.compartment.id   <- eqn.row$Compartment.id   
  eqn.equation.text    <- eqn.row$Equation.Text    
  eqn.equation.latex   <- eqn.row$Equation.Latex   
  eqn.equation.mathjax <- eqn.row$Equation.MathJax 
  eqn.string.rate.law  <- eqn.row$String.Rate.Law  
  eqn.pretty.rate.law  <- eqn.row$Pretty.Rate.Law  
  eqn.latex.rate.law   <- eqn.row$Latex.Rate.Law   
  eqn.mathjax.rate.law <- eqn.row$MathJax.Rate.Law 
  eqn.mathml.rate.law  <- eqn.row$MathMl.Rate.Law 
  eqn.reversible       <- eqn.row$Reversible       
  
  # Initializing Vars (Need to check if I can remove this now)
  arrow_type  <- NA
  FR.bool     <- FALSE
  RR.bool     <- FALSE
  num.FRs     <- 1
  num.RRs     <- 1
  use.Vmax    <- FALSE
  prod.exists <- FALSE
  num.prods   <- 1

  # Unpack the different kind of laws to fill out proper information
  if (eqn.reaction.law == "mass_action") {
    # Extract reaction from chemical equation
    chemInfo <- rv.REACTIONS$massAction[[eqn.ID]]
    
    ID               <- chemInfo$ID
    law              <- chemInfo$Reaction.Law
    r.stoichiometry  <- str_split(chemInfo$r.stoichiometry, ", ")[[1]]
    Reactants        <- str_split(chemInfo$Reactants,  ", ")[[1]]
    p.stoichiometry  <- str_split(chemInfo$p.stoichiometry, ", ")[[1]]
    Products         <- str_split(chemInfo$Products,  ", ")[[1]] 
    Reactants.id     <- str_split(chemInfo$Reactants.id, ", ")[[1]]
    Products.id      <- str_split(chemInfo$Products.id, ", ")[[1]]
    arrow_type       <- chemInfo$Reversible
    kf               <- chemInfo$kf
    kr               <- chemInfo$kr
    kf.id            <- chemInfo$kf.id
    kr.id            <- chemInfo$kr.id
    
    
    div(
        pickerInput(
          inputId = "PI_mass_action_reverisble_option_edit",
          label = "Reversability?",
          choices = c("Reversible" = "both_directions",
                      "Irreversible" = "forward_only"),
          choicesOpt = list(icon = c("glyphicon glyphicon-resize-horizontal",
                                     "glyphicon glyphicon-arrow-right")),
          selected = arrow_type
        )
    )
  }
  else if (eqn.reaction.law == "mass_action_w_reg") {
    
    chemInfo <- rv.REACTIONS$massActionwReg[[eqn.ID]]
    
    ID               <- chemInfo$ID
    law              <- chemInfo$Reaction.Law
    r.stoichiometry  <- str_split(chemInfo$r.stoichiometry, ", ")[[1]]
    Reactants        <- str_split(chemInfo$Reactants,  ", ")[[1]]
    p.stoichiometry  <- str_split(chemInfo$p.stoichiometry, ", ")[[1]]
    Products         <- str_split(chemInfo$Products,  ", ")[[1]] 
    Reactants.id     <- str_split(chemInfo$Reactants.id, ", ")[[1]]
    Products.id      <- str_split(chemInfo$Products.id, ", ")[[1]]
    arrow_type       <- chemInfo$Reversible
    kf               <- chemInfo$kf
    kr               <- chemInfo$kr
    kf.id            <- chemInfo$kf.id
    kr.id            <- chemInfo$kr.id
    Use.Forward.Mod  <- chemInfo$Use.Forward.Mod
    Forward.Mods     <- str_split(chemInfo$Forward.Mods, ", ")[[1]]
    Forward.Mods.id  <- str_split(chemInfo$Forward.Mods.id, ", ")[[1]]
    Forward.Pars     <- str_split(chemInfo$Forward.Pars, ", ")[[1]]
    Forward.Pars.id  <- str_split(chemInfo$Forward.Pars.id, ", ")[[1]]
    Use.Reverse.Mod  <- chemInfo$Use.Reverse.Mod
    Reverse.Mods     <- str_split(chemInfo$Reverse.Mods, ", ")[[1]]
    Reverse.Mods.id  <- str_split(chemInfo$Reverse.Mods.id, ", ")[[1]]
    Reverse.Pars     <- str_split(chemInfo$Reverse.Pars, ", ")[[1]]
    Reverse.Pars.id  <- str_split(chemInfo$Reverse.Pars.id, ", ")[[1]]
    
    # Number of forward mods
    if (Use.Forward.Mod) {
      n.f.mods <- length(strsplit(Forward.Mods, ", ")[[1]])
    } else { 
      n.f.mods <- 1
    }
    
    # Number of reverse mods
    if (Use.Reverse.Mod) {
      n.r.mods <- length(strsplit(Reverse.Mods, ", ")[[1]])
    } else { 
      n.r.mods <- 1
    }
    
    div(
      pickerInput(
        inputId = "reaction_mass_action_wReg_reverisble_edit",
        label = "Reversability?",
        choices = c("Reversible" = "both_directions",
                    "Irreversible" = 'forward_only'),
        choicesOpt =
          list(icon = c(
            "glyphicon glyphicon-resize-horizontal",
            "glyphicon glyphicon-arrow-right"
          )),
        selected = arrow_type
      ),
      hr(),
      prettyCheckbox(inputId = "CB_MAwR_chem_modifier_forward_edit",
                     label = "Add Forward Regulator(s)",
                     value = Use.Forward.Mod),
      conditionalPanel(
        condition = "input.CB_MAwR_chem_modifier_forward_edit",
        numericInput(
          inputId = "NI_MAwR_n_forward_regulators_edit",
          label = "# of Forward Regulators",
          value = n.f.mods,
          min = 1,
          step = 1
        )
      ),
      conditionalPanel(
        condition = "input.reaction_mass_action_wReg_reverisble_edit ==
                                                            'both_directions'",
        prettyCheckbox(
          inputId = "CB_MAwR_chem_modifier_reverse_edit",
          label = "Add Reverse Regulator(s)",
          value = Use.Reverse.Mod
        ),
        conditionalPanel(
          condition =
            "input.CB_MAwR_chem_modifier_reverse_edit",
          numericInput(
            inputId =
              "NI_MAwR_n_reverse_regulators_edit",
            label = "# of Reverse Regulators",
            value = n.r.mods,
            min = 1,
            step = 1
          )
        )
      )
    )
    
  }
  else if (eqn.reaction.law == "synthesis") {
    syn <- rv.REACTIONS$synthesis[[eqn.ID]]
    
    ID               <- syn$ID
    law              <- syn$Reaction.Law
    VarSyn           <- syn$VarSyn
    VarSyn.id        <- syn$VarSyn.id
    Rate.Constant    <- syn$Rate.Constant
    Rate.Constant.id <- syn$Rate.Constant.id
    Factor           <- syn$Factor
    Factor.id        <- syn$Factor.id
    
    if (is.na(Factor)) {use.factor <- FALSE} else {use.factor <- TRUE}
    
    div(
      prettyCheckbox(
            inputId = "CB_synthesis_factor_checkbox_edit",
            label = "Factor Driving Synthesis?",
            value = use.factor
          )
    )
  }
  else if (eqn.reaction.law == "degradation_rate") {
    degInfo   <- rv.REACTIONS$degradation.by.rate[[eqn.ID]]
    
    ID         <- degInfo$ID
    law        <- degInfo$Reaction.Law
    VarDeg     <- degInfo$VarDeg
    VarDeg.id  <- degInfo$VarDeg.id
    ConcDep    <- degInfo$ConcDep
    RC         <- degInfo$Rate.Constant
    RC.id      <- degInfo$Rate.Constant.id
    Product    <- degInfo$Products
    Product.id <- degInfo$Products.id
    
    prod.exists <- ifelse(is.na(Product), FALSE, TRUE)
    if (prod.exists) {
      num.prods <- length(strsplit(Product, ", ")[[1]])
    }
    
    div(
      prettyCheckbox(
        inputId = "CB_degradation_rate_toProducts_edit",
        label = "Degrade Into Products?",
        value = prod.exists
      ),
      conditionalPanel(
        condition = "input.CB_degradation_rate_toProducts_edit",
        numericInput(
          inputId = "NI_degradation_rate_num_products_edit",
          label = "Number of Products",
          value = num.prods,
          min = 1,
          step = 1
        )
      )
    )
  }
  else if (eqn.reaction.law == "degradation_by_enzyme") {
    degInfo   <- rv.REACTIONS$degradation.by.enzyme[[eqn.ID]]
    
    ID         <- degInfo$ID
    law        <- degInfo$Reaction.Law
    VarDeg     <- degInfo$VarDeg
    VarDeg.id  <- degInfo$VarDeg.id
    RC         <- degInfo$Rate.Constant
    RC.id      <- degInfo$Rate.Constant.id
    UseVmax    <- degInfo$UseVmax
    Km         <- degInfo$Km
    Km.id      <- degInfo$Km.id
    Vmax       <- degInfo$Vmax
    Vmax.id    <- degInfo$Vmax.id
    Enzyme     <- degInfo$Enzyme
    Enzyme.id  <- degInfo$Enzyme.id
    kcat       <- degInfo$kcat
    kcat.id    <- degInfo$kcat.id
    Product    <- degInfo$Products
    Product.id <- degInfo$Products.id
    
    prod.exists <- ifelse(is.na(Product), FALSE, TRUE)
    if (prod.exists) {
      num.prods <- length(strsplit(Product, ", ")[[1]])
    }
    
    div(
      prettyCheckbox(
        inputId = "CB_degradation_enzyme_toProducts_edit",
        label = "Degrade Into Products?",
        value = prod.exists
      ),
      conditionalPanel(
        condition = "input.CB_degradation_enzyme_toProducts_edit",
        numericInput(
          inputId = "NI_degradation_enzyme_num_products_edit",
          label = "Number of Products",
          value = num.prods,
          min = 1,
          step = 1
        ),
        prettyCheckbox(
          inputId = "CB_degradation_enzyme_relative_formation_edit",
          label = "Relative Formation",
          value = if ("krel" %in% names(degInfo) && !is.na(degInfo$krel) && degInfo$krel != "") TRUE else FALSE
        )
      ),
      hr(),
      prettyCheckbox(inputId = "CB_degradation_enzyme_useVmax_edit",
                     label = "Use Vmax",
                     value = UseVmax)
    )

  } 
  else if (eqn.reaction.law == "michaelis_menten") {
    Info   <- rv.REACTIONS$michaelisMenten[[eqn.ID]]
    
    ID            <- Info$ID
    law           <- Info$Reaction.Law
    Substrate     <- Info$Substrate
    Substrate.id  <- Info$Substrate.id
    Product       <- Info$Product
    Product.id    <- Info$Product.id
    UseVmax       <- Info$UseVmax
    Km            <- Info$Km
    Km.id         <- Info$Km.id
    Vmax          <- Info$Vmax
    Vmax.id       <- Info$Vmax.id
    Enzyme        <- Info$Enzyme
    Enzyme.id     <- Info$Enzyme.id
    kcat          <- Info$kcat
    kcat.id       <- Info$kcat.id
    
    div (
      prettyCheckbox(
        inputId = "CB_michaelis_menten_useVmax_edit",
        label = "Use Vmax",
        value = UseVmax
      ) 
    )
  }
})

# Main Box (Right): RenderUI ---------------------------------------------------
output$eqnCreate_edit_rending_mainbar <- renderUI({
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- rv.REACTIONS$reactions[[eqn.num]]
  
  # Unpack Equation Information
  eqn.ID               <- eqn.row$ID            
  eqn.display.type     <- eqn.row$Eqn.Display.Type 
  eqn.reaction.law     <- eqn.row$Reaction.Law    
  eqn.species          <- eqn.row$Species          
  eqn.reactants        <- eqn.row$Reactants        
  eqn.products         <- eqn.row$Products         
  eqn.Modifiers        <- eqn.row$Modifiers  
  eqn.parameters       <- eqn.row$Parameters       
  eqn.compartment      <- eqn.row$Compartment      
  eqn.description      <- eqn.row$Description      
  eqn.species.id       <- eqn.row$Species.id      
  eqn.reactants.id     <- eqn.row$Reactants.id     
  eqn.products.id      <- eqn.row$Products.id      
  eqn.modifiers.id     <- eqn.row$Modifiers.id     
  eqn.parameters.id    <- eqn.row$Parameters.id   
  eqn.compartment.id   <- eqn.row$Compartment.id   
  eqn.equation.text    <- eqn.row$Equation.Text    
  eqn.equation.latex   <- eqn.row$Equation.Latex   
  eqn.equation.mathjax <- eqn.row$Equation.MathJax 
  eqn.string.rate.law  <- eqn.row$String.Rate.Law  
  eqn.pretty.rate.law  <- eqn.row$Pretty.Rate.Law  
  eqn.latex.rate.law   <- eqn.row$Latex.Rate.Law   
  eqn.mathjax.rate.law <- eqn.row$MathJax.Rate.Law 
  eqn.mathml.rate.law  <- eqn.row$MathMl.Rate.Law 
  eqn.reversible       <- eqn.row$Reversible  
  
  if (eqn.reaction.law == "mass_action") {
    # Extract chem information
    chemInfo <- rv.REACTIONS$massAction[[eqn.ID]]
    
    ID               <- chemInfo$ID
    law              <- chemInfo$Reaction.Law
    r.stoichiometry  <- str_split(chemInfo$r.stoichiometry, ", ")[[1]]
    Reactants        <- str_split(chemInfo$Reactants,  ", ")[[1]]
    p.stoichiometry  <- str_split(chemInfo$p.stoichiometry, ", ")[[1]]
    Products         <- str_split(chemInfo$Products,  ", ")[[1]] 
    Reactants.id     <- str_split(chemInfo$Reactants.id, ", ")[[1]]
    Products.id      <- str_split(chemInfo$Products.id, ", ")[[1]]
    arrow_type       <- chemInfo$Reversible
    kf               <- chemInfo$kf
    kr               <- chemInfo$kr
    kf.id            <- chemInfo$kf.id
    kr.id            <- chemInfo$kr.id

    number.reactants <- length(Reactants)
    number.products  <- length(Products)
    
    # Get parameter values
    kf.value <- rv.PARAMETERS$parameters[[kf.id]]$Value
    if (!is.na(kr.id)) {
      kr.value <- rv.PARAMETERS$parameters[[kr.id]]$Value
    } else {
      kr.value <- 0
    }
    
    
    # Render Ui
    div(
      fluidRow(
        column(
          width = 3, 
          numericInput(
            inputId = "NI_mass_action_num_reactants_edit",
            label = "Number of Reactants",
            value = number.reactants,
            min = 1,
            step = 1)
        ), 
        column(
          width = 3,
          numericInput(
            inputId = "NI_mass_action_num_products_edit",
            label = "Number of Products",
            value = number.products,
            min = 1,
            step = 1
          )
        )
      ),
      fluidRow(
        column(
          style = "border-right: 1px solid #e5e5e5; padding-right:20px",
          width = 4,
          lapply(seq(number.reactants), function(i){
            div(
              HTML(paste0("<b>Reactant ", as.character(i), "</b>")),
              splitLayout(
                numericInput(
                  inputId = paste0("NI_MA_r_stoichiometry_edit_", 
                                   as.character(i)),
                  label = NULL,
                  value = as.numeric(r.stoichiometry[i]),
                  min = 1,
                  step = 1),
                pickerInput(
                  inputId = paste0("PI_MA_reactant_edit_", as.character(i)),
                  label = NULL,
                  choices = sort(rv.SPECIES$df.by.compartment$Name),
                  selected = Reactants[i],
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = "startsWith",
                                          dropupAuto = FALSE)
                ),
                cellWidths = c("25%", "75%")
              )
            )
          })
        ), #end Column
        column(
          style = "border-right: 1px solid #e5e5e5; 
                 padding-right: 20px; 
                 padding-left: 20px;",
          width = 4,
          lapply(seq(number.products), function(i){
            div(
              HTML(paste0("<b>Product ", as.character(i), "</b>")),
              splitLayout(
                numericInput(
                  inputId = paste0("NI_MA_p_stoichiometry_edit_", 
                                   as.character(i)),
                  label = NULL,
                  value = p.stoichiometry[i],
                  min = 1,
                  step = 1),
                pickerInput(
                  inputId = paste0("PI_MA_product_edit_", 
                                   as.character(i)),
                  label = NULL,
                  choices = sort(rv.SPECIES$df.by.compartment$Name),
                  selected = Products[i],
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = "startsWith",
                                          dropupAuto = FALSE)
                ),
                cellWidths = c("25%", "75%")
              )
            )
          })
        ), #end Column
        column(
          style = "padding-left: 20px; padding-right: 0px",
          width = 3,
          textInput(
            inputId = "TI_mass_action_forward_k_edit",
            label = "Forward Rate Constant",
            value = kf
          ),
          conditionalPanel(
            condition = 
              "input.PI_mass_action_reverisble_option_edit== 'both_directions'",
            textInput(
              inputId = "TI_mass_action_reverse_k_edit",
              label = "Reverse Rate Constant",
              value = kr
            )
          )
        ), #end column
        column(
          style = "padding-left: 0px",
          width = 1,
          textInput(
            inputId = "TI_mass_action_forward_k_value_edit",
            label = "Value",
            value = kf.value
          ),
          conditionalPanel(
            condition = 
              "input.PI_mass_action_reverisble_option_edit== 'both_directions'",
            textInput(
              inputId = "TI_mass_action_reverse_k_value_edit",
              label = "Value",
              value = kr.value)
          )
        ),
        tags$head(tags$style("#TI_mass_action_forward_k_edit
                             {margin-top: -7px;}")),
        tags$head(tags$style("#TI_mass_action_reverse_k_edit
                             {margin-top: -7px;}")),
        tags$head(
          tags$style("#TI_mass_action_forward_k_value_edit
                     {margin-top: -7px;}")),
        tags$head(
          tags$style("#TI_mass_action_reverse_k_value_edit
                     {margin-top: -7px;}"))
      )
      
    )
  }
  else if (eqn.reaction.law == "mass_action_w_reg") {
    
    chemInfo <- rv.REACTIONS$massActionwReg[[eqn.ID]]
    
    ID               <- chemInfo$ID
    law              <- chemInfo$Reaction.Law
    r.stoichiometry  <- str_split(chemInfo$r.stoichiometry, ", ")[[1]]
    Reactants        <- str_split(chemInfo$Reactants,  ", ")[[1]]
    p.stoichiometry  <- str_split(chemInfo$p.stoichiometry, ", ")[[1]]
    Products         <- str_split(chemInfo$Products,  ", ")[[1]] 
    Reactants.id     <- str_split(chemInfo$Reactants.id, ", ")[[1]]
    Products.id      <- str_split(chemInfo$Products.id, ", ")[[1]]
    arrow_type       <- chemInfo$Reversible
    kf               <- chemInfo$kf
    kr               <- chemInfo$kr
    kf.id            <- chemInfo$kf.id
    kr.id            <- chemInfo$kr.id
    Use.Forward.Mod  <- chemInfo$Use.Forward.Mod
    Forward.Mods     <- str_split(chemInfo$Forward.Mods, ", ")[[1]]
    Forward.Mods.id  <- str_split(chemInfo$Forward.Mods.id, ", ")[[1]]
    Forward.Pars     <- str_split(chemInfo$Forward.Pars, ", ")[[1]]
    Forward.Pars.id  <- str_split(chemInfo$Forward.Pars.id, ", ")[[1]]
    Use.Reverse.Mod  <- chemInfo$Use.Reverse.Mod
    Reverse.Mods     <- str_split(chemInfo$Reverse.Mods, ", ")[[1]]
    Reverse.Mods.id  <- str_split(chemInfo$Reverse.Mods.id, ", ")[[1]]
    Reverse.Pars     <- str_split(chemInfo$Reverse.Pars, ", ")[[1]]
    Reverse.Pars.id  <- str_split(chemInfo$Reverse.Pars.id, ", ")[[1]]
    
    # Number of forward mods
    if (Use.Forward.Mod) {
      n.f.mods <- length(strsplit(Forward.Mods, ", ")[[1]])
    } else { 
      n.f.mods <- 1
    }
    
    # Number of reverse mods
    if (Use.Reverse.Mod) {
      n.r.mods <- length(strsplit(Reverse.Mods, ", ")[[1]])
    } else { 
      n.r.mods <- 1
    }
    
    number.reactants <- length(Reactants)
    number.products  <- length(Products)
    
    # Get parameter values
    kf.value <- rv.PARAMETERS$parameters[[kf.id]]$Value
    if (!is.na(kr.id)) {
      kr.value <- rv.PARAMETERS$parameters[[kr.id]]$Value
    } else {
      kr.value <- 0
    }
    
    div(
      fluidRow(
        column(
          style = "border-right: 1px solid #e5e5e5; padding-right:20px",
          width = 4,
          lapply(seq(number.reactants), function(i){
            div(
              HTML(paste0("<b>Reactant ", as.character(i), "</b>")),
              splitLayout(
                numericInput(
                  inputId = paste0("NI_MAwR_r_stoichiometry_edit_", 
                                   as.character(i)),
                  label = NULL,
                  value = r.stoichiometry[i],
                  min = 1,
                  step = 1),
                pickerInput(
                  inputId = paste0("PI_MAwR_reactant_edit_", as.character(i)),
                  label = NULL,
                  choices = sort(rv.SPECIES$df.by.compartment$Name),
                  selected = Reactants[i],
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = "startsWith",
                                          dropupAuto = FALSE)
                ),
                cellWidths = c("25%", "75%")
              )
            )
          })
        ), #end Column
        column(
          style = "border-right: 1px solid #e5e5e5; 
               padding-right: 20px; 
               padding-left: 20px;",
          width = 4,
          lapply(seq(number.products), function(i){
            div(
              HTML(paste0("<b>Product ", as.character(i), "</b>")),
              splitLayout(
                numericInput(
                  inputId = paste0("NI_MAwR_p_stoichiometry_edit_", 
                                   as.character(i)),
                  label = NULL,
                  value = p.stoichiometry[i],
                  min = 1,
                  step = 1),
                pickerInput(
                  inputId = paste0("PI_MAwR_product_edit_", as.character(i)),
                  label = NULL,
                  choices = sort(rv.SPECIES$df.by.compartment$Name),
                  selected = Products[i],
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = "startsWith",
                                          dropupAuto = FALSE)
                ),
                cellWidths = c("25%", "75%")
              )
            )
          })
        ), #end Column
        column(
          style = "padding-left: 20px; padding-right: 0px",
          width = 3,
          conditionalPanel(
            condition = "!input.CB_MAwR_chem_modifier_forward_edit",
            textInput(
              inputId = "TI_MAwR_forward_k_edit",
              label = "Forward Rate Constant",
              value = kf
              )
          ),
          conditionalPanel(
            condition = 
             "input.reaction_mass_action_wReg_reverisble_edit == 
                                                           'both_directions' && 
             !input.CB_MAwR_chem_modifier_reverse_edit",
            textInput(
              inputId = "TI_MAwR_reverse_k_edit",
              label = "Reverse Rate Constant",
              value = kr
              )
          )
        ),
        column(
          style = "padding-left: 0px",
          width = 1,
          conditionalPanel(
            condition = "!input.CB_MAwR_chem_modifier_forward_edit",
            textInput(
              inputId = "TI_MAwR_forward_k_value_edit",
              label = "Value",
              value = kf.value
            )
          ),
          conditionalPanel(
            condition = 
              "input.reaction_mass_action_wReg_reverisble_edit == 
            'both_directions' && 
             !input.CB_MAwR_chem_modifier_reverse_edit",
            textInput(
              inputId = "TI_MAwR_reverse_k_value_edit",
              label = "Value",
              value = kr.value)
          )
        ),
        tags$head(
          tags$style("#TI_MAwR_forward_k_value_edit {margin-top: -7px;}")),
        tags$head(
          tags$style("#TI_MAwR_reverse_k_value_edit {margin-top: -7px;}")),
        tags$head(
          tags$style("#TI_MAwR_reverse_k_edit {margin-top: -7px;}")),
        tags$head(
          tags$style("#TI_MAwR_forward_k_edit {margin-top: -7px;}"))
      ), #end fluidRow`
      conditionalPanel(
        condition = "input.CB_MAwR_chem_modifier_forward_edit || 
                     input.CB_MAwR_chem_modifier_reverse_edit",
        hr()
      ),
      fluidRow(
        column(
          width = 3,
          conditionalPanel(
            condition = "input.CB_MAwR_chem_modifier_forward_edit",
            lapply(seq(n.f.mods), function(i){
              pickerInput(
                inputId = paste0("PI_MAwR_forward_regulator_edit_", 
                                 as.character(i)),
                label = paste0("Forward Regulator ", as.character(i)),
                choices = sort(c(rv.SPECIES$df.by.compartment$Name,
                                 rv.PARAMETERS$parameters.names)),
                selected = Forward.Mods[i],
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith"))
            })
          )
        ),
        column(
          width = 3,
          conditionalPanel(
            condition = "input.CB_MAwR_chem_modifier_forward_edit",
            lapply(seq(n.f.mods), function(i){
              textInput(
                inputId = paste0("TI_MAwR_forward_regulator_RC_edit_", 
                                 as.character(i)),
                label = "Rate Constant",
                value = Forward.Pars[i]
              )
            })
          )
        ),
        column(
          width = 3,
          conditionalPanel(
            condition = "input.CB_MAwR_chem_modifier_forward_edit",
            lapply(seq(n.f.mods), function(i){
              textInput(
                inputId = paste0("TI_MAwR_forward_regulator_RC_value_edit_",
                                 as.character(i)),
                label = "Value",
                value = rv.PARAMETERS$parameters[[Forward.Pars.id[i]]]$Value
              )
            })
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          conditionalPanel(
            condition = "input.CB_MAwR_chem_modifier_reverse_edit",
            lapply(seq(n.r.mods), function(i){
              pickerInput(
                inputId = paste0("PI_MAwR_reverse_regulator_edit_", 
                                 as.character(i)),
                label = paste0("Reverse Regulator ", as.character(i)),
                choices = sort(c(rv.SPECIES$df.by.compartment$Name,
                                 rv.PARAMETERS$parameters.names)),
                selected = Reverse.Mods[i],
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith")
              )
            })
          )
        ),
        column(
          width = 3,
          conditionalPanel(
            condition = "input.CB_MAwR_chem_modifier_reverse_edit",
            lapply(seq(n.r.mods), function(i){
              textInput(
                inputId = paste0("TI_MAwR_reverse_regulator_RC_edit_", 
                                 as.character(i)),
                label = "Rate Constant",
                value = Reverse.Pars[i]
              )
            })
          )
        ),
        column(
          width = 3,
          conditionalPanel(
            condition = "input.CB_MAwR_chem_modifier_reverse_edit",
            lapply(seq(n.r.mods), function(i){
              textInput(
                inputId = paste0("TI_MAwR_reverse_regulator_RC_value_edit_",
                                 as.character(i)),
                label = "Value",
                value = rv.PARAMETERS$parameters[[Reverse.Pars.id[i]]]$Value
              )
            })
          )
        )
      )
    )
  }
  else if (eqn.reaction.law == "exponential_growth") {
    growthInfo <- rv.REACTIONS$exponentialGrowth[[eqn.ID]]
    
    species    <- growthInfo$Species
    mu         <- growthInfo$Mu
    mu.id      <- growthInfo$Mu.id
    mu.value   <- rv.PARAMETERS$parameters[[mu.id]]$Value
    
    div(
      fluidRow(
        column(
          width = 4,
          pickerInput(
            inputId = "PI_exp_growth_species_edit",
            label   = "Growing Species",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = species,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          textInput(
            inputId = "TI_exp_growth_mu_edit",
            label = "Growth Rate Parameter (mu)",
            value = mu
          )
        ),
        column(
          width = 3,
          numericInput(
            inputId = "NI_exp_growth_mu_value_edit",
            label = "Value",
            value = mu.value,
            min = 0,
            step = 0.01
          )
        )
      )
    )
  }
  else if (eqn.reaction.law == "logistic_competition") {
    info <- rv.REACTIONS$logisticCompetition[[eqn.ID]]
    species.x   <- info$Species.X
    species.y   <- info$Species.Y
    r.x         <- info$r.x
    alpha.xy    <- info$alpha.xy
    Kc          <- info$Kc
    
    # Check if single species mode
    single.species.mode <- if (!is.null(info$Single.Species.Mode)) info$Single.Species.Mode else FALSE
    
    r.x.val      <- rv.PARAMETERS$parameters[[info$r.x.id]]$Value
    alpha.xy.val <- rv.PARAMETERS$parameters[[info$alpha.xy.id]]$Value
    Kc.val       <- rv.PARAMETERS$parameters[[info$Kc.id]]$Value
    
    if (single.species.mode) {
      # Single species mode UI
      div(
        fluidRow(
          column(
            width = 4,
            pickerInput("PI_log_comp_species_x_edit", "Species X (growing competitively)",
                        choices = sort(rv.SPECIES$df.by.compartment$Name),
                        selected = species.x,
                        options = pickerOptions(liveSearch = TRUE,
                                                liveSearchStyle = "startsWith"))
          ),
          column(
            width = 4,
            pickerInput("PI_log_comp_species_y_edit", "Species Y (competitor only)",
                        choices = sort(rv.SPECIES$df.by.compartment$Name),
                        selected = species.y,
                        options = pickerOptions(liveSearch = TRUE,
                                                liveSearchStyle = "startsWith"))
          )
        ),
        fluidRow(
          column(width = 3, textInput("TI_log_comp_r_x_edit", "r_x", value = r.x)),
          column(width = 3, numericInput("NI_log_comp_r_x_value_edit", "Value", value = r.x.val, min = 0, step = 0.01)),
          column(width = 3, textInput("TI_log_comp_alpha_xy_edit", "alpha_xy", value = alpha.xy)),
          column(width = 3, numericInput("NI_log_comp_alpha_xy_value_edit", "Value", value = alpha.xy.val, min = 0, step = 0.01))
        ),
        fluidRow(
          column(width = 3, textInput("TI_log_comp_Kc_edit", "Kc (carrying capacity)", value = Kc)),
          column(width = 3, numericInput("NI_log_comp_Kc_value_edit", "Value", value = Kc.val, min = 0.0001, step = 0.1))
        )
      )
    } else {
      # Both species mode UI
      r.y         <- info$r.y
      alpha.yx    <- info$alpha.yx
      r.y.val      <- rv.PARAMETERS$parameters[[info$r.y.id]]$Value
      alpha.yx.val <- rv.PARAMETERS$parameters[[info$alpha.yx.id]]$Value
      
      div(
        fluidRow(
          column(
            width = 4,
            pickerInput("PI_log_comp_species_x_edit", "Species X",
                        choices = sort(rv.SPECIES$df.by.compartment$Name),
                        selected = species.x,
                        options = pickerOptions(liveSearch = TRUE,
                                                liveSearchStyle = "startsWith"))
          ),
          column(
            width = 4,
            pickerInput("PI_log_comp_species_y_edit", "Species Y",
                        choices = sort(rv.SPECIES$df.by.compartment$Name),
                        selected = species.y,
                        options = pickerOptions(liveSearch = TRUE,
                                                liveSearchStyle = "startsWith"))
          )
        ),
        fluidRow(
          column(width = 3, textInput("TI_log_comp_r_x_edit", "r_x", value = r.x)),
          column(width = 3, numericInput("NI_log_comp_r_x_value_edit", "Value", value = r.x.val, min = 0, step = 0.01)),
          column(width = 3, textInput("TI_log_comp_r_y_edit", "r_y", value = r.y)),
          column(width = 3, numericInput("NI_log_comp_r_y_value_edit", "Value", value = r.y.val, min = 0, step = 0.01))
        ),
        fluidRow(
          column(width = 3, textInput("TI_log_comp_alpha_xy_edit", "alpha_xy", value = alpha.xy)),
          column(width = 3, numericInput("NI_log_comp_alpha_xy_value_edit", "Value", value = alpha.xy.val, min = 0, step = 0.01)),
          column(width = 3, textInput("TI_log_comp_alpha_yx_edit", "alpha_yx", value = alpha.yx)),
          column(width = 3, numericInput("NI_log_comp_alpha_yx_value_edit", "Value", value = alpha.yx.val, min = 0, step = 0.01))
        ),
        fluidRow(
          column(width = 3, textInput("TI_log_comp_Kc_edit", "Kc (carrying capacity)", value = Kc)),
          column(width = 3, numericInput("NI_log_comp_Kc_value_edit", "Value", value = Kc.val, min = 0.0001, step = 0.1))
        )
      )
    }
  }
  else if (eqn.reaction.law == "monod_growth") {
    info <- rv.REACTIONS$monodGrowth[[eqn.ID]]
    species    <- info$Species
    substrate  <- info$Substrate
    mu_max     <- info$Mu_max
    K_s        <- info$K_s
    
    mu_max.val <- rv.PARAMETERS$parameters[[info$Mu_max.id]]$Value
    K_s.val    <- rv.PARAMETERS$parameters[[info$K_s.id]]$Value
    
    div(
      fluidRow(
        column(
          width = 4,
          pickerInput("PI_monod_species_edit", "Growing Species (X)",
                      choices = sort(rv.SPECIES$df.by.compartment$Name),
                      selected = species,
                      options = pickerOptions(liveSearch = TRUE,
                                              liveSearchStyle = "startsWith"))
        ),
        column(
          width = 4,
          pickerInput("PI_monod_substrate_edit", "Substrate (S)",
                      choices = sort(rv.SPECIES$df.by.compartment$Name),
                      selected = substrate,
                      options = pickerOptions(liveSearch = TRUE,
                                              liveSearchStyle = "startsWith"))
        )
      ),
      fluidRow(
        column(width = 3, textInput("TI_monod_mu_max_edit", "mu_max", value = mu_max)),
        column(width = 3, numericInput("NI_monod_mu_max_value_edit", "Value", value = mu_max.val, min = 0, step = 0.01)),
        column(width = 3, textInput("TI_monod_K_s_edit", "K_s (half-saturation)", value = K_s)),
        column(width = 3, numericInput("NI_monod_K_s_value_edit", "Value", value = K_s.val, min = 0.0001, step = 0.01))
      )
    )
  }
  else if (eqn.reaction.law == "competitive_monod") {
    info <- rv.REACTIONS$competitiveMonod[[eqn.ID]]
    species.x   <- info$Species.X
    species.y   <- info$Species.Y
    substrate   <- info$Substrate
    mu_max.x    <- info$mu_max.x
    mu_max.y    <- info$mu_max.y
    K_s.x       <- info$K_s.x
    K_s.y       <- info$K_s.y
    alpha.xy    <- info$alpha.xy
    alpha.yx    <- info$alpha.yx
    Kc          <- info$Kc
    Y_x         <- info$Y_x
    Y_y         <- info$Y_y
    single.species.mode <- if (!is.null(info$Single.Species.Mode)) info$Single.Species.Mode else FALSE
    no.substrate.restriction <- if (!is.null(info$No.Substrate.Restriction)) info$No.Substrate.Restriction else FALSE
    
    # Set checkbox values
    updatePrettyCheckbox(session, "CB_comp_monod_single_species_edit", value = single.species.mode)
    updatePrettyCheckbox(session, "CB_comp_monod_no_substrate_restriction_edit", value = no.substrate.restriction)
    
    mu_max.x.val <- rv.PARAMETERS$parameters[[info$mu_max.x.id]]$Value
    mu_max.y.val <- rv.PARAMETERS$parameters[[info$mu_max.y.id]]$Value
    K_s.x.val    <- rv.PARAMETERS$parameters[[info$K_s.x.id]]$Value
    K_s.y.val    <- rv.PARAMETERS$parameters[[info$K_s.y.id]]$Value
    alpha.xy.val <- rv.PARAMETERS$parameters[[info$alpha.xy.id]]$Value
    alpha.yx.val <- rv.PARAMETERS$parameters[[info$alpha.yx.id]]$Value
    Kc.val       <- rv.PARAMETERS$parameters[[info$Kc.id]]$Value
    Y_x.val      <- rv.PARAMETERS$parameters[[info$Y_x.id]]$Value
    Y_y.val      <- rv.PARAMETERS$parameters[[info$Y_y.id]]$Value
    
    div(
      fluidRow(
        column(width = 3, pickerInput("PI_comp_monod_species_x_edit", "Species X",
                    choices = sort(rv.SPECIES$df.by.compartment$Name),
                    selected = species.x,
                    options = pickerOptions(liveSearch = TRUE,
                                            liveSearchStyle = "startsWith"))),
        column(width = 3, pickerInput("PI_comp_monod_species_y_edit", "Species Y",
                    choices = sort(rv.SPECIES$df.by.compartment$Name),
                    selected = species.y,
                    options = pickerOptions(liveSearch = TRUE,
                                            liveSearchStyle = "startsWith"))),
        column(width = 3, pickerInput("PI_comp_monod_substrate_edit", "Substrate (S)",
                    choices = sort(rv.SPECIES$df.by.compartment$Name),
                    selected = substrate,
                    options = pickerOptions(liveSearch = TRUE,
                                            liveSearchStyle = "startsWith")))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_mu_max_x_edit", "mu_max_x", value = mu_max.x)),
        column(width = 3, numericInput("NI_comp_monod_mu_max_x_value_edit", "Value", value = mu_max.x.val, min = 0, step = 0.01)),
        column(width = 3, textInput("TI_comp_monod_mu_max_y_edit", "mu_max_y", value = mu_max.y)),
        column(width = 3, numericInput("NI_comp_monod_mu_max_y_value_edit", "Value", value = mu_max.y.val, min = 0, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_K_s_x_edit", "K_s_x", value = K_s.x)),
        column(width = 3, numericInput("NI_comp_monod_K_s_x_value_edit", "Value", value = K_s.x.val, min = 0.0001, step = 0.01)),
        column(width = 3, textInput("TI_comp_monod_K_s_y_edit", "K_s_y", value = K_s.y)),
        column(width = 3, numericInput("NI_comp_monod_K_s_y_value_edit", "Value", value = K_s.y.val, min = 0.0001, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_alpha_xy_edit", "alpha_xy", value = alpha.xy)),
        column(width = 3, numericInput("NI_comp_monod_alpha_xy_value_edit", "Value", value = alpha.xy.val, min = 0, step = 0.01)),
        column(width = 3, textInput("TI_comp_monod_alpha_yx_edit", "alpha_yx", value = alpha.yx)),
        column(width = 3, numericInput("NI_comp_monod_alpha_yx_value_edit", "Value", value = alpha.yx.val, min = 0, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_Kc_edit", "Kc (carrying capacity)", value = Kc)),
        column(width = 3, numericInput("NI_comp_monod_Kc_value_edit", "Value", value = Kc.val, min = 0.0001, step = 0.1)),
        column(width = 3, textInput("TI_comp_monod_Y_x_edit", "Y_x (yield)", value = Y_x)),
        column(width = 3, numericInput("NI_comp_monod_Y_x_value_edit", "Value", value = Y_x.val, min = 0.0001, step = 0.01))
      ),
      fluidRow(
        column(width = 3, textInput("TI_comp_monod_Y_y_edit", "Y_y (yield)", value = Y_y)),
        column(width = 3, numericInput("NI_comp_monod_Y_y_value_edit", "Value", value = Y_y.val, min = 0.0001, step = 0.01))
      )
    )
  }
  else if (eqn.reaction.law == "predator_prey") {
    info <- rv.REACTIONS$predatorPrey[[eqn.ID]]
    prey      <- info$Prey
    predator  <- info$Predator
    r         <- info$r
    a         <- info$a
    b         <- info$b
    d         <- info$d
    r.val     <- info$r.val
    a.val     <- info$a.val
    b.val     <- info$b.val
    d.val     <- info$d.val
    
    div(
      fluidRow(
        column(width = 4,
          pickerInput("PI_pred_prey_prey_edit", "Prey (X)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = prey,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith"))),
        column(width = 4,
          pickerInput("PI_pred_prey_predator_edit", "Predator (Y)",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = predator,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")))
      ),
      hr(),
      fluidRow(
        column(width = 3, textInput("TI_pred_prey_r_edit", "r (prey growth rate)", value = r)),
        column(width = 3, numericInput("NI_pred_prey_r_value_edit", "Value", value = r.val, min = 0, step = 0.01)),
        column(width = 3, textInput("TI_pred_prey_a_edit", "a (attack rate)", value = a)),
        column(width = 3, numericInput("NI_pred_prey_a_value_edit", "Value", value = a.val, min = 0, step = 0.0001))
      ),
      fluidRow(
        column(width = 3, textInput("TI_pred_prey_b_edit", "b (conversion rate)", value = b)),
        column(width = 3, numericInput("NI_pred_prey_b_value_edit", "Value", value = b.val, min = 0, step = 0.0001)),
        column(width = 3, textInput("TI_pred_prey_d_edit", "d (predator death rate)", value = d)),
        column(width = 3, numericInput("NI_pred_prey_d_value_edit", "Value", value = d.val, min = 0, step = 0.01))
      )
    )
  }
  else if (eqn.reaction.law == "synthesis") {
    
    syn <- rv.REACTIONS$synthesis[[eqn.ID]]
    
    ID               <- syn$ID
    law              <- syn$Reaction.Law
    VarSyn           <- syn$VarSyn
    VarSyn.id        <- syn$VarSyn.id
    Rate.Constant    <- syn$Rate.Constant
    Rate.Constant.id <- syn$Rate.Constant.id
    Factor           <- syn$Factor
    Factor.id        <- syn$Factor.id
    
    if (is.na(Factor)) {use.factor <- FALSE} else {use.factor <- TRUE}
    
    div(
      conditionalPanel(
        condition = "!input.CB_synthesis_factor_checkbox_edit",
        fluidRow(
          column(
            width = 3,
            pickerInput(
              inputId = "PI_synthesis_rate_var_edit",
              label   = "Species to synthesize",
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              selected = VarSyn,
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith") 
            )
          )
        ),
        fluidRow(
          column(
            width = 3,
            textInput(
              inputId = "TI_synthesis_rate_RC_edit",
              label = "Rate Constant",
              value = Rate.Constant
              
            )
          ),
          column(
            width = 3, 
            textInput(
              inputId = "TI_synthesis_rate_RC_value_edit",
              label = "Value",
              value = rv.PARAMETERS$parameters[[Rate.Constant.id]]$Value
            )
          )
        )
      ), 
      conditionalPanel(
        condition = "input.CB_synthesis_factor_checkbox_edit",
        fluidRow(
          column(
            width = 3,
            pickerInput(
              inputId = "PI_synthesis_byFactor_var_edit",
              label   = "Species to synthesize",
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              selected = VarSyn,
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith") 
            )
          ),
          column(
            width = 3, 
            pickerInput(
              inputId = "PI_synthesis_byFactor_factor_edit",
              label = "Factor causing synthesis",
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              selected = Factor
            )
          )
        ),
        fluidRow(
          column(
            width = 3, 
            textInput(
              inputId = "TI_synthesis_byFactor_RC",
              label = "Rate Constant",
              value = Rate.Constant
            )
          ),
          column(
            width = 3, 
            textInput(
              inputId = "TI_synthesis_byFactor_RC_value",
              label = "Value",
              value = rv.PARAMETERS$parameters[[Rate.Constant.id]]$Value
            )
          )
        )
      )
    )
  }
  else if (eqn.reaction.law == "degradation_rate") {
    degInfo   <- rv.REACTIONS$degradation.by.rate[[eqn.ID]]
    
    ID         <- degInfo$ID
    law        <- degInfo$Reaction.Law
    VarDeg     <- degInfo$VarDeg
    VarDeg.id  <- degInfo$VarDeg.id
    ConcDep    <- degInfo$ConcDep
    RC         <- degInfo$Rate.Constant
    RC.id      <- degInfo$Rate.Constant.id
    Product    <- degInfo$Products
    Product.id <- degInfo$Products.id
    krel       <- if ("krel" %in% names(degInfo)) degInfo$krel else NA
    krel.id    <- if ("krel.id" %in% names(degInfo)) degInfo$krel.id else NA
    use.relative.formation <- !is.na(krel) && !is.na(krel.id)
    
    prod.exists <- ifelse(is.na(Product), FALSE, TRUE)
    if (prod.exists) {
      num.prods <- length(strsplit(Product, ", ")[[1]])
    }
    
    div(
      fluidRow(
        column(
          width = 4,
          pickerInput(
            inputId = "PI_degradation_rate_species_edit",
            label   = "Species to degrade",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = VarDeg,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith") 
          )
        ),
        column(
          width = 8,
          conditionalPanel(
            condition = "input.CB_degradation_rate_toProducts_edit",
            fluidRow(
              column(
                width = 12,
                prettyCheckbox(
                  inputId = "CB_degradation_rate_relative_formation_edit",
                  label = "Relative Formation",
                  value = use.relative.formation
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                lapply(
                  seq(input$NI_degradation_rate_num_products_edit), function(i){
                    pickerInput(
                      inputId = paste0("PI_degradation_rate_product_edit_", 
                                       as.character(i)),
                      label = paste0("Product ", as.character(i)),
                      choices = sort(rv.SPECIES$df.by.compartment$Name),
                      selected = Product[i],
                      options = pickerOptions(liveSearch = TRUE,
                                              liveSearchStyle = "startsWith"))
                  }
                )
              ),
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.CB_degradation_rate_relative_formation_edit",
                  fluidRow(
                    column(
                      width = 12,
                      textInput(
                        inputId = "TI_degradation_rate_krel_edit",
                        label = "krel (product yield fraction)",
                        value = if (!is.na(krel)) krel else "krel"
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      numericInput(
                        inputId = "NI_degradation_rate_krel_value_edit",
                        label = "Value (0-1)",
                        value = if (!is.na(krel.id)) rv.PARAMETERS$parameters[[krel.id]]$Value else 0.1,
                        min = 0,
                        max = 1,
                        step = 0.01
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 8,
          splitLayout(
            textInput(
              inputId = "TI_degradation_rate_RC_edit",
              label = "Rate Constant",
              value = RC
            ),
            textInput(
              inputId = "TI_degradation_rate_RC_value_edit",
              label = "Value",
              value = rv.PARAMETERS$parameters[[RC.id]]$Value
            ),
            div(
              style = "padding-top:38px; padding-left:15px;",
              checkboxInput(
                inputId = "CB_degradation_rate_conc_dependent_edit",
                label = "Concentration Dependent",
                value = ConcDep)
            )
          )
        )  
      )
    )
  }
  else if (eqn.reaction.law == "degradation_by_enzyme") {
    degInfo   <- rv.REACTIONS$degradation.by.enzyme[[eqn.ID]]
    
    ID         <- degInfo$ID
    law        <- degInfo$Reaction.Law
    VarDeg     <- degInfo$VarDeg
    VarDeg.id  <- degInfo$VarDeg.id
    RC         <- degInfo$Rate.Constant
    RC.id      <- degInfo$Rate.Constant.id
    UseVmax    <- degInfo$UseVmax
    Km         <- degInfo$Km
    Km.id      <- degInfo$Km.id
    Vmax       <- degInfo$Vmax
    Vmax.id    <- degInfo$Vmax.id
    Enzyme     <- degInfo$Enzyme
    Enzyme.id  <- degInfo$Enzyme.id
    kcat       <- degInfo$kcat
    kcat.id    <- degInfo$kcat.id
    Product    <- degInfo$Products
    Product.id <- degInfo$Products.id
    krel       <- if ("krel" %in% names(degInfo)) degInfo$krel else NA
    krel.id    <- if ("krel.id" %in% names(degInfo)) degInfo$krel.id else NA
    use.relative.formation <- !is.na(krel) && !is.na(krel.id)
    
    prod.exists <- ifelse(is.na(Product), FALSE, TRUE)
    if (prod.exists) {
      num.prods <- length(strsplit(Product, ", ")[[1]])
      Product <- strsplit(Product, ", ")[[1]]
    }
    
    div(
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "PI_degradation_enzyme_species_edit",
            label   = "Species to degrade",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = VarDeg,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith") 
          ),
          conditionalPanel(
            condition = "!input.CB_degradation_enzyme_useVmax_edit",
            pickerInput(
              inputId = "PI_degradation_enzyme_enzyme_edit",
              label = "Enzyme",
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              selected = Enzyme
            )
          )
        ),
        column(
          width = 9,
          conditionalPanel(
            condition = "input.CB_degradation_enzyme_toProducts_edit",
            fluidRow(
              column(
                width = 12,
                prettyCheckbox(
                  inputId = "CB_degradation_enzyme_relative_formation_edit",
                  label = "Relative Formation",
                  value = use.relative.formation
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                lapply(
                  seq(input$NI_degradation_enzyme_num_products_edit), function(i){
                    pickerInput(
                      inputId = paste0("PI_degradation_enzyme_product_edit_", 
                                       as.character(i)),
                      label = paste0("Product ", as.character(i)),
                      choices = sort(rv.SPECIES$df.by.compartment$Name),
                      selected = if (prod.exists && i <= length(Product)) Product[i] else NULL,
                      options = pickerOptions(liveSearch = TRUE,
                                              liveSearchStyle = "startsWith"))
                  }
                )
              ),
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.CB_degradation_enzyme_relative_formation_edit",
                  fluidRow(
                    column(
                      width = 12,
                      textInput(
                        inputId = "TI_degradation_enzyme_krel_edit",
                        label = "krel (product yield fraction)",
                        value = if (!is.na(krel)) krel else "krel"
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      width = 12,
                      numericInput(
                        inputId = "NI_degradation_enzyme_krel_value_edit",
                        label = "Value (0-1)",
                        value = if (!is.na(krel.id) && krel.id %in% names(rv.PARAMETERS$parameters)) {
                          rv.PARAMETERS$parameters[[krel.id]]$Value
                        } else 0.1,
                        min = 0,
                        max = 1,
                        step = 0.01
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      hr(),
      conditionalPanel(
        condition = "!input.CB_degradation_enzyme_useVmax_edit",
        fluidRow(
          column(
            style = "padding-right: 0px;",
            width = 3,
            textInput(
              inputId = "TI_degradation_enzyme_kcat_edit",
              label = "kcat",
              value = kcat
            )
          ),
          column(
            style = "padding-left: 0px;",
            width = 3,
            textInput(
              inputId = "TI_degradation_enzyme_kcat_value_edit",
              label = "Value",
              value = rv.PARAMETERS$parameters[[kcat.id]]$Value
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.CB_degradation_enzyme_useVmax_edit",
        fluidRow(
          column(
            style = "padding-right: 0px;",
            width = 3,
            textInput(
              inputId = "TI_degradation_enzyme_Vmax_edit",
              label = "Vmax",
              value = Vmax
            )
          ),
          column(
            style = "padding-left: 0px;",
            width = 3,
            textInput(
              inputId = "TI_degradation_enzyme_Vmax_value_edit",
              label = "Value",
              value = rv.PARAMETERS$parameters[[Vmax.id]]$Value
            )
          )
        )
      ),
      fluidRow(
        column(
          style = "padding-right: 0px;",
          width = 3,
          textInput(
            inputId = "TI_degradation_enzyme_Km_edit",
            label = "Km",
            value = Km
          )
        ),
        column(
          style = "padding-left: 0px;",
          width = 3,
          textInput(
            inputId = "TI_degradation_enzyme_Km_value_edit",
            label = "Value",
            value = rv.PARAMETERS$parameters[[Km.id]]$Value
          )
        )
      )
    )
  }
  else if (eqn.reaction.law == "exponential_growth") {
    growthInfo <- rv.REACTIONS$exponentialGrowth[[eqn.ID]]
    
    species    <- growthInfo$Species
    mu         <- growthInfo$Mu
    mu.id      <- growthInfo$Mu.id
    mu.value   <- rv.PARAMETERS$parameters[[mu.id]]$Value
    
    div(
      fluidRow(
        column(
          width = 4,
          pickerInput(
            inputId = "PI_exp_growth_species_edit",
            label   = "Growing Species",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = species,
            options = pickerOptions(liveSearch = TRUE,
                                    liveSearchStyle = "startsWith")
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          textInput(
            inputId = "TI_exp_growth_mu_edit",
            label = "Growth Rate Parameter (mu)",
            value = mu
          )
        ),
        column(
          width = 3,
          numericInput(
            inputId = "NI_exp_growth_mu_value_edit",
            label = "Value",
            value = mu.value,
            min = 0,
            step = 0.01
          )
        )
      )
    )
  }
  else if (eqn.reaction.law == "michaelis_menten") {
    
    Info   <- rv.REACTIONS$michaelisMenten[[eqn.ID]]
    
    ID            <- Info$ID
    law           <- Info$Reaction.Law
    Substrate     <- Info$Substrate
    Substrate.id  <- Info$Substrate.id
    Product       <- Info$Product
    Product.id    <- Info$Product.id
    UseVmax       <- Info$UseVmax
    Km            <- Info$Km
    Km.id         <- Info$Km.id
    Vmax          <- Info$Vmax
    Vmax.id       <- Info$Vmax.id
    Enzyme        <- Info$Enzyme
    Enzyme.id     <- Info$Enzyme.id
    kcat          <- Info$kcat
    kcat.id       <- Info$kcat.id
    
    div(
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "PI_michaelis_menten_substrate_edit",
            label = "Substrate",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = Substrate,
            options = pickerOptions(
              liveSearch = TRUE,
              liveSearchStyle = "startsWith",
              dropupAuto = FALSE
            )
          )
        ),
        column(
          width = 3,
          offset = 1,
          pickerInput(
            inputId = "PI_michaelis_menten_product_edit",
            label = "Product",
            choices = sort(rv.SPECIES$df.by.compartment$Name),
            selected = Product,
            options = pickerOptions(
              liveSearch = TRUE,
              liveSearchStyle = "startsWith",
              dropupAuto = FALSE
            )
          )
        ),
        column(
          width = 3, 
          offset = 1,
          conditionalPanel(
            condition = "!input.CB_michaelis_menten_useVmax_edit",
            pickerInput(
              inputId = "PI_michaelis_menten_enzyme_edit",
              label = "Enzyme",
              choices = sort(rv.SPECIES$df.by.compartment$Name),
              selected = Enzyme,
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith")
            )
          )
        )
      ),
      hr(),
      fluidRow(
        column(
          style = "padding-right: 0px",
          width = 3,
          textInput(
            inputId = "TI_michaelis_menten_Km_edit",
            label = "Km",
            value = Km
          )
        ),
        column(
          style = "padding-left: 0px",
          width = 3,
          textInput(
            inputId = "TI_michaelis_menten_Km_value_edit",
            label = "Value",
            value = rv.PARAMETERS$parameters[[Km.id]]$Value
          )
        )
      ),
      fluidRow(
        column(
          width = 3,
          style = "padding-right: 0px",
          conditionalPanel(
            condition = "input.CB_michaelis_menten_useVmax_edit",
            textInput(
              inputId = "TI_michaelis_menten_vmax_edit",
              label = "Vmax",
              value = Vmax
            )
          ),
          conditionalPanel(
            condition = "!input.CB_michaelis_menten_useVmax_edit",
            textInput(
              inputId = "TI_michaelis_menten_kcat_edit",
              label = "kcat",
              value = kcat
            )
          )
        ),
        column(
          width = 3,
          style = "padding-left: 0px",
          conditionalPanel(
            condition = "input.CB_michaelis_menten_useVmax_edit",
            textInput(
              inputId = "TI_michaelis_menten_vmax_value_edit",
              label = "Value",
              value = rv.PARAMETERS$parameters[[Vmax.id]]$Value
            )
          ),
          conditionalPanel(
            condition = "!input.CB_michaelis_menten_useVmax_edit",
            textInput(
              inputId = "TI_michaelis_menten_kcat_value_edit",
              label = "Value",
              value = rv.PARAMETERS$parameters[[kcat.id]]$Value
            )
          )
        )
      )
    )
  }
  else if (eqn.reaction.law == "predator_prey") {
    # Use the UI from equationBuilder_predator_prey_edit
    uiOutput("equationBuilder_predator_prey_edit")
  }
})



# Equation Text UI Show --------------------------------------------------------
output$build_equation_edit <- renderUI({
  tryCatch({
    withMathJax(equationBuilder_edit_mathJax())
  }, warning = function(w) {
    # showNotification('there was a warning','',type = "error")
    # return()
  }, error = function(e) {
    # showNotification('there was an error','',type = "error")
    # return()
  }, silent=TRUE)
  
})


# Edit: Store New Equation -----------------------------------------------------

observeEvent(input$modal_editEqn_edit_button, {
  
  # JS Visual Runs
  w.test$show()
  shinyjs::disable("createEqn_store_edit_button")
  Sys.sleep(0.5)
  
  comp.id <- NA
  # Find equation in data structure
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- rv.REACTIONS$reactions[[eqn.num]]
  
  # Unpack Equation Information
  eqn.ID               <- eqn.row$ID            
  eqn.display.type     <- eqn.row$Eqn.Display.Type 
  eqn.reaction.law     <- eqn.row$Reaction.Law    
  eqn.species          <- eqn.row$Species          
  eqn.reactants        <- eqn.row$Reactants        
  eqn.products         <- eqn.row$Products         
  eqn.Modifiers        <- eqn.row$Modifiers  
  eqn.parameters       <- eqn.row$Parameters       
  eqn.compartment      <- eqn.row$Compartment      
  eqn.description      <- eqn.row$Description      
  eqn.species.id       <- eqn.row$Species.id      
  eqn.reactants.id     <- eqn.row$Reactants.id     
  eqn.products.id      <- eqn.row$Products.id      
  eqn.modifiers.id     <- eqn.row$Modifiers.id     
  eqn.parameters.id    <- eqn.row$Parameters.id   
  eqn.compartment.id   <- eqn.row$Compartment.id   
  eqn.equation.text    <- eqn.row$Equation.Text    
  eqn.equation.latex   <- eqn.row$Equation.Latex   
  eqn.equation.mathjax <- eqn.row$Equation.MathJax 
  eqn.string.rate.law  <- eqn.row$String.Rate.Law  
  eqn.pretty.rate.law  <- eqn.row$Pretty.Rate.Law  
  eqn.latex.rate.law   <- eqn.row$Latex.Rate.Law   
  eqn.mathjax.rate.law <- eqn.row$MathJax.Rate.Law 
  eqn.mathml.rate.law  <- eqn.row$MathMl.Rate.Law 
  eqn.reversible       <- eqn.row$Reversible
  
  # Unpack Old Parameters in Equation
  old.params    <- str_split(eqn.parameters, ", ")[[1]]
  old.species   <- str_split(eqn.species, ", ")[[1]]
  old.reactants <- str_split(eqn.reactants, ", ")[[1]]
  old.products  <- str_split(eqn.products, ", ")[[1]]
  
  old.params.id    <- str_split(eqn.parameters.id, ", ")[[1]]
  old.species.id   <- str_split(eqn.species.id, ", ")[[1]]
  old.reactants.id <- str_split(eqn.reactants.id, ", ")[[1]]
  old.products.id  <- str_split(eqn.products.id, ", ")[[1]]
  
  comp.id <- eqn.compartment.id

  # Initialize new variables
  parameters          <- c() # Parameter Variable Vector
  param.vals          <- c() # Parameter Values
  param.units         <- c() # parameter Unit Vector
  unit.descriptions   <- c() # Parameter Unit Breakdown Vector
  param.descriptions  <- c() # Parameter Description Vector
  base.units          <- c() # Base Unit for calculations
  base.values         <- c() # Base Unit Values
  species             <- c() # Variables in model to add
  parameters.id       <- c() # Parameter Ids
  species.id          <- c() # Variable Ids
  passed.error.check  <- TRUE
  
  # Initalize reactants/products
  reactants    <- NA
  reactants.id <- NA
  products     <- NA
  products.id  <- NA
  isReversible <- FALSE
  
  # Get Compartment information
  compartment    <- input$eqnCreate_active_compartment
  compartment.id <- FindId(compartment)
  
  # Get Compartment volume
  volume.var <- rv.COMPARTMENTS$compartments[[compartment.id]]$Volume
  
  # Mass Action
  if (eqn.reaction.law == "mass_action") {
    reaction.id <- NA
    eqn.display <- "Mass Action"
    backend.call <- "mass_action"
    
    modifiers    <- NA
    modifiers.id <- NA
    
    number.reactants <- as.numeric(input$NI_mass_action_num_reactants_edit)
    number.products  <- as.numeric(input$NI_mass_action_num_products_edit)
    
    # Build left hand side of equation
    left     <- BuildEquationSide("input$NI_MA_r_stoichiometry_edit_", 
                                  "input$PI_MA_reactant_edit_", 
                                  number.reactants)
    r.stoich      <- left[["coefs"]]
    reactants     <- left[["vars"]]
    reactants.id  <- left[["ids"]]
    
    # Build right hand side equation
    right    <- BuildEquationSide("input$NI_MA_p_stoichiometry_edit_",
                                  "input$PI_MA_product_edit_", 
                                  number.products)
    p.stoich    <- right[["coefs"]]
    products    <- right[["vars"]]
    products.id <- right[["ids"]]
    
    eqn.description <- ""
    eqn.d <- "Mass Action Reaction"
    species    <- c(strsplit(reactants, ", ")[[1]], 
                    strsplit(products, ", ")[[1]])
    species.id <- c(strsplit(reactants.id, ", ")[[1]],
                    strsplit(products.id, ", ")[[1]])
    
    # Find Kf information
    kf    <- input$TI_mass_action_forward_k_edit
    
    # Rate Constant Values
    kf.val <- input$TI_mass_action_forward_k_value_edit
    
    # Build Rate Constant Units
    kf.unit <- DetermineRateConstantUnits(
      r.stoich,
      rv.UNITS$units.base$For.Var,
      rv.UNITS$units.base$Volume,
      rv.UNITS$units.base$Duration,
      rv.UNITS$units.selected$For.Var,
      rv.UNITS$units.selected$Volume,
      rv.UNITS$units.selected$Duration
    )
    
    # Convert rate constant units if necessary
    if (kf.unit$unit != kf.unit$unit.base) {
      kf.base.val <- UnitConversion(kf.unit$unit.description,
                                    kf.unit$unit,
                                    kf.unit$base.unit,
                                    as.numeric(kf.val))
    } else {
      kf.base.val <- kf.val
    }
    
    # Write Unit Descriptions
    kf.d <- paste0("Forward rate constant for the reaction of ",
                   reactants,
                   " to ",
                   products)
    
    parameters         <- c(parameters, kf)
    param.vals         <- c(param.vals, kf.val)
    param.units        <- c(param.units, kf.unit$unit)
    unit.descriptions  <- c(unit.descriptions, kf.unit$unit.description)
    param.descriptions <- c(param.descriptions, kf.d)
    base.units         <- c(base.units, kf.unit$unit.base)
    base.values        <- c(base.values, kf.base.val)
    
    reversible <- input$PI_mass_action_reverisble_option_edit
    if (reversible == "both_directions") {
      isReversible <- TRUE
      # If the reaction is reversible then we need to build the reverse
      # rate constant for the reaction
      kr     <- input$TI_mass_action_reverse_k_edit
      kr.val <- input$TI_mass_action_reverse_k_value_edit
      
      # Build Rate Constant Units
      kr.unit <- DetermineRateConstantUnits(
        p.stoich,
        rv.UNITS$units.base$For.Var,
        rv.UNITS$units.base$Volume,
        rv.UNITS$units.base$Duration,
        rv.UNITS$units.selected$For.Var,
        rv.UNITS$units.selected$Volume,
        rv.UNITS$units.selected$Duration
      )
      
      # Convert rate constant units if necessary
      if (kr.unit$unit != kr.unit$unit.base) {
        kr.base.val <- UnitConversion(kr.unit$unit.description,
                                      kr.unit$unit,
                                      kr.unit$base.unit,
                                      as.numeric(kr.val))
      } else {
        kr.base.val <- kr.val
      }
      
      # Write Unit Descriptions
      kr.d <- paste0("Reverse rate constant for the reaction of ",
                     reactants,
                     " to ",
                     products
      )
      
      parameters         <- c(parameters, kr)
      param.vals         <- c(param.vals, kr.val)
      param.units        <- c(param.units,kr.unit$unit)
      unit.descriptions  <- c(unit.descriptions, kr.unit$unit.description)
      param.descriptions <- c(param.descriptions, kr.d)
      base.units         <- c(base.units, kr.unit$unit.base)
      base.values        <- c(base.values, kr.base.val)
      
    } 
    else if (reversible == "forward_only") {
      kr     <- NA
      kr.val <- NA
    }
    # browser()

    # Ensure par.ids reflects the current `parameters` order (safety rebuild)
    if (exists("par.ids")) {
      par.ids <- c()
      if (length(parameters) > 0) {
        for (i in seq_along(parameters)) {
          par.ids <- c(par.ids, FindId(parameters[i]))
        }
      }
    }
    # Build Rate Law
    laws <- Law_Of_Mass_Action(r.stoich,
                               reactants,
                               p.stoich,
                               products,
                               reversible,
                               kf,
                               kr)
    
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    content.ml  <- laws$content.ml
    
  } 
  else if (eqn.reaction.law == "exponential_growth") {
    reaction.id  <- NA
    eqn.display  <- "Exponential Growth"
    backend.call <- "exponential_growth"
    modifiers    <- NA
    modifiers.id <- NA
    reactants    <- NA
    reactants.id <- NA
    products     <- NA
    products.id  <- NA
    isReversible <- FALSE
    
    growth.species    <- input$PI_exp_growth_species_edit
    growth.species.id <- FindId(growth.species)
    species           <- growth.species
    species.id        <- growth.species.id
    
    mu.name     <- input$TI_exp_growth_mu_edit
    mu.val      <- input$NI_exp_growth_mu_value_edit
    unit.description <- "num <div> time"
    base.unit   <- paste0("1/", rv.UNITS$units.base$Duration)
    param.unit  <- paste0("1/", rv.UNITS$units.selected$Duration)
    param.description <- paste0("Specific growth rate for ", growth.species)
    
    if (param.unit != base.unit) {
      base.val <- UnitConversion(unit.description,
                                 param.unit,
                                 base.unit,
                                 as.numeric(mu.val))
    } else {
      base.val <- mu.val
    }
    
    parameters         <- c(parameters, mu.name)
    param.vals         <- c(param.vals, mu.val)
    param.units        <- c(param.units, param.unit)
    unit.descriptions  <- c(unit.descriptions, unit.description)
    param.descriptions <- c(param.descriptions, param.description)
    base.units         <- c(base.units, base.unit)
    base.values        <- c(base.values, base.val)
    
    rate.law    <- paste0(mu.name, "*", growth.species)
    p.rate.law  <- rate.law
    latex.law   <- paste0(mu.name, "\\cdot ", growth.species)
    mathjax.law <- paste0(Var2MathJ(mu.name), "*", Var2MathJ(growth.species))
    mathml.law  <- NA
    content.ml  <- NA
    eqn.d       <- "Exponential growth dX/dt = mu*X"
  }
  else if (eqn.reaction.law == "monod_growth") {
    reaction.id  <- NA
    eqn.display  <- "Monod Growth"
    backend.call <- "monod_growth"
    modifiers    <- NA
    modifiers.id <- NA
    isReversible <- FALSE
    
    growth.species    <- input$PI_monod_species_edit
    growth.species.id <- FindId(growth.species)
    substrate         <- input$PI_monod_substrate_edit
    substrate.id     <- FindId(substrate)
    species           <- c(growth.species, substrate)
    species.id        <- c(growth.species.id, substrate.id)
    
    # Substrate is consumed (reactant), growing species is produced (product)
    reactants    <- substrate
    reactants.id <- substrate.id
    products     <- growth.species
    products.id  <- growth.species.id
    
    mu_max.name     <- input$TI_monod_mu_max_edit
    mu_max.val      <- input$NI_monod_mu_max_value_edit
    unit.description.mu <- "num <div> time"
    base.unit.mu    <- paste0("1/", rv.UNITS$units.base$Duration)
    param.unit.mu   <- paste0("1/", rv.UNITS$units.selected$Duration)
    
    if (param.unit.mu != base.unit.mu) {
      base.val.mu <- UnitConversion(unit.description.mu,
                                    param.unit.mu,
                                    base.unit.mu,
                                    as.numeric(mu_max.val))
    } else {
      base.val.mu <- mu_max.val
    }
    
    K_s.name     <- input$TI_monod_K_s_edit
    K_s.val      <- input$NI_monod_K_s_value_edit
    unit.K_s     <- rv.UNITS$units.selected$For.Var
    base.K_s     <- rv.UNITS$units.base$For.Var
    unit.description.K_s <- paste0("conc (", base.K_s, ")")
    
    if (unit.K_s != base.K_s) {
      base.val.K_s <- UnitConversion(unit.description.K_s,
                                     unit.K_s,
                                     base.K_s,
                                     as.numeric(K_s.val))
    } else {
      base.val.K_s <- K_s.val
    }
    
    parameters         <- c(parameters, mu_max.name, K_s.name)
    param.vals         <- c(param.vals, mu_max.val, K_s.val)
    param.units        <- c(param.units, param.unit.mu, unit.K_s)
    unit.descriptions  <- c(unit.descriptions, unit.description.mu, unit.description.K_s)
    param.descriptions <- c(param.descriptions, paste0("Specific growth rate for ", growth.species), paste0("Half-saturation constant for ", substrate))
    base.units         <- c(base.units, base.unit.mu, base.K_s)
    base.values        <- c(base.values, base.val.mu, base.val.K_s)
    
    rate.law    <- paste0(mu_max.name, "*", growth.species, "*", substrate, "/(", K_s.name, "+", substrate, ")")
    p.rate.law  <- rate.law
    latex.law   <- paste0(mu_max.name, "\\cdot ", growth.species, "\\cdot \\frac{", substrate, "}{", K_s.name, "+", substrate, "}")
    mathjax.law <- paste0(Var2MathJ(mu_max.name), "*", Var2MathJ(growth.species), "*\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.name), "+", Var2MathJ(substrate), "}")
    mathml.law  <- NA
    content.ml  <- NA
    eqn.d       <- paste0("Monod growth d", growth.species, "/dt = ", mu_max.name, "*", growth.species, "*", substrate, "/(", K_s.name, "+", substrate, ")")
  }
  else if (eqn.reaction.law == "competitive_monod") {
    # Check if single species mode (only X grows competitively)
    single.species.mode <- isTruthy(input$CB_comp_monod_single_species_edit)
    # Check if substrate consumption should exclude competitive restriction
    no.substrate.restriction <- isTruthy(input$CB_comp_monod_no_substrate_restriction_edit)
    
    reaction.id  <- NA
    eqn.display  <- if (single.species.mode) "Competitive Monod Growth (Single Species)" else "Competitive Monod Growth"
    backend.call <- "competitive_monod"
    modifiers    <- NA
    modifiers.id <- NA
    isReversible <- FALSE
    
    # Use different input IDs based on mode
    if (single.species.mode) {
      species.x    <- input$PI_comp_monod_species_x_edit_2
      species.y    <- input$PI_comp_monod_species_y_edit_2
      substrate    <- input$PI_comp_monod_substrate_edit_2
    } else {
      species.x    <- input$PI_comp_monod_species_x_edit
      species.y    <- input$PI_comp_monod_species_y_edit
      substrate    <- input$PI_comp_monod_substrate_edit
    }
    species.id.x <- FindId(species.x)
    species.id.y <- FindId(species.y)
    substrate.id <- FindId(substrate)
    
    if (single.species.mode) {
      # Only X grows competitively, Y is a modifier
      species      <- c(species.x, substrate)
      species.id   <- c(species.id.x, substrate.id)
      modifiers    <- species.y
      modifiers.id <- species.id.y
    } else {
      # Both species compete
      species      <- c(species.x, species.y, substrate)
      species.id   <- c(species.id.x, species.id.y, substrate.id)
    }
    
    # Parameters
    mu_max.x.name  <- input$TI_comp_monod_mu_max_x_edit
    mu_max.x.val   <- input$NI_comp_monod_mu_max_x_value_edit
    K_s.x.name     <- input$TI_comp_monod_K_s_x_edit
    K_s.x.val      <- input$NI_comp_monod_K_s_x_value_edit
    alpha.xy.name  <- input$TI_comp_monod_alpha_xy_edit
    alpha.xy.val   <- input$NI_comp_monod_alpha_xy_value_edit
    Kc.name        <- input$TI_comp_monod_Kc_edit
    Kc.val         <- input$NI_comp_monod_Kc_value_edit
    Y_x.name       <- input$TI_comp_monod_Y_x_edit
    Y_x.val        <- input$NI_comp_monod_Y_x_value_edit
    
    # Units
    unit.description.mu <- "num <div> time"
    base.unit.mu        <- paste0("1/", rv.UNITS$units.base$Duration)
    unit.mu             <- paste0("1/", rv.UNITS$units.selected$Duration)
    
    unit.K_s     <- rv.UNITS$units.selected$For.Var
    base.K_s     <- rv.UNITS$units.base$For.Var
    unit.description.K_s <- paste0("conc (", base.K_s, ")")
    
    unit.Kc      <- rv.UNITS$units.selected$For.Var
    base.Kc      <- rv.UNITS$units.base$For.Var
    
    addParam <- function(name, val, unit, base.unit, unit.desc, desc){
      if (unit != base.unit) {
        base.val <- UnitConversion(unit.desc, unit, base.unit, as.numeric(val))
      } else { base.val <- val }
      list(name=name,val=val,unit=unit,base.unit=base.unit,unit.desc=unit.desc,
           base.val=base.val, desc=desc)
    }
    
    p.mu_max.x <- addParam(mu_max.x.name, mu_max.x.val, unit.mu, base.unit.mu, unit.description.mu,
                          paste0("Maximum growth rate of ", species.x))
    p.K_s.x    <- addParam(K_s.x.name, K_s.x.val, unit.K_s, base.K_s, unit.description.K_s,
                          paste0("Half-saturation constant for ", species.x))
    p.alpha.xy <- addParam(alpha.xy.name, alpha.xy.val, "dimensionless", "dimensionless",
                          "dimensionless", paste0("Effect of ", species.y, " on ", species.x))
    p.Kc       <- addParam(Kc.name, Kc.val, unit.Kc, base.Kc,
                          paste0("conc (", base.Kc, ")"),
                          "Community carrying capacity")
    p.Y_x      <- addParam(Y_x.name, Y_x.val, "dimensionless", "dimensionless",
                          "dimensionless", paste0("Yield coefficient for ", species.x))
    
    if (single.species.mode) {
      # Single species mode: only X parameters
      pack <- list(p.mu_max.x, p.K_s.x, p.alpha.xy, p.Kc, p.Y_x)
    } else {
      # Both species mode: need all parameters
      mu_max.y.name  <- input$TI_comp_monod_mu_max_y_edit
      mu_max.y.val   <- input$NI_comp_monod_mu_max_y_value_edit
      K_s.y.name     <- input$TI_comp_monod_K_s_y_edit
      K_s.y.val      <- input$NI_comp_monod_K_s_y_value_edit
      alpha.yx.name  <- input$TI_comp_monod_alpha_yx_edit
      alpha.yx.val   <- input$NI_comp_monod_alpha_yx_value_edit
      Y_y.name       <- input$TI_comp_monod_Y_y_edit
      Y_y.val        <- input$NI_comp_monod_Y_y_value_edit
      
      p.mu_max.y <- addParam(mu_max.y.name, mu_max.y.val, unit.mu, base.unit.mu, unit.description.mu,
                            paste0("Maximum growth rate of ", species.y))
      p.K_s.y    <- addParam(K_s.y.name, K_s.y.val, unit.K_s, base.K_s, unit.description.K_s,
                            paste0("Half-saturation constant for ", species.y))
      p.alpha.yx <- addParam(alpha.yx.name, alpha.yx.val, "dimensionless", "dimensionless",
                            "dimensionless", paste0("Effect of ", species.x, " on ", species.y))
      p.Y_y      <- addParam(Y_y.name, Y_y.val, "dimensionless", "dimensionless",
                            "dimensionless", paste0("Yield coefficient for ", species.y))
      pack <- list(p.mu_max.x, p.mu_max.y, p.K_s.x, p.K_s.y, p.alpha.xy, p.alpha.yx, p.Kc, p.Y_x, p.Y_y)
    }
    
    for (p in pack){
      parameters         <- c(parameters, p$name)
      param.vals         <- c(param.vals, p$val)
      param.units        <- c(param.units, p$unit)
      unit.descriptions  <- c(unit.descriptions, p$unit.desc)
      param.descriptions <- c(param.descriptions, p$desc)
      base.units         <- c(base.units, p$base.unit)
      base.values        <- c(base.values, p$base.val)
    }
    
    # Rate laws:
    # For X: μ_max_x * X * S / (K_s_x + S) * (1 - (X + α_xy * Y) / K_c)
    rate.law.x <- paste0(mu_max.x.name, "*", species.x, "*", substrate, "/(", K_s.x.name, "+", substrate, ")*(1-(", species.x, "+", alpha.xy.name, "*", species.y, ")/", Kc.name, ")")
    # For S consumption from X: Y_x * (growth rate of X) - ODE derivation will add negative sign for reactant
    # If no.substrate.restriction is TRUE, remove competitive term from substrate consumption
    if (no.substrate.restriction) {
      rate.law.s.x <- paste0(Y_x.name, "*", mu_max.x.name, "*", species.x, "*", substrate, "/(", K_s.x.name, "+", substrate, ")")
    } else {
      rate.law.s.x <- paste0(Y_x.name, "*", mu_max.x.name, "*", species.x, "*", substrate, "/(", K_s.x.name, "+", substrate, ")*(1-(", species.x, "+", alpha.xy.name, "*", species.y, ")/", Kc.name, ")")
    }
    
    if (single.species.mode) {
      # Only X equation and S consumption from X
      rate.law.y <- NA
      rate.law.s.y <- NA
      # Build MathJax - substrate consumption may or may not have competitive term
      if (no.substrate.restriction) {
        mathjax.law <- paste0("\\begin{aligned}",
                              "\\frac{d", Var2MathJ(species.x), "}{dt} &= ", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(alpha.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                              "\\frac{d", Var2MathJ(substrate), "}{dt} &= -", Var2MathJ(Y_x.name), "*", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}",
                              "\\end{aligned}")
      } else {
        mathjax.law <- paste0("\\begin{aligned}",
                              "\\frac{d", Var2MathJ(species.x), "}{dt} &= ", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(alpha.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                              "\\frac{d", Var2MathJ(substrate), "}{dt} &= -", Var2MathJ(Y_x.name), "*", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(alpha.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right)",
                              "\\end{aligned}")
      }
      eqn.d       <- paste0("Competitive Monod growth: ", species.x, " grows competitively with ", species.y, " as competitor")
    } else {
      # Both species equations
      mu_max.y.name  <- input$TI_comp_monod_mu_max_y_edit
      K_s.y.name     <- input$TI_comp_monod_K_s_y_edit
      alpha.yx.name  <- input$TI_comp_monod_alpha_yx_edit
      Y_y.name       <- input$TI_comp_monod_Y_y_edit
      
      rate.law.y <- paste0(mu_max.y.name, "*", species.y, "*", substrate, "/(", K_s.y.name, "+", substrate, ")*(1-(", species.y, "+", alpha.yx.name, "*", species.x, ")/", Kc.name, ")")
      # If no.substrate.restriction is TRUE, remove competitive term from substrate consumption
      if (no.substrate.restriction) {
        rate.law.s.y <- paste0(Y_y.name, "*", mu_max.y.name, "*", species.y, "*", substrate, "/(", K_s.y.name, "+", substrate, ")")
      } else {
        rate.law.s.y <- paste0(Y_y.name, "*", mu_max.y.name, "*", species.y, "*", substrate, "/(", K_s.y.name, "+", substrate, ")*(1-(", species.y, "+", alpha.yx.name, "*", species.x, ")/", Kc.name, ")")
      }
      # Build MathJax - substrate consumption may or may not have competitive term
      if (no.substrate.restriction) {
        mathjax.law <- paste0("\\begin{aligned}",
                              "\\frac{d", Var2MathJ(species.x), "}{dt} &= ", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(alpha.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                              "\\frac{d", Var2MathJ(species.y), "}{dt} &= ", Var2MathJ(mu_max.y.name), Var2MathJ(species.y), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.y.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.y), "+", Var2MathJ(alpha.yx.name), Var2MathJ(species.x), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                              "\\frac{d", Var2MathJ(substrate), "}{dt} &= -", Var2MathJ(Y_x.name), "*", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}-", Var2MathJ(Y_y.name), "*", Var2MathJ(mu_max.y.name), Var2MathJ(species.y), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.y.name), "+", Var2MathJ(substrate), "}",
                              "\\end{aligned}")
      } else {
        mathjax.law <- paste0("\\begin{aligned}",
                              "\\frac{d", Var2MathJ(species.x), "}{dt} &= ", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(alpha.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                              "\\frac{d", Var2MathJ(species.y), "}{dt} &= ", Var2MathJ(mu_max.y.name), Var2MathJ(species.y), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.y.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.y), "+", Var2MathJ(alpha.yx.name), Var2MathJ(species.x), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                              "\\frac{d", Var2MathJ(substrate), "}{dt} &= -", Var2MathJ(Y_x.name), "*", Var2MathJ(mu_max.x.name), Var2MathJ(species.x), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.x.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(alpha.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right)-", Var2MathJ(Y_y.name), "*", Var2MathJ(mu_max.y.name), Var2MathJ(species.y), "\\frac{", Var2MathJ(substrate), "}{", Var2MathJ(K_s.y.name), "+", Var2MathJ(substrate), "}\\left(1-\\frac{", Var2MathJ(species.y), "+", Var2MathJ(alpha.yx.name), Var2MathJ(species.x), "}{", Var2MathJ(Kc.name), "}\\right)",
                              "\\end{aligned}")
      }
      eqn.d       <- "Competitive Monod growth between two species on shared substrate"
    }
    
    rate.law    <- rate.law.x
    p.rate.law  <- rate.law.x
    latex.law   <- rate.law.x
    mathml.law  <- NA
    content.ml  <- NA
  }
  else if (eqn.reaction.law == "predator_prey") {
    reaction.id  <- NA
    eqn.display  <- "Predator–Prey"
    backend.call <- "predator_prey"
    modifiers    <- NA
    modifiers.id <- NA
    reactants    <- NA
    reactants.id <- NA
    products     <- NA
    products.id  <- NA
    isReversible <- FALSE
    
    # Species
    species.x    <- input$PI_pred_prey_prey_edit
    species.y    <- input$PI_pred_prey_predator_edit
    if (is.null(species.x) || species.x == "" || is.null(species.y) || species.y == "") {
      return()
    }
    species.id.x <- FindId(species.x)
    species.id.y <- FindId(species.y)
    species      <- c(species.x, species.y)
    species.id   <- c(species.id.x, species.id.y)
    
    # Parameters
    r.name <- input$TI_pred_prey_r_edit
    r.val  <- input$NI_pred_prey_r_value_edit
    a.name <- input$TI_pred_prey_a_edit
    a.val  <- input$NI_pred_prey_a_value_edit
    b.name <- input$TI_pred_prey_b_edit
    b.val  <- input$NI_pred_prey_b_value_edit
    d.name <- input$TI_pred_prey_d_edit
    d.val  <- input$NI_pred_prey_d_value_edit
    
    unit.description.r <- "num <div> time"
    base.unit.r        <- paste0("1/", rv.UNITS$units.base$Duration)
    unit.r             <- paste0("1/", rv.UNITS$units.selected$Duration)
    
    addParam <- function(name, val, unit, base.unit, unit.desc, desc){
      if (unit != base.unit) {
        base.val <- UnitConversion(unit.desc, unit, base.unit, as.numeric(val))
      } else { base.val <- val }
      list(name=name,val=val,unit=unit,base.unit=base.unit,unit.desc=unit.desc,
           base.val=base.val, desc=desc)
    }
    
    p.r <- addParam(r.name, r.val, unit.r, base.unit.r, unit.description.r,
                    paste0("Prey growth rate for ", species.x))
    p.a <- addParam(a.name, a.val, unit.r, base.unit.r, unit.description.r,
                    paste0("Attack rate (loss of ", species.x, " due to ", species.y, ")"))
    p.b <- addParam(b.name, b.val, unit.r, base.unit.r, unit.description.r,
                    paste0("Conversion rate (gain of ", species.y, " from consuming ", species.x, ")"))
    p.d <- addParam(d.name, d.val, unit.r, base.unit.r, unit.description.r,
                    paste0("Predator death rate for ", species.y))
    
    pack <- list(p.r, p.a, p.b, p.d)
    for (p in pack){
      parameters         <- c(parameters, p$name)
      param.vals         <- c(param.vals, p$val)
      param.units        <- c(param.units, p$unit)
      unit.descriptions  <- c(unit.descriptions, p$unit.desc)
      param.descriptions <- c(param.descriptions, p$desc)
      base.units         <- c(base.units, p$base.unit)
      base.values        <- c(base.values, p$base.val)
    }
    
    # Rate laws (net right-hand sides)
    rate.law.x <- paste0(r.name, "*", species.x, "-", a.name, "*", species.x, "*", species.y)
    rate.law.y <- paste0(b.name, "*", species.x, "*", species.y, "-", d.name, "*", species.y)
    
    mathjax.law <- paste0("\\begin{aligned}",
                          "\\frac{d", Var2MathJ(species.x), "}{dt} &= ", Var2MathJ(r.name), Var2MathJ(species.x),
                          "-", Var2MathJ(a.name), Var2MathJ(species.x), Var2MathJ(species.y), " \\\\",
                          "\\frac{d", Var2MathJ(species.y), "}{dt} &= ", Var2MathJ(b.name), Var2MathJ(species.x), Var2MathJ(species.y),
                          "-", Var2MathJ(d.name), Var2MathJ(species.y),
                          "\\end{aligned}")
    rate.law    <- rate.law.x
    p.rate.law  <- rate.law.x
    latex.law   <- rate.law.x
    mathml.law  <- NA
    content.ml  <- NA
    eqn.d       <- paste0("Predator–prey interaction between ", species.x, " (prey) and ", species.y, " (predator)")
    eqn.text    <- paste0(species.x, " <-->(predator-prey) ", species.y)
  }
  else if (eqn.reaction.law == "logistic_competition") {
    # Check if single species mode (only X grows competitively)
    single.species.mode <- isTruthy(input$CB_log_comp_single_species_edit)
    
    reaction.id  <- NA
    eqn.display  <- if (single.species.mode) "Logistic Competition (Single Species)" else "Logistic Competition"
    backend.call <- "logistic_competition"
    modifiers    <- NA
    modifiers.id <- NA
    reactants    <- NA
    reactants.id <- NA
    products     <- NA
    products.id  <- NA
    isReversible <- FALSE
    
    # Use different input IDs based on mode
    if (single.species.mode) {
      species.x    <- input$PI_log_comp_species_x_edit_2
      species.y    <- input$PI_log_comp_species_y_edit_2
    } else {
      species.x    <- input$PI_log_comp_species_x_edit
      species.y    <- input$PI_log_comp_species_y_edit
    }
    species.id.x <- FindId(species.x)
    species.id.y <- FindId(species.y)
    
    if (single.species.mode) {
      # Only X grows competitively, Y is a modifier
      species      <- species.x
      species.id   <- species.id.x
      modifiers    <- species.y
      modifiers.id <- species.id.y
    } else {
      # Both species compete
      species      <- c(species.x, species.y)
      species.id   <- c(species.id.x, species.id.y)
    }
    
    # parameters
    r.x.name  <- input$TI_log_comp_r_x_edit
    r.x.val   <- input$NI_log_comp_r_x_value_edit
    a.xy.name <- input$TI_log_comp_alpha_xy_edit
    a.xy.val  <- input$NI_log_comp_alpha_xy_value_edit
    Kc.name   <- input$TI_log_comp_Kc_edit
    Kc.val    <- input$NI_log_comp_Kc_value_edit
    
    unit.description.r  <- "num <div> time"
    base.unit.r         <- paste0("1/", rv.UNITS$units.base$Duration)
    unit.r              <- paste0("1/", rv.UNITS$units.selected$Duration)
    
    addParam <- function(name, val, unit, base.unit, unit.desc, desc){
      if (unit != base.unit) {
        base.val <- UnitConversion(unit.desc, unit, base.unit, as.numeric(val))
      } else { base.val <- val }
      list(name=name,val=val,unit=unit,base.unit=base.unit,unit.desc=unit.desc,
           base.val=base.val, desc=desc)
    }
    
    p.r.x <- addParam(r.x.name, r.x.val, unit.r, base.unit.r, unit.description.r,
                      paste0("Growth rate of ", species.x))
    p.a.xy<- addParam(a.xy.name, a.xy.val, "dimensionless", "dimensionless",
                      "dimensionless", paste0("Effect of ", species.y, " on ", species.x))
    unit.Kc <- rv.UNITS$units.selected$For.Var
    base.Kc <- rv.UNITS$units.base$For.Var
    p.Kc <- addParam(Kc.name, Kc.val, unit.Kc, base.Kc,
                     paste0("conc (", base.Kc, ")"),
                     "Community carrying capacity")
    
    if (single.species.mode) {
      # Single species mode: only X parameters
      pack <- list(p.r.x, p.a.xy, p.Kc)
    } else {
      # Both species mode: need r.y and alpha.yx
      r.y.name  <- input$TI_log_comp_r_y_edit
      r.y.val   <- input$NI_log_comp_r_y_value_edit
      a.yx.name <- input$TI_log_comp_alpha_yx_edit
      a.yx.val  <- input$NI_log_comp_alpha_yx_value_edit
      
      p.r.y <- addParam(r.y.name, r.y.val, unit.r, base.unit.r, unit.description.r,
                        paste0("Growth rate of ", species.y))
      p.a.yx<- addParam(a.yx.name, a.yx.val, "dimensionless", "dimensionless",
                        "dimensionless", paste0("Effect of ", species.x, " on ", species.y))
      pack <- list(p.r.x, p.r.y, p.a.xy, p.a.yx, p.Kc)
    }
    
    for (p in pack){
      parameters         <- c(parameters, p$name)
      param.vals         <- c(param.vals, p$val)
      param.units        <- c(param.units, p$unit)
      unit.descriptions  <- c(unit.descriptions, p$unit.desc)
      param.descriptions <- c(param.descriptions, p$desc)
      base.units         <- c(base.units, p$base.unit)
      base.values        <- c(base.values, p$base.val)
    }
    
    rate.law.x <- paste0(r.x.name,"*",species.x,"*(1-(",species.x,"+",a.xy.name,"*",species.y,")/",Kc.name,")")
    
    if (single.species.mode) {
      # Only X equation
      rate.law.y <- NA
      rate.law   <- rate.law.x
      p.rate.law <- rate.law.x
      latex.law  <- rate.law.x
      mathjax.law<- paste0("\\frac{d", Var2MathJ(species.x), "}{dt} = ", Var2MathJ(r.x.name), Var2MathJ(species.x), "\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(a.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right)")
      eqn.d      <- paste0("Logistic competition: ", species.x, " grows competitively with ", species.y, " as competitor")
    } else {
      # Both species equations
      r.y.name  <- input$TI_log_comp_r_y_edit
      a.yx.name <- input$TI_log_comp_alpha_yx_edit
      rate.law.y <- paste0(r.y.name,"*",species.y,"*(1-(",species.y,"+",a.yx.name,"*",species.x,")/",Kc.name,")")
      rate.law   <- paste(rate.law.x, rate.law.y, sep=" ; ")
      p.rate.law <- rate.law
      latex.law  <- rate.law
      mathjax.law<- paste0("\\begin{aligned}",
                            "\\frac{d", Var2MathJ(species.x), "}{dt} &= ", Var2MathJ(r.x.name), Var2MathJ(species.x), "\\left(1-\\frac{", Var2MathJ(species.x), "+", Var2MathJ(a.xy.name), Var2MathJ(species.y), "}{", Var2MathJ(Kc.name), "}\\right) \\\\",
                            "\\frac{d", Var2MathJ(species.y), "}{dt} &= ", Var2MathJ(r.y.name), Var2MathJ(species.y), "\\left(1-\\frac{", Var2MathJ(species.y), "+", Var2MathJ(a.yx.name), Var2MathJ(species.x), "}{", Var2MathJ(Kc.name), "}\\right)",
                            "\\end{aligned}")
      eqn.d      <- "Logistic competition between two species"
    }
    
    mathml.law <- NA
    content.ml <- NA
  }
  else if (eqn.reaction.law == "mass_action_w_reg") {
    reaction.id <- NA
    eqn.display <- "Regulated Mass Action"
    backend.call <- "mass_action_w_reg"
    
    # browser()
    
    modifiers    <- NA
    modifiers.id <- NA
    
    # Base rate constants that can vary based on options
    kf     <- NA
    kf.id  <- NA
    kf.val <- NA
    kr     <- NA
    kr.id  <- NA
    kr.val <- NA
    
    # Modifier rate constants/variables that can vary based on options
    Forward.Mods    <- NA
    Forward.Mods.id <- NA
    Forward.Pars    <- NA
    Forward.Pars.id <- NA
    Reverse.Mods    <- NA
    Reverse.Mods.id <- NA
    Reverse.Pars    <- NA
    Reverse.Pars.id <- NA
    # browser()
    number.reactants <- as.numeric(input$NI_mass_action_wReg_num_reactants_edit)
    number.products  <- as.numeric(input$NI_mass_action_wReg_num_products_edit)
    
    has.f.reg <- input$CB_MAwR_chem_modifier_forward_edit
    has.r.reg <- input$CB_MAwR_chem_modifier_reverse_edit
    n.f.reg   <- as.numeric(input$NI_MAwR_n_forward_regulators_edit) 
    n.r.reg   <- as.numeric(input$NI_MAwR_n_reverse_regulators_edit) 
    
    # Build left hand side of equation
    left     <- BuildEquationSide("input$NI_MAwR_r_stoichiometry_edit_", 
                                  "input$PI_MAwR_reactant_edit_", 
                                  number.reactants)
    r.stoich      <- left[["coefs"]]
    reactants     <- left[["vars"]]
    reactants.id  <- left[["ids"]]
    
    # Build right hand side equation
    right    <- BuildEquationSide("input$NI_MAwR_p_stoichiometry_edit_",
                                  "input$PI_MAwR_product_edit_", 
                                  number.products)
    p.stoich    <- right[["coefs"]]
    products    <- right[["vars"]]
    products.id <- right[["ids"]]
    
    eqn.description <- ""
    species    <- c(strsplit(reactants, ", ")[[1]], 
                    strsplit(products, ", ")[[1]])
    species.id <- c(strsplit(reactants.id, ", ")[[1]],
                    strsplit(products.id, ", ")[[1]])
    
    # Check for forwared regulators
    if (has.f.reg) {
      # Parse forward modifiers information
      f.regs <- BuildRegulatorSide(
        "input$PI_MAwR_forward_regulator_edit_", 
        "input$TI_MAwR_forward_regulator_RC_edit_",
        "input$TI_MAwR_forward_regulator_RC_value_edit_",
        n.f.reg,
        reactants,
        products,
        TRUE)
      FMs     <- f.regs[["regulators"]]
      FM.RC   <- f.regs[["rateConstants"]]
      FM.ids  <- f.regs[["reg.ids"]]
      FM.vals <- f.regs[["regulator.val"]]
      
      FM.rc.descript <- f.regs[["rc.descript"]]
      
      Forward.Mods    <- paste0(FMs, collapse = ", ")
      Forward.Mods.id <- paste0(FM.ids, collapse = ", ")
      Forward.Pars    <- paste0(FM.RC, collapse = ", ")
      
      for (i in seq_along(FM.RC)) {
        u <- DetermineRateConstantUnits("1",
                                        rv.UNITS$units.base$For.Var,
                                        rv.UNITS$units.base$Volume,
                                        rv.UNITS$units.base$Duration,
                                        rv.UNITS$units.selected$For.Var,
                                        rv.UNITS$units.selected$Volume,
                                        rv.UNITS$units.selected$Duration)
        # Perform conversion to base units if needed
        if (u$unit != u$unit.base) {
          base.val <- UnitConversion(u$unit.d,
                                     u$unit,
                                     u$base.unit,
                                     as.numeric(FM.vals[i]))
        } else {
          base.val <- FM.vals[i]
        }
        
        
        parameters         <- c(parameters, FM.RC[i])
        param.vals         <- c(param.vals, FM.vals[i])
        param.units        <- c(param.units, u$unit)
        unit.descriptions  <- c(unit.descriptions, u$unit.d)
        param.descriptions <- c(param.descriptions, FM.rc.descript[i])
        base.units         <- c(base.units, u$unit.base)
        base.values        <- c(base.values, base.val)
      }
      
    } 
    else {
      # Find kf if there are no modifiers for it
      
      kf    <- input$TI_MAwR_forward_k_edit
      kf.id <- FindId(kf)
      # Rate Constant Values
      kf.val <- input$TI_MAwR_forward_k_value_edit
      
      # Build Rate Constant Units
      kf.unit <- DetermineRateConstantUnits(
        p.stoich,
        rv.UNITS$units.base$For.Var,
        rv.UNITS$units.base$Volume,
        rv.UNITS$units.base$Duration,
        rv.UNITS$units.selected$For.Var,
        rv.UNITS$units.selected$Volume,
        rv.UNITS$units.selected$Duration
      )
      
      # Convert rate constant units if necessary
      if (kf.unit$unit != kf.unit$unit.base) {
        kf.base.val <- UnitConversion(kf.unit$unit.description,
                                      kf.unit$unit,
                                      kf.unit$base.unit,
                                      as.numeric(kf.val))
      } else {
        kf.base.val <- kf.val
      }
      
      # Write Unit Descriptions
      kf.d <- paste0("Forward rate constant for the reaction of ",
                     reactants,
                     " to ",
                     products)
      
      parameters         <- c(parameters, kf)
      param.vals         <- c(param.vals, kf.val)
      param.units        <- c(param.units, kf.unit$unit)
      unit.descriptions  <- c(unit.descriptions, kf.unit$unit.description)
      param.descriptions <- c(param.descriptions, kf.d)
      base.units         <- c(base.units, kf.unit$unit.base)
      base.values        <- c(base.values, kf.base.val)
      
    }
    
    reversible <- input$reaction_mass_action_wReg_reverisble_edit
    if (reversible == "both_directions") {
      # If the reaction is reversible then we need to build the reverse
      # rate constant for the reaction
      isReversible <- TRUE
      if (has.r.reg) {
        r.regs <- BuildRegulatorSide(
          "input$PI_MAwR_reverse_regulator_edit_", 
          "input$TI_MAwR_reverse_regulator_RC_edit_",
          "input$TI_MAwR_reverse_regulator_RC_value_edit_",
          n.r.reg,
          reactants,
          products,
          FALSE)
        RMs     <- r.regs[["regulators"]]
        RM.RC   <- r.regs[["rateConstants"]]
        RM.ids  <- r.regs[["reg.ids"]]
        RM.vals <- r.regs[["regulator.val"]]
        
        RM.rc.descript <- r.regs[["rc.descript"]]
        
        Reverse.Mods    <- paste0(RMs, collapse = ", ")
        Reverse.Mods.id <- paste0(RM.ids, collapse = ", ")
        Reverse.Pars    <- paste0(RM.RC, collapse = ", ")
        
        for (i in seq_along(RM.RC)) {
          u <- DetermineRateConstantUnits("1",
                                          rv.UNITS$units.base$For.Var,
                                          rv.UNITS$units.base$Volume,
                                          rv.UNITS$units.base$Duration,
                                          rv.UNITS$units.selected$For.Var,
                                          rv.UNITS$units.selected$Volume,
                                          rv.UNITS$units.selected$Duration)
          
          # Perform conversion to base units if needed
          if (u$unit != u$unit.base) {
            base.val <- UnitConversion(u$unit.d,
                                       u$unit,
                                       u$base.unit,
                                       as.numeric(RM.vals[i]))
          } else {
            base.val <- RM.vals[i]
          }
          
          parameters         <- c(parameters, RM.RC[i])
          param.vals         <- c(param.vals, RM.vals[i])
          param.units        <- c(param.units, u$unit)
          unit.descriptions  <- c(unit.descriptions, u$unit.d)
          param.descriptions <- c(param.descriptions, RM.rc.descript[i])
          base.units         <- c(base.units, u$unit.base)
          base.values        <- c(base.values, base.val)
        }
      } 
      else {
        kr     <- input$TI_MAwR_reverse_k_edit
        kr.val <- input$TI_MAwR_reverse_k_value_edit
        kr.id  <- FindId(kr)
        # Build Rate Constant Units
        kr.unit <- DetermineRateConstantUnits(
          r.stoich,
          rv.UNITS$units.base$For.Var,
          rv.UNITS$units.base$Volume,
          rv.UNITS$units.base$Duration,
          rv.UNITS$units.selected$For.Var,
          rv.UNITS$units.selected$Volume,
          rv.UNITS$units.selected$Duration
        )
        
        # Convert rate constant units if necessary
        if (kr.unit$unit != kr.unit$unit.base) {
          kr.base.val <- UnitConversion(kr.unit$unit.description,
                                        kr.unit$unit,
                                        kr.unit$base.unit,
                                        as.numeric(kr.val))
        } else {
          kr.base.val <- kr.val
        }
        
        # Write Unit Descriptions
        kr.d <- paste0("Reverse rate constant for the reaction of ",
                       reactants,
                       " to ",
                       products
        )
        
        parameters         <- c(parameters, kr)
        param.vals         <- c(param.vals, kr.val)
        param.units        <- c(param.units,kr.unit$unit)
        unit.descriptions  <- c(unit.descriptions, kr.unit$unit.description)
        param.descriptions <- c(param.descriptions, kr.d)
        base.units         <- c(base.units, kr.unit$unit.base)
        base.values        <- c(base.values, kr.base.val)
      }
    }
    
    # Build Modifier Structures
    if (has.f.reg & has.r.reg) {
      modifiers    <- c(FMs, RMs)
      modifiers.id <- c(FM.ids, RM.ids)
    } else if (has.f.reg & !has.r.reg) {
      modifiers    <- FMs
      modifiers.id <- FM.ids
    } else if (!has.f.reg & has.r.reg) {
      modifiers    <- RMs
      modifiers.id <- RM.ids
    } else {
      #pass
    }
    
    eqn.d <- "Mass Action with Regulation"

    laws <- Regulated_Law_Of_Mass_Action(r.stoich, 
                                         reactants,
                                         p.stoich,
                                         products,
                                         reversible,
                                         kf,
                                         kr,
                                         volume.var,
                                         has.f.reg,
                                         Forward.Mods,
                                         Forward.Pars,
                                         has.r.reg,
                                         Reverse.Mods,
                                         Reverse.Pars) 
    
    # Extract reaction laws 
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    content.ml  <- laws$content.ml
  }
  else if (eqn.reaction.law == "synthesis") {
    
    # Separate if factor or not
    if (input$CB_synthesis_factor_checkbox_edit) {
      # Synthesis uses a factor
      eqn.d    <- "Synthesis Reaction by Factor"
      eqn.display <- "Synthesis (Factor)"
      backend.call <- "synthesis_factor"
      
      var.syn    <- input$PI_synthesis_byFactor_var_edit
      var.syn.id <- FindId(var.syn)
      factor     <- input$PI_synthesis_byFactor_factor_edit
      factor.id  <- FindId(factor)
      
      # factor is not involved in differential equations
      modifiers    <- factor
      modifiers.id <- factor.id
      
      products    <- var.syn
      products.id <- var.syn.id
      
      species     <- c(species, var.syn)
      species.id  <- c(species.id, var.syn.id)
      
      parameter          <- input$TI_synthesis_byFactor_RC_edit
      param.val          <- input$TI_synthesis_byFactor_RC_value_edit
      base.unit          <- paste0("1/", rv.UNITS$units.base$Duration)
      param.unit         <- paste0("1/", rv.UNITS$units.selected$Duration)
      unit.description   <- "num <div> time"
      param.description  <- paste0("Synthesis rate constant of ", 
                                   species,
                                   " by factor ",
                                   factor)
      
      # Base unit conversion if necessary
      if (param.unit != base.unit) {
        base.val <- UnitConversion(unit.description,
                                   param.unit,
                                   base.unit,
                                   as.numeric(param.val))
      } else {
        base.val <- param.val
      }
      
      parameters          <- c(parameters, parameter)
      param.vals          <- c(param.vals, param.val)
      param.units         <- c(param.units, param.unit)
      unit.descriptions   <- c(unit.descriptions, unit.description)
      param.descriptions  <- c(param.descriptions, param.description)
      base.units          <- c(base.units, base.unit)
      base.values         <- c(base.values, base.val)
      
      laws <- Synthesis_By_Factor(parameter, factor)
      
    } else {
      # Synthesis by rate
      eqn.d       <- "Synthesis Reaction by Rate"
      eqn.display <- "Synthesis (Rate)"
      backend.call <- "synthesis_base_rate"
      
      modifiers    <- NA
      modifiers.id <- NA
      
      var.syn    <- input$PI_synthesis_rate_var_edit
      var.syn.id <- FindId(var.syn)
      factor     <- NA
      factor.id  <- NA
      
      products    <- var.syn
      products.id <- var.syn.id
      
      species     <- c(species, var.syn)
      species.id  <- c(species.id, var.syn.id)
      
      parameter          <- input$TI_synthesis_rate_RC_edit
      param.val          <- input$TI_synthesis_rate_RC_value_edit
      base.unit          <- paste0(rv.UNITS$units.base$For.Var,
                                   "/", 
                                   "(",
                                   rv.UNITS$units.base$Volume,
                                   "*",
                                   rv.UNITS$units.base$Duration,
                                   ")")
      param.unit          <- paste0(rv.UNITS$units.selected$For.Var,
                                    "/", 
                                    "(",
                                    rv.UNITS$units.selected$Volume,
                                    "*",
                                    rv.UNITS$units.selected$Duration,
                                    ")")
      unit.description   <- paste0("conc (", 
                                   rv.UNITS$units.base$For.Var,
                                   ")",
                                   " <div> ",
                                   "<group> volume <multiply> time <endgroup>"
      )
      param.description  <- paste0("Synthesis rate constant of ", 
                                   species,
                                   " by factor ",
                                   factor)
      
      # Base unit conversion if necessary
      if (param.unit != base.unit) {
        base.val <- UnitConversion(unit.description,
                                   param.unit,
                                   base.unit,
                                   as.numeric(param.val))
      } else {
        base.val <- param.val
      }
      
      parameters          <- c(parameters, parameter)
      param.vals          <- c(param.vals, param.val)
      param.units         <- c(param.units, param.unit)
      unit.descriptions   <- c(unit.descriptions, unit.description)
      param.descriptions  <- c(param.descriptions, param.description)
      base.units          <- c(base.units, base.unit)
      base.values         <- c(base.values, base.val)
      
      laws <- Synthesis_By_Rate(parameter)
      
    }
    
  }
  else if (eqn.reaction.law == "degradation_rate") {
    # browser()
    eqn.d       <- "Degrdation by Rate"
    eqn.display <- "Degradation (Rate)"
    
    modifiers    <- NA
    modifiers.id <- NA
    
    deg.species    <- input$PI_degradation_rate_species_edit
    deg.species.id <- FindId(deg.species)
    ConcDep        <- input$CB_degradation_rate_conc_dependent_edit
    
    reactants    <- deg.species
    reactants.id <- deg.species.id
    
    if (ConcDep) {
      backend.call <- "degradation_rate_concDep"
    } else {
      backend.call <- "degradation_rate_not_concDep"
    }
    
    # Check to see if products are being produced and store them
    if (input$CB_degradation_rate_toProducts_edit) {
      if (ConcDep) {
        backend.call <- "degradation_rate_concDep_products"
      } else {
        backend.call <- "degradation_rate_not_concDep_products"
      }
      products    <- c()
      products.id <- c()
      num.deg.products <- 
        as.numeric(input$NI_degradation_rate_num_products_edit)
      for (i in seq(num.deg.products)) {
        prod <- eval(
          parse(text = paste0("input$PI_degradation_rate_product_edit_", 
                                         as.character(i))))
        prod.id <- FindId(prod)
        
        products <- c(products, prod)
        products.id <- c(products.id, prod.id)
      }
      # Collapse Products into string list if needed
      products.collapsed     <- paste0(products, collapse = ", ")
      products.id.collapsed  <- paste0(products.id, collapse = ", ")
    } else {
      products               <- NA
      products.id            <- NA
      products.collapsed     <- NA
      products.id.collapsed  <- NA
    }
    
    if (!is.na(products.collapsed)) {
      species    <- c(deg.species, products)
      species.id <- c(deg.species.id, products.id)
    } else {
      species    <- deg.species
      species.id <- deg.species.id
    }
    
    parameter         <- input$TI_degradation_rate_RC_edit
    param.val         <- input$TI_degradation_rate_RC_value_edit
    base.unit         <- paste0("1/", rv.UNITS$units.base$Duration)
    param.unit        <- paste0("1/", rv.UNITS$units.selected$Duration)
    unit.description  <- "num <div> time"
    param.description <- paste0("Degradation rate constant for ", species)
    
    # Base unit conversion if necessary
    if (param.unit != base.unit) {
      base.val <- UnitConversion(unit.description,
                                 param.unit,
                                 base.unit,
                                 as.numeric(param.val))
    } else {
      base.val <- param.val
    }
    
    parameters          <- c(parameters, parameter)
    param.vals          <- c(param.vals, param.val)
    param.units         <- c(param.units, param.unit)
    unit.descriptions   <- c(unit.descriptions, unit.description)
    param.descriptions  <- c(param.descriptions, param.description)
    base.units          <- c(base.units, base.unit)
    base.values         <- c(base.values, base.val)
    
    # Add krel parameter if products are being produced AND relative formation is checked
    krel.param <- NA
    krel.param.id <- NA
    if (input$CB_degradation_rate_toProducts_edit && isTruthy(input$CB_degradation_rate_relative_formation_edit)) {
      krel.param         <- input$TI_degradation_rate_krel_edit
      krel.param.val     <- input$NI_degradation_rate_krel_value_edit
      krel.base.unit     <- "dimensionless"
      krel.param.unit    <- "dimensionless"
      krel.unit.desc     <- "dimensionless"
      krel.param.desc    <- paste0("Product yield fraction for degradation of ", deg.species)
      
      parameters          <- c(parameters, krel.param)
      param.vals          <- c(param.vals, krel.param.val)
      param.units         <- c(param.units, krel.param.unit)
      unit.descriptions   <- c(unit.descriptions, krel.unit.desc)
      param.descriptions  <- c(param.descriptions, krel.param.desc)
      base.units          <- c(base.units, krel.base.unit)
      base.values         <- c(base.values, krel.param.val)
    }
    
    # Store Rate Law
    laws <- Degradation_By_Rate(parameter, ConcDep, deg.species)
  }
  else if (eqn.reaction.law == "degradation_by_enzyme") {
    print("DEBUG: ===== ENTERING degradation_by_enzyme block =====")
    print(paste("DEBUG: eqn.ID =", eqn.ID))
    print(paste("DEBUG: length(parameters) at start =", length(parameters)))
    
    eqn.d       <- "Degrdation by enzyme"
    eqn.display <- "Degradation (By Enzyme)"

    # Initialize vars that are pathway dependent to NA
    modifiers    <- NA
    modifiers.id <- NA
    enzyme       <- NA
    enzyme.id    <- NA
    kcat         <- NA
    kcat.id      <- NA
    Vmax         <- NA
    Vmax.id      <- NA
    
    deg.species    <- input$PI_degradation_enzyme_species_edit
    deg.species.id <- FindId(deg.species)
    
    reactants    <- deg.species
    reactants.id <- deg.species.id
    
    Use.Vmax   <- input$CB_degradation_enzyme_useVmax_edit
    
    # browser()
    # Check to see if products are being produced and store them
    if (input$CB_degradation_enzyme_toProducts_edit) {
      backend.call <- "degradation_by_enzyme_wProducts"
      
      products    <- c()
      products.id <- c()
      num.deg.products <- 
        as.numeric(input$NI_degradation_enzyme_num_products_edit)
      for (i in seq(num.deg.products)) {
        prod <- eval(
          parse(text = paste0("input$PI_degradation_enzyme_product_edit_", 
                                         as.character(i))))
        prod.id <- FindId(prod)
        
        products    <- c(products, prod)
        products.id <- c(products.id, prod.id)
      }
      # Collapse Products into string list if needed
      products.collapsed     <- paste0(products, collapse = ", ")
      products.id.collapsed  <- paste0(products.id, collapse = ", ")
    } else {
      products               <- NA
      products.id            <- NA
      products.collapsed     <- NA
      products.id.collapsed  <- NA
    }
    
    if (!is.na(products.collapsed)) {
      species    <- c(deg.species, products)
      species.id <- c(deg.species.id, products.id)
    } else {
      species    <- deg.species
      species.id <- deg.species.id
    }
    
    # Km Rate Constant
    Km               <- input$TI_degradation_enzyme_Km_edit
    Km.val           <- input$TI_degradation_enzyme_Km_value_edit
    Km.unit          <- paste0(rv.UNITS$units.selected$For.Var,
                               "/",
                               rv.UNITS$units.selected$Volume
    )
    Km.base.unit     <- paste0(rv.UNITS$units.base$For.Var,
                               "/",
                               rv.UNITS$units.base$Volume
    )
    Km.unit.descript <- paste0("conc (", rv.UNITS$units.base$For.Var, ")",
                               " <div> ",
                               "volume")
    Km.descript      <- paste0("Michelias Menten constant for degradation of ",
                               species)
    
    # Base unit conversion if necessary
    if (Km.unit != Km.base.unit) {
      Km.base.val <- UnitConversion(Km.unit.descript,
                                    Km.unit,
                                    Km.base.unit,
                                    as.numeric(Km.val))
    } else {
      Km.base.val <- Km.val
    }
    
    # Store Km Parameter
    parameters          <- c(parameters, Km)
    param.vals          <- c(param.vals, Km.val)
    param.units         <- c(param.units, Km.unit)
    unit.descriptions   <- c(unit.descriptions, Km.unit.descript)
    param.descriptions  <- c(param.descriptions, Km.descript)
    base.units          <- c(base.units, Km.base.unit)
    base.values         <- c(base.values, Km.base.val)
    
    # If Uses Vmax 
    if (Use.Vmax) {
      # In this option the reaction used Vmax instead of kcat*enzyme
      backend.call <- "degradation_by_enzyme_use_vmax"
      
      # Vmax Rate Constant
      Vmax               <- input$TI_degradation_enzyme_Vmax_edit
      Vmax.val           <- input$TI_degradation_enzyme_Vmax_value_edit
      Vmax.base.unit     <- paste0(rv.UNITS$units.base$For.Var,
                                   "/", 
                                   "(",
                                   rv.UNITS$units.base$Volume,
                                   "*",
                                   rv.UNITS$units.base$Duration,
                                   ")")
      Vmax.unit          <- paste0(rv.UNITS$units.selected$For.Var,
                                   "/", 
                                   "(",
                                   rv.UNITS$units.selected$Volume,
                                   "*",
                                   rv.UNITS$units.selected$Duration,
                                   ")")
      Vmax.unit.descript   <- paste0("conc (",
                                     rv.UNITS$units.base$For.Var,
                                     ")",
                                     " <div> ",
                                     "<group> volume <multiply> time <endgroup>"
      )
      Vmax.descript    <- paste0("Maximum Velocity for degradation of ", 
                                 species)
      
      if (Vmax.unit != Vmax.base.unit) {
        Vmax.base.val <- UnitConversion(Vmax.unit.descript,
                                        Vmax.unit,
                                        Vmax.base.unit,
                                        as.numeric(Vmax.val))
      } else {
        Vmax.base.val <- Vmax.val
      }
      
      # Store Vmax Parameter
      parameters          <- c(parameters, Vmax)
      param.vals          <- c(param.vals, Vmax.val)
      param.units         <- c(param.units, Vmax.unit)
      unit.descriptions   <- c(unit.descriptions, Vmax.unit.descript)
      param.descriptions  <- c(param.descriptions, Vmax.descript)
      base.units          <- c(base.units, Vmax.base.unit)
      base.values         <- c(base.values, Vmax.base.val)
      
      # Store Rate Law
      laws <- Degradation_By_Enzyme_Vmax(deg.species, Km, Vmax)
    } else {
      # In this option kcat*enzyme is used instead of Vmax for reaction
      backend.call <- "degradation_by_enzyme_no_vmax"
      
      enzyme    <- input$PI_degradation_enzyme_enzyme_edit
      enzyme.id <- FindId(enzyme)
      
      modifiers    <- enzyme
      modifiers.id <- enzyme.id
      
      
      # kcat
      kcat               <- input$TI_degradation_enzyme_kcat_edit
      kcat.val           <- input$TI_degradation_enzyme_kcat_value_edit
      kcat.base.unit     <- paste0("1/", rv.UNITS$units.base$Duration)
      kcat.unit          <- paste0("1/", rv.UNITS$units.selected$Duration)
      kcat.unit.descript <- "num <div> time"
      kcat.descript      <- paste0("Enzymatic degradation rate constant of ", 
                                   species,
                                   " by ",
                                   enzyme)
      
      if (kcat.unit != kcat.base.unit) {
        kcat.base.val <- UnitConversion(kcat.unit.descript,
                                        kcat.unit,
                                        kcat.base.unit,
                                        as.numeric(kcat.val))
      } else {
        kcat.base.val <- kcat.val
      }
      
      # Store kcat Parameter
      parameters          <- c(parameters, kcat)
      param.vals          <- c(param.vals, kcat.val)
      param.units         <- c(param.units, kcat.unit)
      unit.descriptions   <- c(unit.descriptions, kcat.unit.descript)
      param.descriptions  <- c(param.descriptions, kcat.descript)
      base.units          <- c(base.units, kcat.base.unit)
      base.values         <- c(base.values, kcat.base.val)
      
      # Store Rate Law
      laws <- Degradation_By_Enzyme_no_Vmax(deg.species, Km, kcat, enzyme)
    }
    
    # Add krel parameter if products are being produced AND relative formation is checked
    print("DEBUG: degradation_by_enzyme - checking for krel")
    print(paste("DEBUG: CB_degradation_enzyme_toProducts_edit =", input$CB_degradation_enzyme_toProducts_edit))
    print(paste("DEBUG: CB_degradation_enzyme_relative_formation_edit =", isTruthy(input$CB_degradation_enzyme_relative_formation_edit)))
    print(paste("DEBUG: length(parameters) before krel =", length(parameters)))
    
    krel.param <- NA
    krel.param.id <- NA
    if (input$CB_degradation_enzyme_toProducts_edit && isTruthy(input$CB_degradation_enzyme_relative_formation_edit)) {
      print("DEBUG: Adding krel parameter to parameters vector")
      krel.param         <- input$TI_degradation_enzyme_krel_edit
      krel.param.val     <- input$NI_degradation_enzyme_krel_value_edit
      krel.base.unit     <- "dimensionless"
      krel.param.unit    <- "dimensionless"
      krel.unit.desc     <- "dimensionless"
      krel.param.desc    <- paste0("Product yield fraction for degradation of ", deg.species)
      
      parameters          <- c(parameters, krel.param)
      param.vals          <- c(param.vals, krel.param.val)
      param.units         <- c(param.units, krel.param.unit)
      unit.descriptions   <- c(unit.descriptions, krel.unit.desc)
      param.descriptions  <- c(param.descriptions, krel.param.desc)
      base.units          <- c(base.units, krel.base.unit)
      base.values         <- c(base.values, krel.param.val)
      print(paste("DEBUG: length(parameters) after krel =", length(parameters)))
      print(paste("DEBUG: krel.param =", krel.param))
    }
    
    # Extract reaction laws 
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    content.ml  <- laws$content.ml
  }
  else if (eqn.reaction.law == "michaelis_menten") {
    # Initialize vars that are pathway dependent to NA
    modifiers    <- NA
    modifiers.id <- NA
    enzyme       <- NA
    enzyme.id    <- NA
    kcat         <- NA
    kcat.id      <- NA
    Vmax         <- NA
    Vmax.id      <- NA
    
    
    eqn.d       <- "Michaelis Menten Enzyme Kinetics"
    eqn.display <- "Michaelis Menten"
    
    substrate    <- input$PI_michaelis_menten_substrate_edit
    substrate.id <- FindId(substrate)
    
    reactants    <- substrate
    reactants.id <- substrate.id
    products      <- input$PI_michaelis_menten_product_edit
    products.id   <- FindId(products)
    
    species    <- c(reactants, products)
    species.id <- c(reactants.id, products.id)
    
    Use.Vmax   <- input$CB_michaelis_menten_useVmax_edit
    
    # Km Rate Constant
    Km               <- input$TI_michaelis_menten_Km_edit
    Km.val           <- input$TI_michaelis_menten_Km_value_edit
    Km.unit          <- paste0(rv.UNITS$units.selected$For.Var,
                               "/",
                               rv.UNITS$units.selected$Volume
    )
    Km.base.unit     <- paste0(rv.UNITS$units.base$For.Var,
                               "/",
                               rv.UNITS$units.base$Volume
    )
    Km.unit.descript <- paste0("conc (", rv.UNITS$units.base$For.Var, ")",
                               " <div> ",
                               "volume")
    Km.descript      <- paste0("Michelias Menten constant for enzymatic", 
                               " conversion of ",
                               species,
                               " to ",
                               products)
    
    # Base unit conversion if necessary
    if (Km.unit != Km.base.unit) {
      Km.base.val <- UnitConversion(Km.unit.descript,
                                    Km.unit,
                                    Km.base.unit,
                                    as.numeric(Km.val))
    } else {
      Km.base.val <- Km.val
    }
    
    # Store Km Parameter
    parameters          <- c(parameters, Km)
    param.vals          <- c(param.vals, Km.val)
    param.units         <- c(param.units, Km.unit)
    unit.descriptions   <- c(unit.descriptions, Km.unit.descript)
    param.descriptions  <- c(param.descriptions, Km.descript)
    base.units          <- c(base.units, Km.base.unit)
    base.values         <- c(base.values, Km.base.val)
    
    # If Uses Vmax 
    if (Use.Vmax) {
      # In this option the reaction used Vmax instead of kcat*enzyme
      backend.call <- "michaelis_menten_use_vmax"
      
      # Vmax Rate Constant
      Vmax               <- input$TI_michaelis_menten_vmax_edit
      Vmax.val           <- input$TI_michaelis_menten_vmax_value_edit
      Vmax.base.unit     <- paste0(rv.UNITS$units.base$For.Var,
                                   "/", 
                                   "(",
                                   rv.UNITS$units.base$Volume,
                                   "*",
                                   rv.UNITS$units.base$Duration,
                                   ")")
      Vmax.unit          <- paste0(rv.UNITS$units.selected$For.Var,
                                   "/", 
                                   "(",
                                   rv.UNITS$units.selected$Volume,
                                   "*",
                                   rv.UNITS$units.selected$Duration,
                                   ")")
      Vmax.unit.descript   <- paste0("conc (",
                                     rv.UNITS$units.base$For.Var,
                                     ")",
                                     " <div> ",
                                     "<group> volume <multiply> time <endgroup>"
      )
      kcat.descript      <- paste0("Enzymatic rate constant for the",
                                   " conversion of ",
                                   species,
                                   " to ",
                                   products, 
                                   " by ",
                                   enzyme)
      
      if (Vmax.unit != Vmax.base.unit) {
        Vmax.base.val <- UnitConversion(Vmax.unit.descript,
                                        Vmax.unit,
                                        Vmax.base.unit,
                                        as.numeric(Vmax.val))
      } else {
        Vmax.base.val <- Vmax.val
      }
      
      # Store Vmax Parameter
      parameters          <- c(parameters, Vmax)
      param.vals          <- c(param.vals, Vmax.val)
      param.units         <- c(param.units, Vmax.unit)
      unit.descriptions   <- c(unit.descriptions, Vmax.unit.descript)
      param.descriptions  <- c(param.descriptions, Vmax.descript)
      base.units          <- c(base.units, Vmax.base.unit)
      base.values         <- c(base.values, Vmax.base.val)
      
      # Find Rate Law
      laws <- Henri_Michaelis_Menten_Vmax(substrate, Km, Vmax)
    } else {
      # In this option kcat*enzyme is used instead of Vmax for reaction
      backend.call <- "michaelis_menten_convert_vmax"
      
      enzyme    <- input$PI_michaelis_menten_enzyme_edit
      enzyme.id <- FindId(enzyme)
      
      modifiers    <- enzyme
      modifiers.id <- enzyme.id
      
      
      # kcat
      kcat               <- input$TI_michaelis_menten_kcat_edit
      kcat.val           <- input$TI_michaelis_menten_kcat_value_edit
      kcat.base.unit     <- paste0("1/", rv.UNITS$units.base$Duration)
      kcat.unit          <- paste0("1/", rv.UNITS$units.selected$Duration)
      kcat.unit.descript <- "num <div> time"
      kcat.descript      <- paste0("Enzymatic degradation rate constant of ", 
                                   species,
                                   " by ",
                                   enzyme)
      
      if (kcat.unit != kcat.base.unit) {
        kcat.base.val <- UnitConversion(kcat.unit.descript,
                                        kcat.unit,
                                        kcat.base.unit,
                                        as.numeric(kcat.val))
      } else {
        kcat.base.val <- kcat.val
      }
      
      # Store kcat Parameter
      parameters          <- c(parameters, kcat)
      param.vals          <- c(param.vals, kcat.val)
      param.units         <- c(param.units, kcat.unit)
      unit.descriptions   <- c(unit.descriptions, kcat.unit.descript)
      param.descriptions  <- c(param.descriptions, kcat.descript)
      base.units          <- c(base.units, kcat.base.unit)
      base.values         <- c(base.values, kcat.base.val)
      
      # Store rate law
      laws <- Henri_Michaelis_Menten_no_Vmax(substrate, Km, kcat, enzyme)
    }
  }
  
  # browser()
  
  #Error Check
  # We need parameter name, unit description
  passed.error.check <- TRUE
  for (i in seq_along(parameters)) {
    par.error.DS <- list("Name" = parameters[i],
                         "UnitDescription" = unit.descriptions[i])
    error.check <- CheckParametersForErrors(par.error.DS,
                                            rv.SPECIES$species,
                                            rv.PARAMETERS$parameters,
                                            rv.COMPARTMENTS$compartments)
    passed.check <- error.check[[1]]
    # Break loop and return error message if parameter fails check
    if (!passed.check) {passed.error.check <- FALSE}
  }
  
  # #Error Check
  # error.check <- CheckParametersForErrors(parameters, 
  #                                         rv.SPECIES$species.names,
  #                                         names(rv.PARAMETERS$parameters))
  # passed.error.check <- error.check[[1]]
  print(paste("DEBUG: passed.error.check =", passed.error.check))
  print(paste("DEBUG: length(parameters) =", length(parameters)))
  if (passed.error.check) {
    print("DEBUG: Creating par.ids vector")
    par.ids <- c()
    # Check to see if parameter names have changed (meaning new parameter)
    if (length(setdiff(old.params, parameters)) == 0) {
      # parameter names have not changed (may be reordered)
      # Map and update parameters by name to avoid index-shift bugs
      for (i in seq_along(old.params)) {
        par.name <- old.params[i]
        # find index in the current parameters vector
        idx <- which(parameters == par.name)
        if (length(idx) == 0) {
          # fallback: skip if not found
          next
        }
        par.id <- FindId(par.name)
        par.ids <- c(par.ids, par.id)
        rv.PARAMETERS$parameters[[par.id]]$Value <- as.numeric(param.vals[idx])
        rv.PARAMETERS$parameters[[par.id]]$Unit <- param.units[idx]
        rv.PARAMETERS$parameters[[par.id]]$UnitDescription <- unit.descriptions[idx]
        rv.PARAMETERS$parameters[[par.id]]$BaseUnit <- base.units[idx]
        rv.PARAMETERS$parameters[[par.id]]$BaseValue <- as.numeric(base.values[idx])
        rv.PARAMETERS$parameters[[par.id]]$Description <- param.descriptions[idx]
      }
    } else {
      # Parameter names have changed 
      params.to.add  <- setdiff(parameters, old.params)
      params.to.del  <- setdiff(old.params, parameters)
      same.params    <- intersect(old.params, parameters)
      
      # Edit same params: map by name into current `parameters` vector
      for (i in seq_along(same.params)) {
        par.name <- same.params[i]
        par.id <- FindId(par.name)
        # find index in the current parameters vector
        idx <- which(parameters == par.name)
        if (length(idx) == 0) next
        par.ids <- c(par.ids, par.id)

        ids.used.in <- c(rv.PARAMETERS$parameters[[par.id]]$Used.In, eqn.ID)
        types <- c(rv.PARAMETERS$parameters[[par.id]]$Type, "Reaction")
        type.n <- c(rv.PARAMETERS$parameters[[par.id]]$Type.Note, eqn.reaction.law)
        is.custom <- rv.PARAMETERS$parameters[[par.id]]$Custom

        # Write out to parameter using the mapped index
        to.par.list <- list("Name"            = par.name,
                            "ID"              = par.id,
                            "Value"           = as.numeric(param.vals[idx]),
                            "Unit"            = param.units[idx],
                            "UnitDescription" = unit.descriptions[idx],
                            "BaseUnit"        = base.units[idx],
                            "BaseValue"       = as.numeric(base.values[idx]),
                            "Description"     = param.descriptions[idx],
                            "Type"            = collapseVector(types),
                            "Type.Note"       = collapseVector(type.n),
                            "Used.In"         = collapseVector(ids.used.in),
                            "Custom"          = is.custom
        )

        # Append parameter entry
        rv.PARAMETERS$parameters[[par.id]] <- to.par.list
      }

      # Add new params: map by name into current `parameters` vector
      for (i in seq_along(params.to.add)) {
        orig.pname <- params.to.add[i]
        idx <- which(parameters == orig.pname)
        pname <- orig.pname
        # If the desired name already exists globally, check whether it's used by other equations.
        # If it is used elsewhere, generate a unique suffixed name instead of reusing the existing parameter.
        if (pname %in% rv.PARAMETERS$parameters.names) {
          existing.id <- FindId(pname)
          used.in.raw <- rv.PARAMETERS$parameters[[existing.id]]$Used.In
          existing.used.in <- if (is.na(used.in.raw) || used.in.raw == "") character(0) else strsplit(used.in.raw, ", ")[[1]]
          # If the existing parameter is associated with any id other than this equation, create a new unique name
          if (length(existing.used.in) > 0 && !(length(existing.used.in) == 1 && existing.used.in == eqn.ID)) {
            base <- pname
            n <- 2L
            new.pname <- paste0(base, "_", n)
            while (new.pname %in% rv.PARAMETERS$parameters.names) {
              n <- n + 1L
              new.pname <- paste0(base, "_", n)
            }
            pname <- new.pname
            par.gen <- GenerateId(rv.ID$id.param.seed, "parameter")
            rv.ID$id.param.seed <- par.gen$seed
            par.id <- par.gen$id
            # Store ID to database using the new unique name
            idx.to.add <- nrow(rv.ID$id.df) + 1
            rv.ID$id.df[idx.to.add, ] <- c(par.id, pname)
          } else {
            par.id <- existing.id
          }
        } else {
          par.gen <- GenerateId(rv.ID$id.param.seed, "parameter")
          rv.ID$id.param.seed <- par.gen$seed
          par.id <- par.gen$id
          # Store ID to database
          idx.to.add <- nrow(rv.ID$id.df) + 1
          rv.ID$id.df[idx.to.add, ] <- c(par.id, pname)
        }
        par.ids <- c(par.ids, par.id)

        # Compose parameter entry using mapped index (if available)
        if (length(idx) == 0) {
          val <- NA; unit <- NA; udesc <- NA; bunit <- NA; bval <- NA; desc <- NA
        } else {
          val <- as.numeric(param.vals[idx])
          unit <- param.units[idx]
          udesc <- unit.descriptions[idx]
          bunit <- base.units[idx]
          bval <- as.numeric(base.values[idx])
          desc <- param.descriptions[idx]
        }

        if (pname %in% rv.PARAMETERS$parameters.names) {
          ids.used.in <- c(rv.PARAMETERS$parameters[[par.id]]$Used.In, eqn.ID)
          types <- c(rv.PARAMETERS$parameters[[par.id]]$Type, "Reaction")
          type.n <- c(rv.PARAMETERS$parameters[[par.id]]$Type.Note, eqn.reaction.law)
          is.custom <- rv.PARAMETERS$parameters[[par.id]]$Custom
        } else {
          ids.used.in <- eqn.ID
          types <- "Reaction"
          type.n <- eqn.reaction.law
          is.custom <- FALSE
        }

        to.par.list <- list("Name"            = pname,
                            "ID"              = par.id,
                            "Value"           = val,
                            "Unit"            = unit,
                            "UnitDescription" = udesc,
                            "BaseUnit"        = bunit,
                            "BaseValue"       = bval,
                            "Description"     = desc,
                            "Type"            = collapseVector(types),
                            "Type.Note"       = collapseVector(type.n),
                            "Used.In"         = collapseVector(ids.used.in),
                            "Custom"          = is.custom)

        rv.PARAMETERS$parameters[[par.id]] <- to.par.list
      }

      for (i in seq_along(params.to.del)) {
        
        par.id <- FindId(params.to.del[i])
        # Find ids attached to parameter
        associated.ids <- 
          strsplit(rv.PARAMETERS$parameters[[par.id]]$Used.In, ", ")[[1]]
        
        if (length(associated.ids) == 1) {
          # This means this eqn was the only id and parameter can be removed
          rv.PARAMETERS$parameters[[par.id]] <- NULL
          rv.ID$id.df <- filter(rv.ID$id.df, id != par.id)
        } else {
          #find the idx of this id and remove its
          # information from that parameter
          idx <- which(associated.ids %in% par.id)
          
          type <- 
            strsplit(rv.PARAMETERS$parameters[[par.id]]$Type, ", ")[[1]]
          type.note <- 
            strsplit(rv.PARAMETERS$parameters[[par.id]]$Type.Note, ", ")[[1]]
          used.in <- 
            strsplit(rv.PARAMETERS$parameters[[par.id]]$Used.In, ", ")[[1]]
          
          new.type      <- collapseVector(type[-idx])
          new.type.note <- collapseVector(type.note[-idx])
          new.used.in   <- collapseVector(used.in[-idx])
          
          rv.PARAMETERS$parameters[[par.id]]$Type      <- new.type
          rv.PARAMETERS$parameters[[par.id]]$Type.Note <- new.type.note
          rv.PARAMETERS$parameters[[par.id]]$Used.In   <- new.used.in
        }
      }

      # After deletions, rebuild par.ids in the order of current `parameters`
      par.ids <- c()
      if (length(parameters) > 0) {
        for (i in seq_along(parameters)) {
          # Only include parameters that still exist in rv.PARAMETERS
          pid <- FindId(parameters[i])
          par.ids <- c(par.ids, pid)
        }
      }
    }
    # browser()
    # Remove species that changed from eqns and add those that are new
    
    # Find different in old and new species ids
    species.id.add  <- setdiff(species.id, old.species.id)
    species.id.del  <- setdiff(old.species.id, species.id)
    
    # If id is old, find it in species db and remove from reaction ids
    if (length(species.id.add) != 0) {
      for (i in seq_along(species.id.del)) {
        # if its only species, remove it and replace it is NA
        if (length(rv.SPECIES$species[[species.id.del[i]]]$Reaction.ids) == 1) {
          rv.SPECIES$species[[species.id.del[i]]]$Reaction.ids <- NA
        } else {
          # Otherwise split species, find idx, and remove that entry
          
          associated.ids <- strsplit(
            rv.SPECIES$species[[species.id.del[i]]]$Reaction.ids, ", ")[[1]]
          idx <- which(associated.ids %in% species.id.del[i])
          eqn.vec <- collapseVector(associated.ids[-idx])
          rv.SPECIES$species[[species.id.del[i]]]$Reaction.ids <- eqn.vec
        }
        
      }
      
      for (i in seq_along(species.id.add)) {
        if (is.na(rv.SPECIES$species[[species.id.add[i]]]$Reaction.ids)) {
          # If its NA, make current id  the id
          rv.SPECIES$species[[species.id.add[i]]]$Reaction.ids <- eqn.ID
        } else {
          # Else paste0 collapse current id with ", "
          items <- 
            strsplit(
              rv.SPECIES$species[[species.id.add[i]]]$Reaction.ids, ", ")[[1]]
          items <- c(items, eqn.ID)
          rv.SPECIES$species[[species.id.add[i]]]$Reaction.ids <- 
            paste0(items, collapse = ", ")
        }
      }
    }
    
    # Extract reaction laws (skip for reactions that already define these variables)
    if (!eqn.reaction.law %in% c("predator_prey", "logistic_competition", "competitive_monod")) {
      rate.law    <- laws$string
      p.rate.law  <- laws$pretty.string
      latex.law   <- laws$latex
      mathjax.law <- laws$mj
      mathml.law  <- laws$mathml
      content.ml  <- laws$content.ml
    }
    
    # We need to collapse these vector terms otherwise when the list is 
    # converted to a dataframe there will be errors
    # browser()
    par.collapsed          <- collapseVector(parameters)
    par.id.collapsed       <- collapseVector(par.ids)
    reactants.collapsed    <- collapseVector(reactants)
    reactants.id.collapsed <- collapseVector(reactants.id)
    products.collapsed     <- collapseVector(products)
    products.id.collapsed  <- collapseVector(products.id)
    species.collapsed      <- collapseVector(species)
    species.id.collapsed   <- collapseVector(species.id)
    modifiers.collapsed    <- collapseVector(modifiers)
    modifiers.id.collapsed <- collapseVector(modifiers.id)

    # Add overall reaction information
    reaction.entry <- list(
      "ID"               = eqn.ID,
      "Eqn.Display.Type" = eqn.display,
      "Reaction.Law"     = eqn.reaction.law,
      "Backend.Call"     = backend.call,
      "Species"          = species.collapsed,
      "Reactants"        = reactants.collapsed,
      "Products"         = products.collapsed, 
      "Modifiers"        = modifiers.collapsed,
      "Parameters"       = par.collapsed,
      "Compartment"      = compartment,
      "Description"      = eqn.d,
      "Species.id"       = species.id.collapsed,
      "Reactants.id"     = reactants.id.collapsed,
      "Products.id"      = products.id.collapsed,
      "Modifiers.id"     = modifiers.id.collapsed, 
      "Parameters.id"    = par.id.collapsed,
      "Compartment.id"   = compartment.id,
      "Equation.Text"    = equationBuilder_edit(),
      "Equation.Latex"   = equationLatexBuilder_edit(),
      "Equation.MathJax" = equationBuilder_edit_mathJax(),
      "String.Rate.Law"  = rate.law,
      "Pretty.Rate.Law"  = p.rate.law,
      "Latex.Rate.Law"   = latex.law,
      "MathJax.Rate.Law" = mathjax.law,
      "MathMl.Rate.Law"  = mathml.law,
      "Content.MathMl"   = content.ml,
      "Reversible"       = isReversible,
      "Show.In.Table"    = if (eqn.reaction.law %in% c("competitive_monod", "logistic_competition", "predator_prey")) FALSE else TRUE
    )
    
    # For competitive_monod, logistic_competition, and predator_prey, don't update the main entry
    # as they use separate internal entries
    if (!eqn.reaction.law %in% c("competitive_monod", "logistic_competition", "predator_prey")) {
      rv.REACTIONS$reactions[[eqn.ID]] <- reaction.entry
    }
    
    # Build specific reaction type reactive variable
    print(paste("DEBUG: Building specific reaction type for", eqn.reaction.law))
    print(paste("DEBUG: exists('par.ids') before reaction type block =", exists("par.ids")))
    if (exists("par.ids")) {
      print(paste("DEBUG: length(par.ids) =", length(par.ids)))
    } else {
      print("DEBUG: ERROR - par.ids does not exist in reaction type block!")
    }
    
    if (eqn.reaction.law == "mass_action") {
      if (exists("par.ids") && length(par.ids) >= 1) {
        kf.id = par.ids[1]
        if (length(par.ids) >= 2) {
          kr.id = par.ids[2]
        } else {
          kr.id = NA
        }
      } else {
        kf.id = NA
        kr.id = NA
      }
      
      sub.entry <- list(
        "ID" = eqn.ID,
        "Reaction.Law"    = eqn.reaction.law,
        "r.stoichiometry" = r.stoich,
        "Reactants"       = reactants,
        "Reactants.id"    = reactants.id,
        "p.stoichiometry" = p.stoich,
        "Products"        = products,
        "Products.id"     = products.id,
        "Reversible"      = reversible,
        "kf"              = kf,
        "kr"              = kr,
        "kf.id"           = kf.id,
        "kr.id"           = kr.id
      )
      
      # Add to mass action RV
      rv.REACTIONS$massAction[[eqn.ID]] <- sub.entry
    } 
    else if (eqn.reaction.law == "exponential_growth") {
      mu.id <- if (exists("par.ids") && length(par.ids) >= 1) par.ids[1] else NA
      sub.entry <- list(
        "ID"            = eqn.ID,
        "Reaction.Law"  = eqn.reaction.law,
        "Species"       = species,
        "Species.id"    = species.id,
        "Mu"            = parameters[1],
        "Mu.id"         = mu.id,
        "Mu.val"        = param.vals[1],
        "Mu.unit"       = param.units[1],
        "Mu.unit.desc"  = unit.descriptions[1],
        "Mu.base.unit"  = base.units[1],
        "Mu.base.val"   = base.values[1]
      )
      rv.REACTIONS$exponentialGrowth[[eqn.ID]] <- sub.entry
    }
    else if (eqn.reaction.law == "monod_growth") {
      mu_max.id <- if (exists("par.ids") && length(par.ids) >= 1) par.ids[1] else NA
      K_s.id    <- if (exists("par.ids") && length(par.ids) >= 2) par.ids[2] else NA
      sub.entry <- list(
        "ID"            = eqn.ID,
        "Reaction.Law"  = eqn.reaction.law,
        "Species"       = growth.species,
        "Species.id"    = growth.species.id,
        "Substrate"     = substrate,
        "Substrate.id"  = substrate.id,
        "Mu_max"        = parameters[1],
        "Mu_max.id"     = mu_max.id,
        "Mu_max.val"    = param.vals[1],
        "Mu_max.unit"   = param.units[1],
        "Mu_max.unit.desc" = unit.descriptions[1],
        "Mu_max.base.unit" = base.units[1],
        "Mu_max.base.val"  = base.values[1],
        "K_s"           = parameters[2],
        "K_s.id"        = K_s.id,
        "K_s.val"       = param.vals[2],
        "K_s.unit"      = param.units[2],
        "K_s.unit.desc" = unit.descriptions[2],
        "K_s.base.unit" = base.units[2],
        "K_s.base.val"  = base.values[2]
      )
      rv.REACTIONS$monodGrowth[[eqn.ID]] <- sub.entry
    }
    else if (eqn.reaction.law == "competitive_monod") {
      # Create three reaction entries: X, Y, and S (similar to create handler)
      # Update existing entries with new rate laws
      # First, find the three reaction IDs (X, Y, S)
      # They should be linked to the species
      species.x.id <- FindId(species.x)
      species.y.id <- FindId(species.y)
      substrate.id <- FindId(substrate)
      
      # Find reaction IDs for each species
      x.reaction.ids <- strsplit(rv.SPECIES$species[[species.x.id]]$Reaction.ids, ", ")[[1]]
      y.reaction.ids <- strsplit(rv.SPECIES$species[[species.y.id]]$Reaction.ids, ", ")[[1]]
      s.reaction.ids <- strsplit(rv.SPECIES$species[[substrate.id]]$Reaction.ids, ", ")[[1]]
      
      # Find the competitive_monod reaction IDs (they should have this reaction law)
      x.id <- NA; y.id <- NA; s.x.id <- NA; s.y.id <- NA
      for (rid in x.reaction.ids) {
        if (rv.REACTIONS$reactions[[rid]]$Reaction.Law == "competitive_monod") {
          x.id <- rid
          break
        }
      }
      for (rid in y.reaction.ids) {
        if (rv.REACTIONS$reactions[[rid]]$Reaction.Law == "competitive_monod" && rid != x.id) {
          y.id <- rid
          break
        }
      }
      # Find both substrate reaction entries
      s.found <- 0
      for (rid in s.reaction.ids) {
        if (rv.REACTIONS$reactions[[rid]]$Reaction.Law == "competitive_monod" && rid != x.id && rid != y.id) {
          if (s.found == 0) {
            s.x.id <- rid
            s.found <- s.found + 1
          } else {
            s.y.id <- rid
            break
          }
        }
      }
      
      # Update reaction entries with new rate laws
      if (!is.na(x.id)) {
        rv.REACTIONS$reactions[[x.id]]$String.Rate.Law  <- rate.law.x
        rv.REACTIONS$reactions[[x.id]]$Pretty.Rate.Law  <- rate.law.x
        rv.REACTIONS$reactions[[x.id]]$Latex.Rate.Law   <- rate.law.x
        rv.REACTIONS$reactions[[x.id]]$MathJax.Rate.Law <- ConvertRateLaw(rate.law.x)$mathjax
        rv.REACTIONS$reactions[[x.id]]$Species          <- species.x
        rv.REACTIONS$reactions[[x.id]]$Species.id       <- species.id.x
        rv.REACTIONS$reactions[[x.id]]$Reactants        <- substrate
        rv.REACTIONS$reactions[[x.id]]$Reactants.id     <- substrate.id
        rv.REACTIONS$reactions[[x.id]]$Products         <- species.x
        rv.REACTIONS$reactions[[x.id]]$Products.id      <- species.id.x
        rv.REACTIONS$reactions[[x.id]]$Show.In.Table    <- TRUE  # Main entry to show in table
        if (single.species.mode) {
          rv.REACTIONS$reactions[[x.id]]$Modifiers        <- species.y
          rv.REACTIONS$reactions[[x.id]]$Modifiers.id     <- species.id.y
        } else {
          rv.REACTIONS$reactions[[x.id]]$Modifiers        <- NA
          rv.REACTIONS$reactions[[x.id]]$Modifiers.id     <- NA
        }
      }
      if (!is.na(y.id) && !single.species.mode) {
        # Only update Y entry if not in single species mode
        rv.REACTIONS$reactions[[y.id]]$String.Rate.Law  <- rate.law.y
        rv.REACTIONS$reactions[[y.id]]$Pretty.Rate.Law  <- rate.law.y
        rv.REACTIONS$reactions[[y.id]]$Latex.Rate.Law   <- rate.law.y
        rv.REACTIONS$reactions[[y.id]]$MathJax.Rate.Law <- ConvertRateLaw(rate.law.y)$mathjax
        rv.REACTIONS$reactions[[y.id]]$Species          <- species.y
        rv.REACTIONS$reactions[[y.id]]$Species.id       <- species.id.y
        rv.REACTIONS$reactions[[y.id]]$Reactants        <- substrate
        rv.REACTIONS$reactions[[y.id]]$Reactants.id     <- substrate.id
        rv.REACTIONS$reactions[[y.id]]$Products         <- species.y
        rv.REACTIONS$reactions[[y.id]]$Products.id      <- species.id.y
        rv.REACTIONS$reactions[[y.id]]$Modifiers        <- NA
        rv.REACTIONS$reactions[[y.id]]$Modifiers.id     <- NA
        rv.REACTIONS$reactions[[y.id]]$Show.In.Table    <- FALSE  # Hide from table - internal only
      }
      if (!is.na(s.x.id)) {
        rv.REACTIONS$reactions[[s.x.id]]$String.Rate.Law  <- rate.law.s.x
        rv.REACTIONS$reactions[[s.x.id]]$Pretty.Rate.Law  <- rate.law.s.x
        rv.REACTIONS$reactions[[s.x.id]]$Latex.Rate.Law   <- rate.law.s.x
        rv.REACTIONS$reactions[[s.x.id]]$MathJax.Rate.Law <- ConvertRateLaw(rate.law.s.x)$mathjax
        rv.REACTIONS$reactions[[s.x.id]]$Species          <- substrate
        rv.REACTIONS$reactions[[s.x.id]]$Species.id       <- substrate.id
        rv.REACTIONS$reactions[[s.x.id]]$Reactants        <- substrate
        rv.REACTIONS$reactions[[s.x.id]]$Reactants.id     <- substrate.id
        rv.REACTIONS$reactions[[s.x.id]]$Products         <- NA
        rv.REACTIONS$reactions[[s.x.id]]$Products.id      <- NA
        rv.REACTIONS$reactions[[s.x.id]]$Modifiers        <- NA
        rv.REACTIONS$reactions[[s.x.id]]$Modifiers.id     <- NA
        rv.REACTIONS$reactions[[s.x.id]]$Show.In.Table    <- FALSE  # Hide from table - internal only
      }
      if (!is.na(s.y.id) && !single.species.mode) {
        # Only update S from Y entry if not in single species mode
        rv.REACTIONS$reactions[[s.y.id]]$String.Rate.Law  <- rate.law.s.y
        rv.REACTIONS$reactions[[s.y.id]]$Pretty.Rate.Law  <- rate.law.s.y
        rv.REACTIONS$reactions[[s.y.id]]$Latex.Rate.Law   <- rate.law.s.y
        rv.REACTIONS$reactions[[s.y.id]]$MathJax.Rate.Law <- ConvertRateLaw(rate.law.s.y)$mathjax
        rv.REACTIONS$reactions[[s.y.id]]$Species          <- substrate
        rv.REACTIONS$reactions[[s.y.id]]$Species.id       <- substrate.id
        rv.REACTIONS$reactions[[s.y.id]]$Reactants        <- substrate
        rv.REACTIONS$reactions[[s.y.id]]$Reactants.id     <- substrate.id
        rv.REACTIONS$reactions[[s.y.id]]$Products         <- NA
        rv.REACTIONS$reactions[[s.y.id]]$Products.id      <- NA
        rv.REACTIONS$reactions[[s.y.id]]$Modifiers        <- NA
        rv.REACTIONS$reactions[[s.y.id]]$Modifiers.id     <- NA
        rv.REACTIONS$reactions[[s.y.id]]$Show.In.Table    <- FALSE  # Hide from table - internal only
      }
      
      # Update competitiveMonod sub-entry
      mu_max.x.id  <- par.ids[1]
      K_s.x.id     <- par.ids[2]
      alpha.xy.id  <- par.ids[3]
      Kc.id        <- par.ids[4]
      Y_x.id       <- par.ids[5]
      
      if (single.species.mode) {
        sub.entry <- list(
          "ID"           = eqn.ID,
          "Reaction.Law" = eqn.reaction.law,
          "Single.Species.Mode" = TRUE,
          "No.Substrate.Restriction" = no.substrate.restriction,
          "Species.X"    = species.x,
          "Species.X.id" = species.id.x,
          "Species.Y"    = species.y,
          "Species.Y.id" = species.id.y,
          "Substrate"    = substrate,
          "Substrate.id" = substrate.id,
          "mu_max.x"     = parameters[1],
          "mu_max.x.id"  = mu_max.x.id,
          "K_s.x"        = parameters[2],
          "K_s.x.id"     = K_s.x.id,
          "alpha.xy"     = parameters[3],
          "alpha.xy.id"  = alpha.xy.id,
          "Kc"           = parameters[4],
          "Kc.id"        = Kc.id,
          "Y_x"          = parameters[5],
          "Y_x.id"       = Y_x.id
        )
      } else {
        mu_max.y.id  <- par.ids[2]
        K_s.y.id     <- par.ids[4]
        alpha.yx.id  <- par.ids[6]
        Y_y.id       <- par.ids[9]
        sub.entry <- list(
          "ID"           = eqn.ID,
          "Reaction.Law" = eqn.reaction.law,
          "Single.Species.Mode" = FALSE,
          "No.Substrate.Restriction" = no.substrate.restriction,
          "Species.X"    = species.x,
          "Species.X.id" = species.id.x,
          "Species.Y"    = species.y,
          "Species.Y.id" = species.id.y,
          "Substrate"    = substrate,
          "Substrate.id" = substrate.id,
          "mu_max.x"     = parameters[1],
          "mu_max.x.id"  = mu_max.x.id,
          "mu_max.y"     = parameters[2],
          "mu_max.y.id"  = mu_max.y.id,
          "K_s.x"        = parameters[3],
          "K_s.x.id"     = K_s.x.id,
          "K_s.y"        = parameters[4],
          "K_s.y.id"     = K_s.y.id,
          "alpha.xy"     = parameters[5],
          "alpha.xy.id"  = alpha.xy.id,
          "alpha.yx"     = parameters[6],
          "alpha.yx.id"  = alpha.yx.id,
          "Kc"           = parameters[7],
          "Kc.id"        = Kc.id,
          "Y_x"          = parameters[8],
          "Y_x.id"       = Y_x.id,
          "Y_y"          = parameters[9],
          "Y_y.id"       = Y_y.id
        )
      }
      rv.REACTIONS$competitiveMonod[[eqn.ID]] <- sub.entry
    }
    else if (eqn.reaction.law == "logistic_competition") {
      # Check if single species mode
      single.species.mode <- isTruthy(input$CB_log_comp_single_species_edit)
      
      # Find reaction entries for X and Y
      x.reaction.ids <- strsplit(rv.SPECIES$species[[species.id.x]]$Reaction.ids, ", ")[[1]]
      x.id <- NA
      for (rid in x.reaction.ids) {
        if (rv.REACTIONS$reactions[[rid]]$Reaction.Law == "logistic_competition" && rv.REACTIONS$reactions[[rid]]$Species == species.x) {
          x.id <- rid
          break
        }
      }
      
      # Update reaction entry for X
      if (!is.na(x.id)) {
        rv.REACTIONS$reactions[[x.id]]$String.Rate.Law  <- rate.law.x
        rv.REACTIONS$reactions[[x.id]]$Pretty.Rate.Law  <- rate.law.x
        rv.REACTIONS$reactions[[x.id]]$Latex.Rate.Law   <- rate.law.x
        rv.REACTIONS$reactions[[x.id]]$MathJax.Rate.Law <- ConvertRateLaw(rate.law.x)$mathjax
        rv.REACTIONS$reactions[[x.id]]$Species          <- species.x
        rv.REACTIONS$reactions[[x.id]]$Species.id       <- species.id.x
        rv.REACTIONS$reactions[[x.id]]$Modifiers        <- if (single.species.mode) species.y else NA
        rv.REACTIONS$reactions[[x.id]]$Modifiers.id     <- if (single.species.mode) species.id.y else NA
        rv.REACTIONS$reactions[[x.id]]$Eqn.Display.Type <- eqn.display
        rv.REACTIONS$reactions[[x.id]]$Description      <- eqn.d
        rv.REACTIONS$reactions[[x.id]]$Equation.Text    <- if (single.species.mode) 
                                                             paste0("logistic competition (", species.x, " with ", species.y, " as competitor)") 
                                                           else 
                                                             paste0("logistic competition (", species.x, ",", species.y, ")")
        rv.REACTIONS$reactions[[x.id]]$Equation.MathJax <- mathjax.law
        rv.REACTIONS$reactions[[x.id]]$Show.In.Table    <- TRUE  # Main entry to show in table
      }
      
      if (!single.species.mode) {
        # Both species mode: find and update Y reaction entry
        y.reaction.ids <- strsplit(rv.SPECIES$species[[species.id.y]]$Reaction.ids, ", ")[[1]]
        y.id <- NA
        for (rid in y.reaction.ids) {
          if (rv.REACTIONS$reactions[[rid]]$Reaction.Law == "logistic_competition" && rv.REACTIONS$reactions[[rid]]$Species == species.y) {
            y.id <- rid
            break
          }
        }
        
        if (!is.na(y.id)) {
          rv.REACTIONS$reactions[[y.id]]$String.Rate.Law  <- rate.law.y
          rv.REACTIONS$reactions[[y.id]]$Pretty.Rate.Law  <- rate.law.y
          rv.REACTIONS$reactions[[y.id]]$Latex.Rate.Law   <- rate.law.y
          rv.REACTIONS$reactions[[y.id]]$MathJax.Rate.Law <- ConvertRateLaw(rate.law.y)$mathjax
          rv.REACTIONS$reactions[[y.id]]$Species          <- species.y
          rv.REACTIONS$reactions[[y.id]]$Species.id       <- species.id.y
          rv.REACTIONS$reactions[[y.id]]$Modifiers        <- NA
          rv.REACTIONS$reactions[[y.id]]$Modifiers.id     <- NA
          rv.REACTIONS$reactions[[y.id]]$Show.In.Table    <- FALSE  # Hide from table - internal only
        }
      }
      
      # Update logisticCompetition sub-entry
      r.x.id      <- par.ids[1]
      alpha.xy.id <- par.ids[2]
      Kc.id       <- par.ids[3]
      
      if (single.species.mode) {
        lc.entry <- list(
          "ID"           = eqn.ID,
          "Reaction.Law" = eqn.reaction.law,
          "Single.Species.Mode" = TRUE,
          "Species.X"    = species.x,
          "Species.X.id" = species.id.x,
          "Species.Y"    = species.y,
          "Species.Y.id" = species.id.y,
          "r.x"          = parameters[1],
          "r.x.id"       = r.x.id,
          "alpha.xy"     = parameters[2],
          "alpha.xy.id"  = alpha.xy.id,
          "Kc"           = parameters[3],
          "Kc.id"        = Kc.id
        )
      } else {
        r.y.id      <- par.ids[2]
        alpha.yx.id <- par.ids[4]
        lc.entry <- list(
          "ID"           = eqn.ID,
          "Reaction.Law" = eqn.reaction.law,
          "Single.Species.Mode" = FALSE,
          "Species.X"    = species.x,
          "Species.X.id" = species.id.x,
          "Species.Y"    = species.y,
          "Species.Y.id" = species.id.y,
          "r.x"          = parameters[1],
          "r.x.id"       = r.x.id,
          "r.y"          = parameters[2],
          "r.y.id"       = r.y.id,
          "alpha.xy"     = parameters[3],
          "alpha.xy.id"  = alpha.xy.id,
          "alpha.yx"     = parameters[4],
          "alpha.yx.id"  = alpha.yx.id,
          "Kc"           = parameters[5],
          "Kc.id"        = Kc.id
        )
      }
      rv.REACTIONS$logisticCompetition[[eqn.ID]] <- lc.entry
    }
    else if (eqn.reaction.law == "predator_prey") {
      # Find reaction entries for prey and predator
      species.x.id <- FindId(species.x)
      species.y.id <- FindId(species.y)
      
      x.reaction.ids <- strsplit(rv.SPECIES$species[[species.x.id]]$Reaction.ids, ", ")[[1]]
      y.reaction.ids <- strsplit(rv.SPECIES$species[[species.y.id]]$Reaction.ids, ", ")[[1]]
      
      x.id <- NA; y.id <- NA
      for (rid in x.reaction.ids) {
        if (rv.REACTIONS$reactions[[rid]]$Reaction.Law == "predator_prey" && rv.REACTIONS$reactions[[rid]]$Species.id == species.x.id) {
          x.id <- rid
          break
        }
      }
      for (rid in y.reaction.ids) {
        if (rv.REACTIONS$reactions[[rid]]$Reaction.Law == "predator_prey" && rv.REACTIONS$reactions[[rid]]$Species.id == species.y.id) {
          y.id <- rid
          break
        }
      }
      
      # Update reaction entry for prey (X)
      if (!is.na(x.id)) {
        rv.REACTIONS$reactions[[x.id]]$String.Rate.Law  <- rate.law.x
        rv.REACTIONS$reactions[[x.id]]$Pretty.Rate.Law  <- rate.law.x
        rv.REACTIONS$reactions[[x.id]]$Latex.Rate.Law   <- rate.law.x
        rv.REACTIONS$reactions[[x.id]]$MathJax.Rate.Law <- ConvertRateLaw(rate.law.x)$mathjax
        rv.REACTIONS$reactions[[x.id]]$Species          <- species.x
        rv.REACTIONS$reactions[[x.id]]$Species.id       <- species.id.x
        rv.REACTIONS$reactions[[x.id]]$Parameters       <- collapseVector(parameters)
        rv.REACTIONS$reactions[[x.id]]$Parameters.id     <- collapseVector(par.ids)
        rv.REACTIONS$reactions[[x.id]]$Description      <- eqn.d
        rv.REACTIONS$reactions[[x.id]]$Equation.Text    <- eqn.text
        rv.REACTIONS$reactions[[x.id]]$Equation.MathJax <- mathjax.law
      }
      
      # Update reaction entry for predator (Y)
      if (!is.na(y.id)) {
        rv.REACTIONS$reactions[[y.id]]$String.Rate.Law  <- rate.law.y
        rv.REACTIONS$reactions[[y.id]]$Pretty.Rate.Law  <- rate.law.y
        rv.REACTIONS$reactions[[y.id]]$Latex.Rate.Law   <- rate.law.y
        rv.REACTIONS$reactions[[y.id]]$MathJax.Rate.Law <- ConvertRateLaw(rate.law.y)$mathjax
        rv.REACTIONS$reactions[[y.id]]$Species          <- species.y
        rv.REACTIONS$reactions[[y.id]]$Species.id       <- species.id.y
        rv.REACTIONS$reactions[[y.id]]$Parameters       <- collapseVector(parameters)
        rv.REACTIONS$reactions[[y.id]]$Parameters.id     <- collapseVector(par.ids)
        rv.REACTIONS$reactions[[y.id]]$Description      <- eqn.d
      }
      
      # Update predatorPrey reactive value entry
      if (exists("par.ids") && length(par.ids) >= 4) {
        r.id <- par.ids[1]
        a.id <- par.ids[2]
        b.id <- par.ids[3]
        d.id <- par.ids[4]
      } else {
        r.id <- NA; a.id <- NA; b.id <- NA; d.id <- NA
      }
      
      pp.entry <- list(
        "ID"           = eqn.ID,
        "Reaction.Law" = eqn.reaction.law,
        "Prey"         = species.x,
        "Prey.id"      = species.id.x,
        "Predator"     = species.y,
        "Predator.id"  = species.id.y,
        "r"            = parameters[1],
        "r.id"         = r.id,
        "r.val"        = param.vals[1],
        "a"            = parameters[2],
        "a.id"         = a.id,
        "a.val"        = param.vals[2],
        "b"            = parameters[3],
        "b.id"         = b.id,
        "b.val"        = param.vals[3],
        "d"            = parameters[4],
        "d.id"         = d.id,
        "d.val"        = param.vals[4]
      )
      rv.REACTIONS$predatorPrey[[eqn.ID]] <- pp.entry
    }
    else if (eqn.reaction.law == "mass_action_w_reg") {
      
      pc <- 1
      # Determine with param ids are which
      if (!is.na(kf)) {
        kf.id <- par.ids[pc]
        pc <- pc + 1
      }
      
      if (!is.na(kr)) {
        kr.id <- par.ids[pc]
        pc <- pc + 1
      }
      
      if (has.f.reg) {
        n.f.reg <- length(strsplit(Forward.Pars, ", ")[[1]])
        Forward.Pars.id <- par.ids[pc:(pc+n.f.reg-1)]
        pc <- pc + n.f.reg
        Forward.Pars.id <- paste0(Forward.Pars.id, collapse = ", ")
      } else {
        Forward.Pars.id <- NA
      }
      
      if (has.r.reg) {
        n.r.reg <- length(strsplit(Reverse.Pars, ", ")[[1]])
        Reverse.Pars.id <- par.ids[pc:(pc+n.r.reg-1)]
        Reverse.Pars.id <- paste0(Reverse.Pars.id, collapse = ", ")
      } else {
        Reverse.Pars.id <- NA
      }
      
      sub.entry <- list(
        "ID" = eqn.ID,
        "Reaction.Law"    = input$eqnCreate_reaction_law,
        "r.stoichiometry" = r.stoich,
        "Reactants"       = reactants,
        "Reactants.id"    = reactants.id,
        "p.stoichiometry" = p.stoich,
        "Products"        = products,
        "Products.id"     = products.id,
        "Reversible"      = reversible,
        "kf"              = kf,
        "kr"              = kr,
        "kf.id"           = kf.id,
        "kr.id"           = kr.id,
        "Use.Forward.Mod" = has.f.reg,
        "Forward.Mods"    = Forward.Mods,
        "Forward.Mods.id" = Forward.Mods.id,
        "Forward.Pars"   = Forward.Pars,
        "Forward.Pars.id" = Forward.Pars.id,
        "Use.Reverse.Mod" = has.r.reg,
        "Reverse.Mods"    = Reverse.Mods,
        "Reverse.Mods.id" = Reverse.Mods.id,
        "Reverse.Pars"    = Reverse.Pars,
        "Reverse.Pars.id" = Reverse.Pars.id
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$massActionwReg)
      rv.REACTIONS$massActionwReg[[n+1]] <- sub.entry
      names(rv.REACTIONS$massActionwReg)[n+1] <- eqn.ID
    }
    else if (eqn.reaction.law == "synthesis") {
      sub.entry <- list(
        "ID"               = eqn.ID,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "VarSyn"           = var.syn,
        "VarSyn.id"        = var.syn.id,
        "Rate.Constant"    = parameter,
        "Rate.Constant.id" = par.ids[1],
        "Factor"           = factor,
        "Factor.id"        = factor.id
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$synthesis)
      rv.REACTIONS$synthesis[[n+1]] <- sub.entry
      names(rv.REACTIONS$synthesis)[n+1] <- eqn.ID
      
    }
    else if (eqn.reaction.law == "degradation_rate") {
      print("DEBUG: degradation_rate sub.entry creation block")
      print(paste("DEBUG: exists('par.ids') =", exists("par.ids")))
      if (exists("par.ids")) {
        print(paste("DEBUG: length(par.ids) =", length(par.ids)))
      }
      
      # Determine krel.param.id - it will be par.ids[2] if products exist AND relative formation is checked, otherwise NA
      krel.param.id <- NA
      if (input$CB_degradation_rate_toProducts_edit && isTruthy(input$CB_degradation_rate_relative_formation_edit) && exists("par.ids") && length(par.ids) >= 2) {
        krel.param.id <- par.ids[2]
      }
      
      # Check if par.ids exists before accessing it
      rate.constant.id <- NA
      if (exists("par.ids") && length(par.ids) >= 1) {
        rate.constant.id <- par.ids[1]
      }
      
      sub.entry <- list(
        "ID"               = eqn.ID,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "VarDeg"           = deg.species,
        "VarDeg.id"        = deg.species.id,
        "ConcDep"          = ConcDep,
        "Rate.Constant"    = parameter,
        "Rate.Constant.id" = rate.constant.id,
        "Products"         = products.collapsed,
        "Products.id"      = products.id.collapsed,
        "krel"             = krel.param,
        "krel.id"          = krel.param.id
      )
      
      # Update existing entry instead of adding new one
      rv.REACTIONS$degradation.by.rate[[eqn.ID]] <- sub.entry
    }
    else if (eqn.reaction.law == "degradation_by_enzyme") {
      print("DEBUG: degradation_by_enzyme sub.entry creation block")
      print(paste("DEBUG: exists('par.ids') =", exists("par.ids")))
      if (exists("par.ids")) {
        print(paste("DEBUG: length(par.ids) =", length(par.ids)))
        print(paste("DEBUG: par.ids =", paste(par.ids, collapse = ", ")))
      } else {
        print("DEBUG: ERROR - par.ids does not exist!")
      }
      
      # Gets ids based on use.Vmax
      # Check if par.ids exists and has elements before accessing it
      Vmax.id <- NA
      kcat.id <- NA
      Km.id   <- NA
      
      if (exists("par.ids") && length(par.ids) >= 1) {
        print("DEBUG: Accessing par.ids[1] for Km.id")
        Km.id <- par.ids[1]
        
        if (Use.Vmax) {
          if (length(par.ids) >= 2) {
            print("DEBUG: Accessing par.ids[2] for Vmax.id")
            Vmax.id <- par.ids[2]
          }
        } else {
          if (length(par.ids) >= 2) {
            print("DEBUG: Accessing par.ids[2] for kcat.id")
            kcat.id <- par.ids[2]
          }
        }
      } else {
        print("DEBUG: WARNING - par.ids not available or empty, setting IDs to NA")
      }
      
      # Determine krel.param.id - it will be the last parameter ID if krel exists
      # Check if krel was added: if we have 3 parameters (Km, Vmax/kcat, krel), then krel is the last one
      krel.param.id <- NA
      krel.param.value <- NA
      print(paste("DEBUG: Checking for krel - toProducts =", input$CB_degradation_enzyme_toProducts_edit))
      print(paste("DEBUG: Checking for krel - relative_formation =", isTruthy(input$CB_degradation_enzyme_relative_formation_edit)))
      if (input$CB_degradation_enzyme_toProducts_edit && isTruthy(input$CB_degradation_enzyme_relative_formation_edit)) {
        # krel.param was added to parameters, so it should be the last one
        # Check if par.ids exists and has enough elements
        print(paste("DEBUG: krel conditions met, checking par.ids - exists =", exists("par.ids")))
        if (exists("par.ids")) {
          print(paste("DEBUG: length(par.ids) =", length(par.ids)))
        }
        if (exists("par.ids") && length(par.ids) >= 3) {
          print("DEBUG: Accessing par.ids[length(par.ids)] for krel.param.id")
          krel.param.id <- par.ids[length(par.ids)]
          krel.param.value <- input$TI_degradation_enzyme_krel_edit
          print(paste("DEBUG: krel.param.id =", krel.param.id))
          print(paste("DEBUG: krel.param.value =", krel.param.value))
        } else {
          print("DEBUG: WARNING - par.ids not available or doesn't have 3+ elements for krel")
        }
      }
      
      sub.entry <- list(
        "ID"               = eqn.ID,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "VarDeg"           = deg.species,
        "VarDeg.id"        = deg.species.id,
        "UseVmax"          = Use.Vmax,
        "Km"               = Km,
        "Km.id"            = Km.id,
        "Vmax"             = Vmax,
        "Vmax.id"          = Vmax.id,
        "Enzyme"           = enzyme,
        "Enzyme.id"        = enzyme.id,
        "kcat"             = kcat,
        "kcat.id"          = kcat.id,
        "Products"         = products.collapsed,
        "Products.id"      = products.id.collapsed,
        "krel"             = krel.param.value,
        "krel.id"          = krel.param.id
      )
      
      # Update existing entry instead of adding new one
      rv.REACTIONS$degradation.by.enzyme[[eqn.ID]] <- sub.entry
    }
    else if (eqn.reaction.law == "michaelis_menten") {
      print("DEBUG: michaelis_menten sub.entry creation block")
      # Gets ids based on use.Vmax
      Vmax.id <- NA
      kcat.id <- NA
      Km.id   <- NA
      
      if (exists("par.ids") && length(par.ids) >= 1) {
        Km.id <- par.ids[1]
        
        if (Use.Vmax) {
          if (length(par.ids) >= 2) {
            Vmax.id <- par.ids[2]
          }
        } else {
          if (length(par.ids) >= 2) {
            kcat.id <- par.ids[2]
          }
        }
      }
      
      sub.entry <- list(
        "ID"               = eqn.ID,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "Substrate"        = substrate,
        "Substrate.id"     = substrate.id,
        "Product"          = products,
        "Product.id"       = products.id,
        "UseVmax"          = Use.Vmax,
        "Km"               = Km,
        "Km.id"            = Km.id,
        "Vmax"             = Vmax,
        "Vmax.id"          = Vmax.id,
        "Enzyme"           = enzyme,
        "Enzyme.id"        = enzyme.id,
        "kcat"             = kcat,
        "kcat.id"          = kcat.id
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$michaelisMenten)
      rv.REACTIONS$michaelisMenten[[n+1]] <- sub.entry
      names(rv.REACTIONS$michaelisMenten)[n+1] <- eqn.ID
    }
    
    if (exists("par.ids")) {
      print(paste("DEBUG: Final par.ids after all parameter processing - length =", length(par.ids)))
      if (length(par.ids) > 0) {
        print(paste("DEBUG: Final par.ids =", paste(par.ids, collapse = ", ")))
      }
    } else {
      print("DEBUG: ERROR - par.ids was never created! passed.error.check must have been FALSE")
    }
    
    # Resolve Diffeqs
    solveForDiffEqs()
    
  } else {
    print("DEBUG: ERROR - passed.error.check is FALSE, so par.ids was never created!")
    print("DEBUG: This means we should not be accessing par.ids anywhere!")
  }
  # # Remove Parameters if they were changed
  # params.to.remove <- setdiff(old.params, p.add)
  # 
  # # Check if old parameters are used elsewhere
  # p.remove <- c()
  # p.save <- c()
  # 
  # #if so, store in message of variables not removed
  # if (length(p.save) > 0) {
  #   message.out <- 
  #     paste0("The following parameter(s) were not deleted because they are used
  #            elsewhere: ",
  #            paste0(p.save, collapse=", ")
  #   )
  #   session$sendCustomMessage(type = 'testmessage',
  #                             message = message.out)
  # }

  #  JS UI functions
  w.test$hide()
  shinyjs::enable("createEqn_store_edit_button")
})



