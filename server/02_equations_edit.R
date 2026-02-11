# Edit Tab Controlling the editing of equations

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
                choices = sort(rv.SPECIES$df.by.compartment$Name),
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
                choices = sort(rv.SPECIES$df.by.compartment$Name),
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
          width = 4,
          conditionalPanel(
            condition = "input.CB_degradation_rate_toProducts_edit",
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
    
    prod.exists <- ifelse(is.na(Product), FALSE, TRUE)
    if (prod.exists) {
      num.prods <- length(strsplit(Product, ", ")[[1]])
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
          width = 3,
          offset = 1,
          conditionalPanel(
            condition = "input.CB_degradation_enzyme_toProducts_edit",
            lapply(
              seq(input$NI_degradation_enzyme_num_products_edit), function(i){
                pickerInput(
                  inputId = paste0("PI_degradation_enzyme_product_edit_", 
                                   as.character(i)),
                  label = paste0("Product ", as.character(i)),
                  choices = sort(rv.SPECIES$df.by.compartment$Name),
                  selected = Product[i],
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = "startsWith"))
              }
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
    
    # Store Rate Law
    laws <- Degradation_By_Rate(parameter, ConcDep, deg.species)
  }
  else if (eqn.reaction.law == "degradation_by_enzyme") {
    
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
  if (passed.error.check) {
    par.ids <- c()
    # Check to see if parameter names have changed (meaning new parameter)
    if (length(setdiff(old.params, parameters)) == 0) {
      # parameter names have not changed
      for (i in seq_along(parameters)) {
        par.id <- FindId(parameters[i])
        par.ids <- c(par.ids, par.id)
        rv.PARAMETERS$parameters[[par.id]]$Value <- as.numeric(param.vals[i])
        rv.PARAMETERS$parameters[[par.id]]$Unit <- param.units[i]
        rv.PARAMETERS$parameters[[par.id]]$UnitDescription <- 
          unit.descriptions[i]
        rv.PARAMETERS$parameters[[par.id]]$BaseUnit <- base.units[i]
        rv.PARAMETERS$parameters[[par.id]]$BaseValue <- 
          as.numeric(base.values[i])
        rv.PARAMETERS$parameters[[par.id]]$Description <- param.descriptions[i]
      }
    } else {
      # Parameter names have changed 
      params.to.add  <- setdiff(parameters, old.params)
      params.to.del  <- setdiff(old.params, parameters)
      same.params    <- intersect(old.params, parameters)
      
      # Edit same params
      for (i in seq_along(same.params)) {
        par.id <- FindId(same.params[i])
        par.ids <- c(par.ids, par.id)
        
        ids.used.in <- c(rv.PARAMETERS$parameters[[par.id]]$Used.In, 
                         eqn.ID)
        types <- c(rv.PARAMETERS$parameters[[par.id]]$Type, 
                   "Reaction")
        type.n <- c(rv.PARAMETERS$parameters[[par.id]]$Type.Note,
                    eqn.reaction.law)
        is.custom <- rv.PARAMETERS$parameters[[par.id]]$Custom
        
        
        # Write out to parameter
        to.par.list <- list("Name"            = same.params[i],
                            "ID"              = par.id,
                            "Value"           = as.numeric(param.vals[i]),
                            "Unit"            = param.units[i],
                            "UnitDescription" = unit.descriptions[i],
                            "BaseUnit"        = base.units[i],
                            "BaseValue"       = as.numeric(base.values[i]),
                            "Description"     = param.descriptions[i],
                            "Type"            = collapseVector(types),
                            "Type.Note"       = collapseVector(type.n),
                            "Used.In"         = collapseVector(ids.used.in),
                            "Custom"          = is.custom
        )
        
        # Append parameter entry
        rv.PARAMETERS$parameters[[par.id]] <- to.par.list
      }
      
      for (i in seq_along(params.to.add)) {
        
        # Check if completely new param
        if (params.to.add[i] %in% rv.PARAMETERS$parameters.names) {
          # Find parameter id
          par.id <- FindId(params.to.add[i])
          par.ids <- c(par.ids, par.id)
          
          ids.used.in <- c(rv.PARAMETERS$parameters[[par.id]]$Used.In, 
                           eqn.ID)
          types <- c(rv.PARAMETERS$parameters[[par.id]]$Type, 
                     "Reaction")
          type.n <- c(rv.PARAMETERS$parameters[[par.id]]$Type.Note,
                      eqn.reaction.law)
          is.custom <- rv.PARAMETERS$parameters[[par.id]]$Custom
          
          
          # Write out to parameter
          to.par.list <- list("Name"            = params.to.add[i],
                              "ID"              = par.id,
                              "Value"           = as.numeric(param.vals[i]),
                              "Unit"            = param.units[i],
                              "UnitDescription" = unit.descriptions[i],
                              "BaseUnit"        = base.units[i],
                              "BaseValue"       = as.numeric(base.values[i]),
                              "Description"     = param.descriptions[i],
                              "Type"            = collapseVector(types),
                              "Type.Note"       = collapseVector(type.n),
                              "Used.In"         = collapseVector(ids.used.in),
                              "Custom"          = is.custom
                              )
          
          # Append parameter entry
          rv.PARAMETERS$parameters[[par.id]] <- to.par.list
          
        } 
        else {
          par.gen <- GenerateId(rv.ID$id.param.seed, "parameter")
          rv.ID$id.param.seed <- par.gen$seed
          par.id <- par.gen$id
        
          par.ids <- c(par.ids, par.id)
          
          # Store ID to database
          idx.to.add <- nrow(rv.ID$id.df) + 1
          rv.ID$id.df[idx.to.add, ] <- c(par.id, params.to.add[i])
          
          # Write out to parameter
          to.par.list <- list("Name"            = params.to.add[i],
                              "ID"              = par.id,
                              "Value"           = as.numeric(param.vals[i]),
                              "Unit"            = param.units[i],
                              "UnitDescription" = unit.descriptions[i],
                              "BaseUnit"        = base.units[i],
                              "BaseValue"       = as.numeric(base.values[i]),
                              "Description"     = param.descriptions[i],
                              "Type"            = "Reaction",
                              "Type.Note"       = eqn.reaction.law,
                              "Used.In"         = eqn.ID,
                              "Custom"          = FALSE)
          
          rv.PARAMETERS$parameters[[par.id]] <- to.par.list
        }
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
    
    # Extract reaction laws 
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    content.ml  <- laws$content.ml
    
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
      "Reversible"       = isReversible
    )
    
    rv.REACTIONS$reactions[[eqn.ID]] <- reaction.entry
    
    # Build specific reaction type reactive variable
    if (eqn.reaction.law == "mass_action") {
      if (length(par.ids) == 1) {
        kf.id = par.ids[1]
        kr.id = NA
      } else {
        kf.id = par.ids[1]
        kr.id = par.ids[2]
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
      sub.entry <- list(
        "ID"               = eqn.ID,
        "Reaction.Law"     = input$eqnCreate_reaction_law,
        "VarDeg"           = deg.species,
        "VarDeg.id"        = deg.species.id,
        "ConcDep"          = ConcDep,
        "Rate.Constant"    = parameter,
        "Rate.Constant.id" = par.ids[1],
        "Products"         = products.collapsed,
        "Products.id"      = products.id.collapsed
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$degradation.by.rate)
      rv.REACTIONS$degradation.by.rate[[n+1]] <- sub.entry
      names(rv.REACTIONS$degradation.by.rate)[n+1] <- eqn.ID
    }
    else if (eqn.reaction.law == "degradation_by_enzyme") {
      # Gets ids based on use.Vmax
      Vmax.id <- NA
      kcat.id <- NA
      Km.id   <- par.ids[1]
      
      if (Use.Vmax) {
        Vmax.id <- par.ids[2]
      } else {
        kcat.id <- par.ids[2]
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
        "Products.id"      = products.id.collapsed
      )
      
      # Add to mass action RV
      n <- length(rv.REACTIONS$degradation.by.enzyme)
      rv.REACTIONS$degradation.by.enzyme[[n+1]] <- sub.entry
      names(rv.REACTIONS$degradation.by.enzyme)[n+1] <- eqn.ID
    }
    else if (eqn.reaction.law == "michaelis_menten") {
      # Gets ids based on use.Vmax
      Vmax.id <- NA
      kcat.id <- NA
      Km.id   <- par.ids[1]
      
      if (Use.Vmax) {
        Vmax.id <- par.ids[2]
      } else {
        kcat.id <- par.ids[2]
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
    
    # Resolve Diffeqs
    solveForDiffEqs()
    
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



