#-------------------------------------------------------------------------------

# Edit Tab Controlling the editing of equations

#-------------------------------------------------------------------------------

# #prints the type of equation
output$build_equation_edit <- renderUI({
  withMathJax(equationBuilder_edit_mathJax())
})
#----------------Left Box with edit options for the equation--------------------
output$eqnCreate_renderingUIcomponents <- renderUI({
  
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)]
  
  eqn.ID      <- eqn.row$ID
  eqn.type    <- eqn.row$EqnType
  eqn.law     <- eqn.row$Law
  eqn.species <- eqn.row$Species
  eqn.RC      <- eqn.row$RateConstants
  eqn.compart <- eqn.row$Compartment
  eqn.descrpt <- eqn.row$Description

  arrow_type  <- NA
  FR.bool     <- FALSE
  RR.bool     <- FALSE
  num.FRs     <- 1
  num.RRs     <- 1
  use.Vmax    <- FALSE
  prod.exists <- FALSE
  num.prods   <- 1
  
  # Unpack the different kind of laws to fill out proper information
  if (eqn.type == "chem_rxn") {
    # Find Row with matching ID and extract
    row        <- match(eqn.ID, eqns$eqn.chem[1:nrow(eqns$eqn.chem), 1])
    chemInfo   <- eqns$eqn.chem[row, 1:ncol(eqns$eqn.chem)]
    
    ID         <- chemInfo$ID[1]
    Law        <- chemInfo$Law[1]
    LHS.coef   <- str_split(chemInfo$LHS_coef[1], " ")[[1]]
    LHS.var    <- str_split(chemInfo$LHS_var[1],  " ")[[1]]
    RHS.coef   <- str_split(chemInfo$RHS_coef[1], " ")[[1]]
    RHS.var    <- str_split(chemInfo$RHS_var[1],  " ")[[1]] 
    arrow_type <- chemInfo$arrow_type[1]
    kf         <- chemInfo$kf[1]
    kr         <- chemInfo$kr[1]
    FR.bool    <- chemInfo$FM_bool[1] 
    FRs        <- str_split(chemInfo$FMs[1], " ")[[1]] 
    FR.RCs     <- str_split(chemInfo$FM_rateC[1], " ")[[1]] 
    RR.bool    <- chemInfo$RM_bool[1] 
    RRs        <- str_split(chemInfo$RMs[1], " ")[[1]] 
    RR.RCs     <- str_split(chemInfo$RM_rateC[1], " ")[[1]]
    
    num.FRs    <- length(FRs)
    num.RRs    <- length(RRs)
    
  } else if (eqn.type == "enzyme_rxn") {
    row        <- match(eqn.ID, eqns$eqn.enzyme[1:nrow(eqns$eqn.enzyme), 1])
    enz.info   <- eqns$eqn.enzyme[row, 1:ncol(eqns$eqn.enzyme)]
    
    ID        <- enz.info$ID[1]
    Law       <- enz.info$Law[1]
    substrate <- enz.info$Substrate[1]
    product   <- enz.info$Product[1]
    enzyme    <- enz.info$Enzyme[1]
    kcat      <- enz.info$kcat[1]
    Km        <- enz.info$Km[1]
    Vmax      <- enz.info$Vmax[1]
    use.Vmax  <- ifelse(is.na(Vmax), FALSE, TRUE)
    
  } else if (eqn.type == "syn") {
    row        <- match(eqn.ID, eqns$eqn.syn[1:nrow(eqns$eqn.syn), 1])
    synInfo    <- eqns$eqn.syn[row, 1:ncol(eqns$eqn.syn)]
    
    ID     <- synInfo$ID[1]
    Law    <- synInfo$Law[1]
    VarSyn <- synInfo$VarSyn[1]
    RC     <- synInfo$RC[1]
    Factor <- synInfo$Factor[1]
    
  } else if (eqn.type == "deg") {
    row        <- match(eqn.ID, eqns$eqn.deg[1:nrow(eqns$eqn.deg), 1])
    degInfo    <- eqns$eqn.deg[row, 1:ncol(eqns$eqn.deg)]
    
    ID        <- degInfo$ID[1]
    Law       <- degInfo$Law[1]
    VarDeg    <- degInfo$VarDeg[1]
    ConcDep   <- degInfo$ConcDep[1]
    RC        <- degInfo$RC[1]
    Km        <- degInfo$Km[1]
    Enz       <- degInfo$Enz[1]
    Vmax      <- degInfo$Vmax[1]
    Product   <- degInfo$Prods[1]
    use.Vmax  <- ifelse(is.na(Vmax), FALSE, TRUE)
    
    prod.exists <- ifelse(is.na(Product), FALSE, TRUE)
    if (prod.exists) {
      num.prods <- length(strsplit(Product, " ")[[1]])
    }
  }
  
  div(
    pickerInput(
      inputId = "eqnCreate_type_of_equation_edit",
      label = "Equation Type",
      choices = c("Chemical Reaction" = "chem_rxn",
                  "Enzyme Based Reaction" = "enzyme_rxn",
                  "Synthesis" = "syn",
                  "Degradation" = "deg",
                  "Custom Rate Parameter" = "rate_eqn",
                  "Time Dependent Equation" = "time_dependent"
      ),
      selected = eqn.type
    ),
    conditionalPanel(
      condition = "input.eqnCreate_type_of_equation_edit == 'chem_rxn'",
      pickerInput(
        inputId = "eqn_chem_law_edit",
        label = "Law",
        choices = c("Mass Action" = "MassAction",
                    "Regulated Mass Action" = "RegulatedMA"
        ),
        selected = Law
      ),
      pickerInput(
        inputId = "eqn_chem_forward_or_both_edit"
        ,label = "Reaction Direction"
        ,choices = c("Reversible" = "both_directions",
                     "Forward" = 'forward_only')
        ,choicesOpt = list(icon = c("glyphicon glyphicon-resize-horizontal",
                                    "glyphicon glyphicon-arrow-right"
                                   )
        ),
        selected = arrow_type
      ),
      conditionalPanel(
        condition = "input.eqn_chem_law_edit == 'RegulatedMA'",
        hr(),
        prettyCheckbox(
          inputId = "eqn_options_chem_modifier_forward_edit",
          label = "Add Forward Regulator(s)",
          value = FR.bool
        ),
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_forward_edit",
          numericInput(inputId = "eqn_options_chem_num_forward_regulators_edit"
                       ,label = "# of Forward Regulators"
                       ,value = num.FRs
                       ,min = 1
                       ,step = 1)
        ),
        conditionalPanel(
          condition = "input.eqn_chem_forward_or_both_edit == 'both_directions'",
          prettyCheckbox(
            inputId = "eqn_options_chem_modifier_reverse_edit"
            ,label = "Add Reverse Regulator(s)"
            ,value = RR.bool
          ),
          conditionalPanel(
            condition = "input.eqn_options_chem_modifier_reverse_edit",
            numericInput(inputId = "eqn_options_chem_num_reverse_regulators_edit"
                         ,label = "# of Reverse Regulators"
                         ,value = num.RRs
                         ,min = 1
                         ,step = 1)
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.eqnCreate_type_of_equation_edit == 'enzyme_rxn'",
      pickerInput(
        inputId = "eqn_enzyme_law_edit",
        label = "Law",
        choices = c("Michaelis Menten Kinetics" = "Michaelis Menten",
                    "Other" = "Other"),
        selected = Law
      ),
      hr(),
      prettyCheckbox(
        inputId = "eqn_options_enzyme_useVmax_edit"
        ,label = "Use Vmax"
        ,value = use.Vmax
      )
    ),
    conditionalPanel(
      condition = "input.eqnCreate_type_of_equation_edit == 'syn'",
      pickerInput(
        inputId = "eqn_syn_law_edit",
        label = "Law",
        choices = c("Rate" = "rate",
                    "By Factor" = "byFactor"),
        selected = Law
      )
    ),
    conditionalPanel(
      condition = "input.eqnCreate_type_of_equation_edit == 'deg'",
      pickerInput(
        inputId = "eqn_deg_law_edit",
        label = "Law",
        choices = c("Rate" = "rate",
                    "By Enzyme" = "byEnzyme"),
        selected = Law
      ),
      hr(),
      prettyCheckbox(
        inputId = "eqn_deg_to_products_edit",
        label = "Degrades to species",
        value = prod.exists
      ),
      conditionalPanel(
        condition = "input.eqn_deg_to_products_edit",
        numericInput(
          inputId = "eqn_deg_num_products_edit",
          label = "Number of Species",
          value = num.prods,
          min = 1,
          step = 1
        )
      ),
      conditionalPanel(
        condition = "input.eqn_deg_law_edit == 'byEnzyme'",
        hr(),
        prettyCheckbox(
          inputId = "eqn_deg_use_Vmax_edit",
          label = "Use Vmax",
          value = use.Vmax
        ),
      )
    )
  )
})

# ---------------Editing Equations RenderUI-------------------------------------
output$eqnCreate_equationBuilder_chem_edit <- renderUI({
  
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)]
  
  eqn.ID      <- eqn.row$ID
  eqn.type    <- eqn.row$EqnType
  eqn.law     <- eqn.row$Law
  eqn.species <- eqn.row$Species
  eqn.RC      <- eqn.row$RateConstants
  eqn.compart <- eqn.row$Compartment
  eqn.descrpt <- eqn.row$Description
  
  row        <- match(eqn.ID, eqns$eqn.chem[1:nrow(eqns$eqn.chem), 1])
  chemInfo   <- eqns$eqn.chem[row, 1:ncol(eqns$eqn.chem)]
  
  ID         <- chemInfo$ID[1]
  Law        <- chemInfo$Law[1]
  LHS.coef   <- str_split(chemInfo$LHS_coef[1], " ")[[1]]
  LHS.var    <- str_split(chemInfo$LHS_var[1],  " ")[[1]]
  RHS.coef   <- str_split(chemInfo$RHS_coef[1], " ")[[1]]
  RHS.var    <- str_split(chemInfo$RHS_var[1],  " ")[[1]] 
  arrow_type <- chemInfo$arrow_type[1]
  kf         <- chemInfo$kf[1]
  kr         <- chemInfo$kr[1]
  FR.bool    <- chemInfo$FM_bool[1] 
  FRs        <- str_split(chemInfo$FMs[1], " ")[[1]] 
  FR.RCs     <- str_split(chemInfo$FM_rateC[1], " ")[[1]] 
  RR.bool    <- chemInfo$RM_bool[1] 
  RRs        <- str_split(chemInfo$RMs[1], " ")[[1]] 
  RR.RCs     <- str_split(chemInfo$RM_rateC[1], " ")[[1]]
  
  num.LHS    <- length(LHS.coef)
  num.RHS    <- length(RHS.coef)
  num.FRs    <- length(FRs)
  num.RRs    <- length(RRs)
  
  div(
    fluidRow(
      column(
        width = 3, 
        numericInput(inputId = "eqnCreate_num_of_eqn_LHS_edit",
                     label = "Number of Reactants",
                     value = num.LHS,
                     min = 1,
                     step = 1)
      ),
      column(
        width = 3,
        numericInput(inputId = "eqnCreate_num_of_eqn_RHS_edit",
                     label = "Number of Products",
                     value = num.RHS,
                     min = 1,
                     step = 1)
      )
    ),
    hr(),
    fluidRow(
      column(
        style = "border-right: 1px solid #e5e5e5; padding-right:20px",
        width = 4,
        lapply(seq(input$eqnCreate_num_of_eqn_LHS_edit), function(i){
          div(
            HTML(paste0("<b>Reactant ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("LHS_Coeff_edit", as.character(i)),
                label = NULL,
                value = as.numeric(LHS.coef[i]),
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("LHS_Var_edit", as.character(i)),
                label = NULL,
                choices = sort(vars$species),
                selected = LHS.var[i],
                options = pickerOptions(liveSearch = TRUE
                                         ,liveSearchStyle = "startsWith"
                                         ,dropupAuto = FALSE)
                )
              ,cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "border-right: 1px solid #e5e5e5; 
               padding-right: 20px; 
               padding-left: 20px;",
        width = 4,
        lapply(seq(input$eqnCreate_num_of_eqn_RHS_edit), function(i){
          div(
            HTML(paste0("<b>Product ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("RHS_Coeff_edit", as.character(i)),
                label = NULL,
                value = as.numeric(RHS.coef[i]),
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("RHS_Var_edit", as.character(i)),
                label = NULL,
                choices = sort(vars$species),
                selected = RHS.var[i],
                options = pickerOptions(liveSearch = TRUE
                                         ,liveSearchStyle = "startsWith"
                                         ,dropupAuto = FALSE)
                )
              ,cellWidths = c("25%", "75%")
            )
          )
        })
      ), #end Column
      column(
        style = "padding-left: 20px;",
        width = 4,
        conditionalPanel(
          condition = "!input.eqn_options_chem_modifier_forward_edit",
          textInput(
            inputId = "eqn_chem_forward_k_edit",
            label = "Forward Rate Constant",
            value = kf
          ),
          tags$head(tags$style("#eqn_chem_forward_k_edit {margin-top: -7px;}")),
        ),
        conditionalPanel(
          condition = "input.eqn_chem_forward_or_both_edit == 'both_directions' && 
                       !input.eqn_options_chem_modifier_reverse_edit",
          textInput(
            inputId = "eqn_chem_back_k_edit",
            label = "Reverse Rate Constant",
            value = kr
          )
        )
      )#end column
    ), #end fluidRow`
    conditionalPanel(
      condition = "input.eqn_options_chem_modifier_forward_edit || 
                   input.eqn_options_chem_modifier_reverse_edit",
      hr()
    ),
    fluidRow(
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_forward_edit",
            lapply(seq(input$eqn_options_chem_num_forward_regulators_edit), function(i){
              pickerInput(
                inputId = paste0("eqn_forward_regulator_edit", as.character(i)),
                label = paste0("Forward Regulator ", as.character(i)),
                choices = sort(vars$species),
                selected = FRs[i],
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = "startsWith"))
            })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_forward_edit",
          lapply(seq(input$eqn_options_chem_num_forward_regulators_edit), function(i){
            textInput(
              inputId = paste0("eqn_forward_rateConstant_edit", as.character(i)),
              label = paste0("Rate Constant ", as.character(i)),
              value = FR.RCs[i]
            )
          })
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_reverse_edit",
          lapply(seq(input$eqn_options_chem_num_reverse_regulators_edit), function(i){
            pickerInput(
              inputId = paste0("eqn_reverse_regulator_edit", as.character(i)),
              label = paste0("Reverse Regulator ", as.character(i)),
              choices = sort(vars$species),
              selected = RRs[i],
              options = pickerOptions(liveSearch = TRUE
                                       ,liveSearchStyle = "startsWith")
              )
          })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_reverse_edit",
          lapply(seq(input$eqn_options_chem_num_reverse_regulators_edit), function(i){
            textInput(
              inputId = paste0("eqn_reverse_rateConstant_edit", as.character(i)),
              label = "Rate Constant",
              value = RR.RCs[i]
              )
          })
        )
      )
    )
  )
})

output$eqnCreate_equationBuilder_enzyme_edit <- renderUI({
  
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)]
  
  eqn.ID      <- eqn.row$ID
  eqn.type    <- eqn.row$EqnType
  eqn.law     <- eqn.row$Law
  eqn.species <- eqn.row$Species
  eqn.RC      <- eqn.row$RateConstants
  eqn.compart <- eqn.row$Compartment
  eqn.descrpt <- eqn.row$Description
  
  row        <- match(eqn.ID, eqns$eqn.enzyme[1:nrow(eqns$eqn.enzyme), 1])
  enz.info   <- eqns$eqn.enzyme[row, 1:ncol(eqns$eqn.enzyme)]
  
  ID        <- enz.info$ID[1]
  Law       <- enz.info$Law[1]
  Substrate <- enz.info$Substrate[1]
  Product   <- enz.info$Product[1]
  Enzyme    <- enz.info$Enzyme[1]
  kcat      <- enz.info$kcat[1]
  Km        <- enz.info$Km[1]
  Vmax      <- enz.info$Vmax[1]
  use.Vmax  <- ifelse(is.na(Vmax), FALSE, TRUE)
  
  div(
    conditionalPanel(
      condition = "input.eqn_enzyme_law_edit == 'Michaelis Menten'",
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "eqn_enzyme_substrate_edit",
            label = "Substrate",
            choices = sort(vars$species),
            selected = Substrate,
            options = pickerOptions(
              liveSearch = TRUE,
              liveSearchStyle = "startsWith",
              dropupAuto = FALSE
            )
          ),
          conditionalPanel(
            condition = "!input.eqn_options_enzyme_useVmax_edit",
            pickerInput(
              inputId = "eqn_enzyme_enzyme_edit",
              label = "Enzyme",
              choices = sort(vars$species),
              selected = Enzyme,
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith")
            )
          )
        ),
        column(
          width = 3,
          offset = 1,
          conditionalPanel(
            condition = "input.eqn_options_enzyme_useVmax_edit",
            textInput(
              inputId = "eqn_enzyme_Vmax_edit",
              label = "Vmax",
              value = Vmax
            )
          ),
          conditionalPanel(
            condition = "!input.eqn_options_enzyme_useVmax_edit",
            textInput(
              inputId = "eqn_enzyme_kcat_edit",
              label = "kcat",
              value = kcat
            )
          ),
          textInput(
            inputId = "eqn_enzyme_Km_edit",
            label = "Km",
            value = Km
          )
        ),
        column(
          width = 3,
          offset = 1,
          pickerInput(
            inputId = "eqn_enzyme_product_edit",
            label = "Product",
            choices = sort(vars$species),
            selected = Product,
            options = pickerOptions(
              liveSearch = TRUE,
              liveSearchStyle = "startsWith",
              dropupAuto = FALSE
            )
          )
        )
      )#end fluidRow
    ),
    conditionalPanel(
      condition = "input.eqn_enzyme_law_edit == 'Other'",
      "Other enzyme laws will be added in these tabs in the future"
    )
  )#end div
})

output$eqnCreate_equationBuilder_synthesis_edit <- renderUI({
  
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)]
  
  eqn.ID      <- eqn.row$ID
  eqn.type    <- eqn.row$EqnType
  eqn.law     <- eqn.row$Law
  eqn.species <- eqn.row$Species
  eqn.RC      <- eqn.row$RateConstants
  eqn.compart <- eqn.row$Compartment
  eqn.descrpt <- eqn.row$Description
  
  row        <- match(eqn.ID, eqns$eqn.syn[1:nrow(eqns$eqn.syn), 1])
  synInfo    <- eqns$eqn.syn[row, 1:ncol(eqns$eqn.syn)]
  
  ID     <- synInfo$ID[1]
  Law    <- synInfo$Law[1]
  VarSyn <- synInfo$VarSyn[1]
  RC     <- synInfo$RC[1]
  Factor <- synInfo$Factor[1]
  
  div(
    fluidRow(
      column(
        width = 4,
        conditionalPanel(
          condition = "input.eqn_syn_law_edit == 'rate'",
          pickerInput(
            inputId  = "eqn_syn_rate_var_edit",
            label    = "Species to synthesize",
            choices  = sort(vars$species),
            selected = VarSyn,
            options  = pickerOptions(liveSearch = TRUE
                                    ,liveSearchStyle = "startsWith") 
          ),
          textInput(
            inputId = "eqn_syn_rate_RC_edit",
            label   = "Rate Constant",
            value   = RC
          )
        ),
        conditionalPanel(
          condition = "input.eqn_syn_law_edit == 'byFactor'",
          pickerInput(
            inputId  = "eqn_syn_sby_var_edit",
            label    = "Species to synthesize",
            choices  = sort(vars$species),
            selected = VarSyn,
            options  = pickerOptions(liveSearch = TRUE
                                    ,liveSearchStyle = "startsWith") 
          ),
          pickerInput(
            inputId  = "eqn_syn_sby_factor_edit",
            label    = "Factor causing synthesis",
            choices  = sort(vars$species),
            selected = Factor
          ),
          textInput(
            inputId = "eqn_syn_sby_RC_edit",
            label = "Rate Constant",
            value = RC
          )
        )
      )
    )
  )
})

output$eqnCreate_equationBuilder_degradation_edit <- renderUI({
  
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)]
  
  eqn.ID      <- eqn.row$ID
  eqn.type    <- eqn.row$EqnType
  eqn.law     <- eqn.row$Law
  eqn.species <- eqn.row$Species
  eqn.RC      <- eqn.row$RateConstants
  eqn.compart <- eqn.row$Compartment
  eqn.descrpt <- eqn.row$Description
  
  
  row        <- match(eqn.ID, eqns$eqn.deg[1:nrow(eqns$eqn.deg), 1])
  degInfo    <- eqns$eqn.deg[row, 1:ncol(eqns$eqn.deg)]
  
  ID        <- degInfo$ID[1]
  Law       <- degInfo$Law[1]
  VarDeg    <- degInfo$VarDeg[1]
  ConcDep   <- degInfo$ConcDep[1]
  RC        <- degInfo$RC[1]
  Km        <- degInfo$Km[1]
  Enz       <- degInfo$Enz[1]
  Vmax      <- degInfo$Vmax[1]
  Product   <- str_split(degInfo$Prods[1], " ")[[1]]
  use.Vmax  <- ifelse(is.na(Vmax), FALSE, TRUE)
  
  prod.exists <- ifelse(is.na(Product), FALSE, TRUE)
  if (prod.exists) {
    num.prods <- length(strsplit(Product, " ")[[1]])
  }
  
  div(
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId  = "eqn_deg_var_edit",
          label    = "Species to degrade",
          choices  = sort(vars$species),
          selected = VarDeg,
          options  = pickerOptions(liveSearch = TRUE
                                  ,liveSearchStyle = "startsWith") 
        )
      ),
      column(
        width = 4,
        conditionalPanel(
          condition = "input.eqn_deg_to_products_edit",
          lapply(seq(input$eqn_deg_num_products_edit), function(i){
            pickerInput(
              inputId  = paste0("eqn_deg_product_edit", as.character(i)),
              label    = paste0("Product ", as.character(i)),
              choices  = sort(vars$species),
              selected = Product[i],
              options  = pickerOptions(liveSearch = TRUE
                                       ,liveSearchStyle = "startsWith")
              )
          })
        )
      )
    ),
    hr(),
    conditionalPanel(
      condition = "input.eqn_deg_law_edit == 'rate'",
      fluidRow(
        column(
          width = 8,
          splitLayout(
            textInput(
              inputId = "eqn_deg_rate_RC_edit",
              label   = "Rate Constant",
              value   = RC
            ),
            div(
              style = "padding-top:38px; padding-left:15px;",
              checkboxInput(
                inputId = "eqn_deg_rate_conc_dependent_edit",
                label = "Concentration Dependent",
                value = ConcDep
                )
            )
          )
        )  
      )
    ),
    conditionalPanel(
      condition = "input.eqn_deg_law_edit == 'byEnzyme'",
      conditionalPanel(
        condition = "!input.eqn_deg_use_Vmax_edit",
        fluidRow(
          column(
            width = 4,
            pickerInput(
              inputId  = "eqn_deg_enzyme_edit",
              label    = "Enzyme",
              choices  = sort(vars$species),
              selected = Enz
            )
          ),
          column(
            width = 4,
            textInput(
              inputId = "eqn_deg_kcat_edit",
              label   = "kcat",
              value   = RC
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.eqn_deg_use_Vmax_edit",
        fluidRow(
          column(
            width = 4,
            textInput(
              inputId = "eqn_deg_Vmax_edit",
              label   = "Vmax",
              value   = Vmax
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          textInput(
            inputId = "eqn_deg_Km_edit",
            label   = "Km",
            value   = Km
          )
        )
      )
    )
  )
})

equationBuilder_edit <- reactive({
  if (input$eqnCreate_type_of_equation_edit == "chem_rxn") {
    n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit)
    n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit)
    n.f.reg = as.numeric(input$eqn_options_chem_num_forward_regulators_edit)
    n.r.reg = as.numeric(input$eqn_options_chem_num_reverse_regulators_edit)
    
    eqn_LHS <- ""
    for (i in seq(n.LHS)) {
      coef <- eval(parse(text = paste0("input$LHS_Coeff_edit", as.character(i))))
      var <- eval(parse(text = paste0("input$LHS_Var_edit", as.character(i))))
      if (coef != "1") {eqn_LHS <- paste0(eqn_LHS, coef, "*")}
      if (i == as.numeric(n.LHS)) {eqn_LHS <- paste0(eqn_LHS, var)}
      else{eqn_LHS <- paste0(eqn_LHS, var, " + ")}
    }
    
    eqn_RHS <- ""
    for (i in seq(n.RHS)) {
      coef <- eval(parse(text = paste0("input$RHS_Coeff_edit", as.character(i))))
      var <- eval(parse(text = paste0("input$RHS_Var_edit", as.character(i))))
      if (coef != "1") {eqn_RHS <- paste0(eqn_RHS, coef, "*")}
      if (i == as.numeric(n.RHS)) {eqn_RHS <- paste0(eqn_RHS, var)}
      else{eqn_RHS <- paste0(eqn_RHS, var, " + ")}
    }
    
    if (input$eqn_chem_forward_or_both_edit == "both_directions") {
      arrow <- "<-->"
      if (input$eqn_options_chem_modifier_forward_edit && 
          input$eqn_options_chem_modifier_reverse_edit) {
        #find regulators and add them together in form ([regulator/constant, regulator2/constant2, etc...])
        forwardModifiers <- c()
        for (i in seq(n.f.reg)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_edit", 
                                                as.character(i))
                                  )
                            )
          rateConstant <- eval(
                           parse(
                             text = paste0("input$eqn_forward_rateConstant_edit", 
                                           as.character(i)
                                           )
                             )
                           )
          modifierExpression <- paste0(regulator, "/", rateConstant)
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")
        
        reverseModifiers <- c()
        for (i in seq(n.r.reg)) {
          regulator <- eval(parse(text = paste0("input$eqn_reverse_regulator_edit",
                                                as.character(i)
                                                )
                                  )
                            )
          rateConstant <- eval(parse(text = paste0("input$eqn_reverse_rateConstant_edit", 
                                                   as.character(i))))
          modifierExpression <- paste0(regulator, "/", rateConstant)
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ",")
        
        arrow <- paste0("([", reverseModifiers, "])", arrow, "([",forwardModifiers ,"])")
      }
      else if (input$eqn_options_chem_modifier_forward_edit && 
               !input$eqn_options_chem_modifier_reverse_edit) {
        forwardModifiers <- c()
        for (i in seq(n.f.reg)) {
          regulator <- eval(parse(text = paste0("input$eqn_forward_regulator_edit", 
                                                as.character(i)
                                                )
                                  )
                            )
          rateConstant <- eval(parse(text = paste0("input$eqn_forward_rateConstant_edit", 
                                                   as.character(i)
                                                   )
                                     )
                               )
          modifierExpression <- paste0(regulator, "/", rateConstant)
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")
        
        arrow <- paste0("(", 
                        input$eqn_chem_back_k_edit, 
                        ")", 
                        arrow, 
                        "([",
                        forwardModifiers,
                        "])")
      }
      else if (!input$eqn_options_chem_modifier_forward_edit && 
               input$eqn_options_chem_modifier_reverse_edit) {
        reverseModifiers <- c()
        for (i in seq(n.r.reg)) {
          regulator <- eval(
                        parse(
                          text = paste0("input$eqn_reverse_regulator_edit", 
                                                as.character(i))))
          rateConstant <- eval(
                           parse(
                             text = paste0("input$eqn_reverse_rateConstant_edit", 
                                                   as.character(i))))
          
          modifierExpression <- paste0(regulator, "/", rateConstant)
          reverseModifiers <- c(reverseModifiers, modifierExpression)
        }
        reverseModifiers <- paste(reverseModifiers, collapse = ",")
        arrow <- paste0("([", 
                        reverseModifiers, 
                        "])", 
                        arrow, 
                        "(", 
                        input$eqn_chem_forward_k_edit, 
                        ")")
      }
      else
      {
        arrow <- paste0("(", 
                        input$eqn_chem_back_k_edit, 
                        ")", 
                        arrow, 
                        "(", 
                        input$eqn_chem_forward_k_edit, 
                        ")"
                        )
      }
    }
    else if (input$eqn_chem_forward_or_both_edit == "forward_only") {
      arrow = "--->"
      if (input$eqn_options_chem_modifier_forward_edit) {
        forwardModifiers <- c()
        for (i in seq(n.f.reg)) {
          regulator <- eval(
                        parse(
                          text = paste0("input$eqn_forward_regulator_edit",
                                                as.character(i))))
          rateConstant <- eval(
                           parse(
                             text = paste0("input$eqn_forward_rateConstant_edit",
                                                   as.character(i))))
          modifierExpression <- paste0(regulator, "/", rateConstant)
          forwardModifiers <- c(forwardModifiers, modifierExpression)
        }
        forwardModifiers <- paste(forwardModifiers, collapse = ",")
        arrow <- paste0(arrow, "([",forwardModifiers ,"])")
      }
      else
      {
        arrow <- paste0(arrow, 
                        "(", 
                        input$eqn_chem_forward_k_edit, 
                        ")"
                        )
      }
    }
    
    textOut <- paste(eqn_LHS, arrow, eqn_RHS)
  }
  else if (input$eqnCreate_type_of_equation_edit == "enzyme_rxn") {
    substrate = input$eqn_enzyme_substrate_edit
    product = input$eqn_enzyme_product_edit
    arrow = "-->"
    enzyme = input$eqn_enzyme_enzyme_edit
    Km = input$eqn_enzyme_Km_edit
    
    if (!input$eqn_options_enzyme_useVmax_edit) {
      kcat = input$eqn_enzyme_kcat_edit
      textOut <- paste0(substrate,
                        " + ", 
                        enzyme,  
                        " (", 
                        kcat, 
                        ")", 
                        arrow, 
                        "(", 
                        Km, 
                        ") ", 
                        product
                        )
    }
    else if (input$eqn_options_enzyme_useVmax_edit) {
      Vmax = input$eqn_enzyme_Vmax_edit
      textOut <- paste0(substrate,
                        " (",
                        Vmax,
                        ", Enzyme)",
                        arrow,
                        "(",
                        Km,
                        ") ",
                        product)
    }
  }
  else if (input$eqnCreate_type_of_equation_edit == "syn") {
    if (input$eqn_syn_law_edit == "rate") {
      arrow <- "-->"
      var   <- input$eqn_syn_rate_var_edit
      rc    <- input$eqn_syn_rate_RC_edit
      type  <- "syn"
      textOut <- paste0(arrow,
                        "(", rc, ")",
                        var
      )
    } 
    else if (input$eqn_syn_law_edit == "byFactor") {
      arrow  <- "-->"
      var    <- input$eqn_syn_sby_var_edit
      rc     <- input$eqn_syn_sby_RC_edit
      factor <- input$eqn_syn_sby_factor_edit
      type   <- "syn"
      textOut <- paste0(factor,
                        arrow,
                        "(", rc, ")",
                        var
      )
    }
  }
  else if (input$eqnCreate_type_of_equation_edit == "deg") {
    if (input$eqn_deg_to_products_edit) {
      num.deg.products <- as.numeric(input$eqn_deg_num_products_edit)
      product <- ""
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$eqn_deg_product_edit", 
                                         as.character(i))))
        if (i == num.deg.products) {
          product <- paste0(product, Var2MathJ(prod))
        } else {
          product <- paste0(product, Var2MathJ(prod), " + ")
        }
      }
    } else {
      product <- ""
    }
    if (input$eqn_deg_law_edit == "rate") {
      arrow <- "->"
      var   <- input$eqn_deg_var_edit
      rc    <- input$eqn_deg_rate_RC_edit
      type  <- "deg"
      textOut <- paste0(var,
                        arrow,
                        "(", rc, ")",
                        product
      )
      
    } else if (input$eqn_deg_law_edit == "byEnzyme") {
      arrow <- "->"
      var   <- input$eqn_deg_var
      Km    <- input$eqn_deg_Km
      type  <- "deg"
      
      if (input$eqn_deg_use_Vmax_edit) {
        Vmax <- input$eqn_deg_Vmax_edit
        textOut <- paste0(var,
                          arrow,
                          "(", Km, ", ", Vmax, ")",
                          product
        )
      } else {
        enz  <- input$eqn_deg_enzyme_edit
        kcat <- input$eqn_deg_kcat_edit
        textOut <- paste0(var,
                          arrow,
                          "(", Km, ", ", kcat, ", ", enz, ")",
                          product
        )
      }
    }
  }

  # else if (input$eqnCreate_type_of_equation_edit == "rate_eqn") {
  #   rate_left <- input$eqnCreate_rate_firstvar
  #   rate_right <- input$eqnCreate_rate_equation
  #   textOut <- paste0(rate_left, " = ", rate_right)
  # }
  # else if (input$eqnCreate_type_of_equation_edit == "time_dependent")
  # {
  #   TD_left <- input$eqnCreate_time_dependent_firstvar
  #   TD_right <- input$eqnCreate_time_dependent_equation
  #   textOut <- paste0(TD_left, "=", TD_right)
  # }
  else{textOut <- "ERROR"}
  return(textOut)
})

#-------------------------------------------------------------------------------

# Edit Tab rewriting of Equations from Equation UI

#-------------------------------------------------------------------------------

observeEvent(input$createEqn_store_edit_button, {
  waiter.eqns$show()
  shinyjs::disable("createEqn_store_edit_button")
  eqn.num     <- as.numeric(input$eqnCreate_edit_select_equation)
  eqn.row     <- eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)]
  old.params  <- str_split(eqns$eqn.info$RateConstants[eqn.num], " ")[[1]]
  
  #extract relevant equation information
  eqn.type <- input$eqnCreate_type_of_equation_edit
  if (eqn.type == "chem_rxn") {
    law <- input$eqn_chem_law_edit
  } else if (eqn.type == "enzyme_rxn") {
    law <- input$eqn_enzyme_law_edit
  } else if (eqn.type == "syn") {
    law <- input$eqn_syn_law_edit
  } else if (eqn.type == "deg") {
    law <- input$eqn_deg_law_edit
  } else if (eqn.type == "rate_eqn") {
    law <- NA
  } else if (eqn.type == "time_dependent") {
    law <- NA
  }
  
  p.add              <- c() # Parameter Variable Vector
  d.add              <- c() # Parameter Description Vector
  passed.error.check <- TRUE
  var.add            <- c() # Variables in model to add
  
  # Build new equations
  if (eqn.type == "chem_rxn") {
    #this will hold all the functions for chemical reactions:
    # Currently holds: Mass Action, Regulated Mass Action
    jPrint("chem_rxn")
    compartment = 1 #placeholder for compartments to be added in future
    
    n.RHS = as.numeric(input$eqnCreate_num_of_eqn_RHS_edit) #number of variables on RHS of equation
    n.LHS = as.numeric(input$eqnCreate_num_of_eqn_LHS_edit) #number of variables on LHS of equation
    
    if (input$eqn_chem_law_edit == "MassAction") { # Mass Action
      jPrint("Mass Action")
      law = "MassAction"
      # Set regulators to null
      FM.bool <- FALSE
      FMs     <- NA
      FM.RC   <- NA
      RM.bool <- FALSE
      RMs     <- NA
      RM.RC   <- NA
      # Build left hand side of equation
      left     <- BuildEquationSide("input$LHS_Coeff_edit", 
                                    "input$LHS_Var_edit", 
                                    n.LHS)
      coef.LHS <- left["coefs"]
      var.LHS  <- left["vars"]
      
      # Build right hand side equation
      right    <- BuildEquationSide("input$RHS_Coeff_edit",
                                    "input$RHS_Var_edit", 
                                    n.RHS)
      coef.RHS <- right["coefs"]
      var.RHS  <- right["vars"]
      
      arrow <- input$eqn_chem_forward_or_both_edit
      if (arrow == "both_directions") {
        jPrint("both directions")
        # Rate Constants
        kf    <- input$eqn_chem_forward_k_edit
        kr    <- input$eqn_chem_back_k_edit
        p.add <- c(p.add, kf, kr)
        
        kf.d <- paste0("Forward rate constant for the reaction of ",
                       paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                       " to ",
                       paste0(str_split(var.RHS, " ")[[1]], collapse = ", "))
        kr.d <- paste0("Reverse rate constant for the reaction of ",
                       paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                       " to ",
                       paste0(str_split(var.RHS, " ")[[1]], collapse = ", ")
        )
        d.add <- c(d.add, kf.d, kr.d)
        
      } else if (arrow == "forward_only") {
        kf    <- input$eqn_chem_forward_k_edit
        kr    <- NA
        p.add <- c(p.add, kf)
        
        kf.d <- paste0("Forward rate constant for the reaction of ",
                       paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                       " to ",
                       paste0(str_split(var.RHS, " ")[[1]], collapse = ", "))
        d.add <- c(d.add, kf.d)
      }
      eqn.description <- ""
      var.add <- paste(var.LHS, var.RHS)
      
    } else if (input$eqn_chem_law_edit == 'RegulatedMA') { # Mass Action w/ Regulation
      law = "RegulatedMA"
      n.f.reg = as.numeric(input$eqn_options_chem_num_forward_regulators_edit) #number of regulators for forward reaction
      n.r.reg = as.numeric(input$eqn_options_chem_num_reverse_regulators_edit) #number of regulators for reverse reaction
      
      # Build left hand side of equation
      left     <- BuildEquationSide("input$LHS_Coeff_edit",
                                    "input$LHS_Var_edit",
                                    n.LHS)
      coef.LHS <- left["coefs"]
      var.LHS  <- left["vars"]
      
      # Build right hand side equation
      right    <- BuildEquationSide("input$RHS_Coeff_edit",
                                    "input$RHS_Var_edit",
                                    n.RHS)
      coef.RHS <- right["coefs"]
      var.RHS  <- right["vars"]
      
      arrow <- input$eqn_chem_forward_or_both_edit
      if (arrow == "both_directions") {
        if (input$eqn_options_chem_modifier_forward_edit) {
          kf      <- NA
          FM.bool <- TRUE
          
          f.regs <- BuildRegulatorSide("input$eqn_forward_regulator_edit", 
                                       "input$eqn_forward_rateConstant_edit", 
                                       n.f.reg,
                                       var.LHS,
                                       var.RHS,
                                       TRUE)
          FMs     <- f.regs["regulators"]
          FM.RC   <- f.regs["rateConstants"]
          p.add   <- c(p.add, f.regs["P.to.add"][[1]])
          jPrint("before extracting descriptions")
          d.add   <- c(d.add, f.regs["P.descriptions"][[1]])
          jPrint("After extracting descriptions")
          jPrint(d.add)
          FMs     <- paste(FMs, collapse = " ")
          FM.RC   <- paste(FM.RC, collapse = " ")
        } else {
          kf      <- input$eqn_chem_forward_k_edit
          p.add   <- c(p.add, kf)
          FM.bool <- FALSE
          FMs     <- NA
          FM.RC   <- NA
          
          kf.d <- paste0("Reverse rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", ")
          )
          d.add <- c(d.add, kf.d)
        }
        # Checks if regulator was used in reverse reaction, hence removing kr 
        # and updating the appropriate values for the regulator 
        if (input$eqn_options_chem_modifier_reverse_edit) {
          kr      <- NA
          RM.bool <- TRUE 
          
          r.regs <- BuildRegulatorSide("input$eqn_reverse_regulator_edit", 
                                       "input$eqn_reverse_rateConstant_edit", 
                                       n.r.reg,
                                       var.LHS,
                                       var.RHS,
                                       FALSE)
          RMs     <- r.regs["regulators"]
          RM.RC   <- r.regs["rateConstants"]
          p.add   <- c(p.add, r.regs["P.to.add"][[1]])
          d.add   <- c(d.add, r.regs["P.descriptions"][[1]])
          RMs     <- paste(RMs, collapse = " ")
          RM.RC   <- paste(RM.RC, collapse = " ")
        }
        else{
          kr      <- input$eqn_chem_back_k_edit
          RM.bool <- FALSE
          RMs     <- NA
          RM.RC   <- NA
          p.add   <- c(p.add, kr)
          
          kr.d <- paste0("Reverse rate constant for the reaction of ",
                         paste0(str_split(var.LHS, " ")[[1]], collapse = ", "),
                         " to ",
                         paste0(str_split(var.RHS, " ")[[1]], collapse = ", ")
          )
          d.add <- c(d.add, kr.d)
        } 
      } else if (arrow == "forward_only") {
        
        # Set reverse regulator variables to NA
        kr <- NA
        RM.bool <- FALSE
        RMs <- NA
        RM.RC <- NA
        
        if (input$eqn_options_chem_modifier_forward_edit) {
          kf      <- NA
          FM.bool <- TRUE
          
          f.regs <- BuildRegulatorSide("input$eqn_forward_regulator_edit", 
                                       "input$eqn_forward_rateConstant_edit", 
                                       n.f.reg,
                                       var.LHS,
                                       var.RHS,
                                       TRUE)
          FMs     <- f.regs["regulators"]
          FM.RC   <- f.regs["rateConstants"]
          p.add   <- c(p.add, f.regs["P.to.add"][[1]])
          d.add   <- c(d.add, f.regs["P.descriptions"][[1]])
          FMs     <- paste(FMs, collapse = " ")
          FM.RC   <- paste(FM.RC, collapse = " ")
        } else {
          kf <- input$eqn_chem_forward_k_edit
          p.add <- c(p.add, kf)
          FM.bool <- FALSE
          FMs <- NA
          FM.RC <- NA
        }
      }
      
      eqn.description = ""
      to.add  <- c(var.LHS, var.RHS)
      to.add  <- to.add[!is.na(to.add)]
      var.add <- paste(to.add, collapse = " ")
    }
    
    # Add equation to df
    passed.error.check <- CheckParametersForErrors(p.add, 
                                                   vars$species, 
                                                   params$vars.all,
                                                   onEdit = TRUE)
    
    if (passed.error.check) {
      # Store parameters to parameter vector
      for (i in seq(length(p.add))) {
        StoreParamsEqn(p.add[i], d.add[i])
      }
      # Store up params and variables in equation
      
      # Generate eqn ID
      ID.gen <- GenerateId(id$id.eqn.seed, "eqn")
      id$id.eqn.seed <- id$id.eqn.seed + 1
      ID <- ID.gen["id"]
      #Build up Dataframe rows
      row.to.df.chem <- c(ID,
                          law,
                          coef.LHS, 
                          var.LHS, 
                          coef.RHS, 
                          var.RHS, 
                          arrow,
                          kf, 
                          kr,
                          FM.bool, 
                          FMs, 
                          FM.RC,
                          RM.bool, 
                          RMs, 
                          RM.RC
      )
      row.to.df.info <- c(ID,
                          eqn.type,
                          law,
                          var.add,
                          paste0(p.add, collapse = " "),
                          compartment,
                          eqn.description)
      
      eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)] <- row.to.df.info
      eqns$eqn.chem[eqn.num, 1:ncol(eqns$eqn.chem)] <- row.to.df.chem
      eqns$main[as.numeric(eqn.num)] <- equationBuilder_edit()
    }
  } else if(eqn.type == "enzyme_rxn") {
    
    if (input$eqn_enzyme_law_edit == "Michaelis Menten") {
      
      eqn.description <- ""
      compartment     <- 1
      law             <- "Michaelis Menten"
      
      substrate  <- input$eqn_enzyme_substrate_edit
      product    <- input$eqn_enzyme_product_edit
      Km         <- input$eqn_enzyme_Km_edit
      arrow      <- "forward_only"
      p.add      <- c(Km)
      var.add    <- c(substrate, product)
      
      Km.d <- paste0("Michaelis Menten constant for the enzymatic conversion of ",
                     substrate,
                     " to ",
                     product
                     )
      
      d.add <- c(Km.d)
      
      if (!input$eqn_options_enzyme_useVmax_edit) {
        kcat    <- input$eqn_enzyme_kcat_edit
        enzyme  <-  input$eqn_enzyme_enzyme_edit
        Vmax    <-  NA
        p.add   <- c(p.add, kcat)
        
        kcat.d <- paste0("Rate constant for the enzymatic conversion of ",
                         substrate,
                         " to ",
                         product
        )
        d.add <- c(d.add, kcat.d)
        
      } else if (input$eqn_options_enzyme_useVmax_edit) {
        Vmax   <- input$eqn_enzyme_Vmax_edit
        kcat   <- NA
        enzyme <- NA
        p.add  <- c(p.add, Vmax)
        
        Vmax.d <- paste0("Maximum velocity for the enzymatic conversion of ",
                         substrate,
                         " to ",
                         product
        )
        d.add <- c(d.add, Vmax.d)
      }
      
      passed.error.check <- CheckParametersForErrors(p.add, 
                                                     vars$species, 
                                                     params$vars.all,
                                                     onEdit = TRUE)
      
      if (passed.error.check) {
        
        for (i in seq(length(p.add))) {
          StoreParamsEqn(p.add[i], pDescription = d.add[i])
        }
        
        # Generate eqn ID
        ID.gen <- GenerateId(id$id.eqn.seed, "eqn")
        id$id.eqn.seed <- id$id.eqn.seed + 1
        ID <- ID.gen["id"]
        
        row.to.df.info <- c(ID,
                            eqn.type,
                            law,
                            paste0(var.add, collapse = " "),
                            paste0(p.add, collapse = " "),
                            compartment,
                            eqn.description)
        
        row.to.df.enzyme <- c(ID,
                              law,
                              substrate,
                              product, 
                              enzyme,
                              kcat,
                              Km, 
                              Vmax)
        
        eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)] <- row.to.df.info
        eqns$eqn.enzyme[eqn.num, 1:ncol(eqns$eqn.enzyme)] <- row.to.df.enzyme
        eqns$main[as.numeric(eqn.num)] <- equationBuilder_edit()
      }
    }
  } else if (eqn.type == "syn") {
    compartment <- 1
    
    if (input$eqn_syn_law_edit == "rate") {
      
      eqn.d   <- ""
      var     <- input$eqn_syn_rate_var_edit
      rc      <- input$eqn_syn_rate_RC_edit
      p.add   <- c(p.add, rc)
      var.add <- c(var.add, var)
      
      rc.d    <- paste0("Synthesis rate constant for ", var)
      d.add   <- c(d.add, rc.d)
      
      factor  <- NA
    } else if (input$eqn_syn_law_edit == "byFactor") {
      
      eqn.d   <- ""
      var     <- input$eqn_syn_sby_var_edit
      rc      <- input$eqn_syn_sby_RC_edit
      factor  <- input$eqn_syn_sby_factor_edit
      p.add   <- c(p.add, rc)
      var.add <- c(var.add, var)
      
      rc.d    <- paste0("Synthesis rate constant of ", var, " by factor ", factor)
      d.add   <- c(d.add, rc.d)
    }
    passed.error.check <- CheckParametersForErrors(p.add, 
                                                   vars$species, 
                                                   params$vars.all,
                                                   onEdit = TRUE)
    
    if (passed.error.check) {
      
      # Store parameters to parameter vector
      for (i in seq(length(p.add))) {
        StoreParamsEqn(p.add[i], d.add[i])
      }
      
      # Generate eqn ID
      ID.gen <- GenerateId(id$id.eqn.seed, "eqn")
      id$id.eqn.seed <- id$id.eqn.seed + 1
      ID <- ID.gen["id"]
      
      #Build up Dataframe rows
      row.to.df.syn <- c(ID,
                         input$eqn_syn_law_edit,
                         var,
                         rc, 
                         factor)
      
      row.to.df.info <- c(ID,
                          eqn.type,
                          input$eqn_syn_law_edit,
                          paste0(var.add, collapse = " "),
                          paste0(p.add, collapse = " "),
                          compartment,
                          eqn.d)
      
      eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)] <- row.to.df.info
      eqns$eqn.syn[eqn.num, 1:ncol(eqns$eqn.syn)] <- row.to.df.syn
      eqns$main[as.numeric(eqn.num)] <- equationBuilder_edit()
    }
  } else if (eqn.type == "deg") {
    compartment <- 1
    if (input$eqn_deg_to_products_edit) {
      num.deg.products <- as.numeric(input$eqn_deg_num_products_edit)
      product <- c()
      for (i in seq(num.deg.products)) {
        prod <- eval(parse(text = paste0("input$eqn_deg_product_edit", as.character(i))))
        product <- c(product, prod)
      }
      var.add <- c(var.add, product)
      product <- paste0(product, collapse = " ")
    } else {
      product <- NA
    }
    
    if (input$eqn_deg_law_edit == "rate") {
      
      eqn.d   <- ""
      var     <- input$eqn_deg_var_edit
      rc      <- input$eqn_deg_rate_RC_edit
      jPrint("Rc1")
      jPrint(rc)
      ConcDep <- input$eqn_deg_rate_conc_dependent_edit
      p.add   <- c(p.add, rc)
      var.add <- c(var.add, var)
      
      rc.d    <- paste0("Degradation rate constant for ", var)
      d.add   <- c(d.add, rc.d)
      
      enz    <- NA
      Km     <- NA
      Vmax   <- NA
      kcat   <- NA
      
    } else if (input$eqn_deg_law_edit == "byEnzyme") {
      
      eqn.d   <- ""
      ConcDep <- FALSE
      var     <- input$eqn_deg_var_edit
      Km      <- input$eqn_deg_Km_edit
      p.add   <- c(p.add, Km)
      var.add <- c(var.add, var)
      
      Km.d    <- paste0("Michelias Menten constant for degradation of ", var)
      d.add   <- c(d.add, Km.d)
      
      if (input$eqn_deg_use_Vmax_edit) {
        Vmax  <- input$eqn_deg_Vmax_edit
        p.add <- c(p.add, Vmax)
        rc    <- NA
        enz   <- NA
        
        Vmax.d  <- paste0("Maximum Velocity for degradation of ", var)
        d.add   <- c(d.add, Vmax.d)
        
      } else {
        rc    <- input$eqn_deg_kcat_edit
        jPrint("Rc")
        jPrint(rc)
        enz   <- input$eqn_deg_enzyme_edit
        p.add <- c(p.add, rc)
        
        kcat.d <- paste0("Enzymatic degradation rate constant of ", var, " by  ", enz)
        d.add  <- c(d.add, kcat.d)
        Vmax <- NA
      }
    }
    jPrint(p.add)
    passed.error.check <- CheckParametersForErrors(p.add, 
                                                   vars$species, 
                                                   params$vars.all,
                                                   onEdit = TRUE)
    
    if (passed.error.check) {
      
      # Store parameters to parameter vector
      for (i in seq(length(p.add))) {
        StoreParamsEqn(p.add[i], d.add[i])
      }
      
      # Generate eqn ID
      ID.gen <- GenerateId(id$id.eqn.seed, "eqn")
      id$id.eqn.seed <- id$id.eqn.seed + 1
      ID <- ID.gen["id"]
      
      #Build up Dataframe rows
      row.to.df.deg <- c(ID,
                         input$eqn_deg_law_edit,
                         var,
                         ConcDep,
                         rc,
                         Km, 
                         enz,
                         Vmax,
                         product
      )
      
      row.to.df.info <- c(ID,
                          eqn.type,
                          input$eqn_deg_law_edit,
                          paste0(var.add, collapse = " "),
                          paste0(p.add, collapse = " "),
                          compartment,
                          eqn.d
      )
      
      eqns$eqn.info[eqn.num, 1:ncol(eqns$eqn.info)] <- row.to.df.info
      eqns$eqn.deg[eqn.num, 1:ncol(eqns$eqn.deg)] <- row.to.df.deg
      eqns$main[as.numeric(eqn.num)] <- equationBuilder_edit()
      
    }
  }
   
  # Remove parameters that were changed
  jPrint(old.params)
  jPrint(p.add)
  params.to.remove <- setdiff(old.params, p.add)
  jPrint(params.to.remove)
  
  # Check if old parameters are used elsewhere
  p.remove <- c()
  p.save <- c()
  check1 <- FALSE
  check2 <- FALSE
  check3 <- FALSE
  # Search eqns and IO for parameter
  for (param in params.to.remove) {
    for (row in seq(nrow(eqns$eqn.info))) {
      pars.to.check <- str_split(eqns$eqn.info$RateConstants[row], " ")[[1]]
      if (param %in% pars.to.check) {
        check1 <- TRUE
      }
    }
    # check1 <- ParameterSearchDF(param, eqns$eqn.info)
    # check2 <- ParameterSearchDF(param, IO$input.info)
    # check3 <- ParameterSearchDF(param, IO$output.info)
    if (check1 || check2 || check3) {
      p.save <- c(p.save, param)
    } else {
      p.remove <- c(p.remove, param)
    }
  }
  #if not, remove it
  for (var in p.remove) {
    DeleteParameters(var)
  }
  #if so, store in message of variables not removed
  if (length(p.save) > 0) {
    message.out <- paste0("The following parameter(s) were not deleted because they are used elsewhere: ",
                          paste0(p.save, collapse=", ")
    )
    session$sendCustomMessage(type = 'testmessage',
                              message = message.out)
  }
  
  updatePickerInput(session, "parameters_filter_type", selected = "Eqns")
  updatePickerInput(session, "parameters_filter_type", selected = "All")
  Sys.sleep(0.5)
  waiter.eqns$hide()
  shinyjs::enable("createEqn_store_edit_button")
})
