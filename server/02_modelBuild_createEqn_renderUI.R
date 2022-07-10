# This script holds the renderUIs for the equation building suite
# w.load.MA.vars <- Waiter$new(id = "eqnCreate_equationBuilder_chem")
# w.load.MA.vars$show()
# w.load.MA.vars$hide()

output$eqnCreate_equationBuilder_chem <- renderUI({
  number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
  number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
  number_forward_regulators = as.numeric(input$eqn_options_chem_num_forward_regulators)
  number_reverse_regulators = as.numeric(input$eqn_options_chem_num_reverse_regulators)
  #Sys.sleep(0.5)
  div(
    fluidRow(
      column(
        style = "border-right: 1px solid #e5e5e5; padding-right:20px",
        width = 4,
        lapply(seq(number_LHS_equations), function(i){
          div(
            HTML(paste0("<b>Reactant ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("LHS_Coeff_", as.character(i)),
                label = NULL,
                value = 1,
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("LHS_Var_", as.character(i)),
                label = NULL,
                choices = sort(vars$species),
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
        lapply(seq(number_RHS_equations), function(i){
          div(
            HTML(paste0("<b>Product ", as.character(i), "</b>")),
            splitLayout(
              numericInput(
                inputId = paste0("RHS_Coeff_", as.character(i)),
                label = NULL,
                value = 1,
                min = 1,
                step = 1),
              pickerInput(
                inputId = paste0("RHS_Var_", as.character(i)),
                label = NULL,
                choices = sort(vars$species),
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
          condition = "!input.eqn_options_chem_modifier_forward",
          textInput(
            inputId = "eqn_chem_forward_k",
            label = "Forward Rate Constant",
            value = paste0("k_f", as.character(eqns$n.eqns.no.del + 1))
          ),
          tags$head(tags$style("#eqn_chem_forward_k {margin-top: -7px;}")),
        ),
        conditionalPanel(
          condition = "input.eqn_chem_forward_or_both=='both_directions' && 
                       !input.eqn_options_chem_modifier_reverse",
          textInput(
            inputId = "eqn_chem_back_k",
            label = "Reverse Rate Constant",
            value = paste0("k_r", as.character(eqns$n.eqns.no.del + 1))
          )
        )
      )#end column
    ), #end fluidRow`
    conditionalPanel(
      condition = "input.eqn_options_chem_modifier_forward || 
                   input.eqn_options_chem_modifier_reverse",
      hr()
    ),
    fluidRow(
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_forward",
          lapply(seq(number_forward_regulators), function(i){
            pickerInput(
              inputId = paste0("eqn_forward_regulator_", as.character(i)),
              label = paste0("Forward Regulator ", as.character(i)),
              choices = sort(vars$species),
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith"))
          })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_forward",
          lapply(seq(number_forward_regulators), function(i){
            textInput(
              inputId = paste0("eqn_forward_rateConstant_", as.character(i)),
              label = paste0("Rate Constant ", as.character(i)),
              value = paste0("k_f", 
                             as.character(eqns$n.eqns.no.del + 1),
                             ".", 
                             as.character(i)
              )
            )
          })
        )
      )
    ),
    fluidRow(
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_reverse",
          lapply(seq(number_reverse_regulators), function(i){
            pickerInput(
              inputId = paste0("eqn_reverse_regulator_", as.character(i)),
              label = paste0("Reverse Regulator ", as.character(i)),
              choices = sort(vars$species),
              options = pickerOptions(liveSearch = TRUE
                                      ,liveSearchStyle = "startsWith")
            )
          })
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.eqn_options_chem_modifier_reverse",
          lapply(seq(number_reverse_regulators), function(i){
            textInput(
              inputId = paste0("eqn_reverse_rateConstant_", as.character(i)),
              label = "Rate Constant",
              value = paste0("k_r",
                             as.character(eqns$n.eqns.no.del + 1),
                             ".",
                             as.character(i))
            )
          })
        )
      )
    )
  )#end div
 
})

output$eqnCreate_equationBuilder_enzyme <- renderUI({
  
  div(
    conditionalPanel(
      condition = "input.eqn_enzyme_law == 'MM'",
      fluidRow(
        column(
          width = 3,
          pickerInput(
            inputId = "eqn_enzyme_substrate",
            label = "Substrate",
            choices = sort(vars$species),
            options = pickerOptions(
              liveSearch = TRUE,
              liveSearchStyle = "startsWith",
              dropupAuto = FALSE
            )
          ),
          conditionalPanel(
            condition = "!input.eqn_options_enzyme_useVmax",
            pickerInput(
              inputId = "eqn_enzyme_enzyme",
              label = "Enzyme",
              choices = sort(vars$species),
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = "startsWith")
            )
          )
        ),
        column(
          width = 3,
          offset = 1,
          conditionalPanel(
            condition = "input.eqn_options_enzyme_useVmax",
            textInput(
              inputId = "eqn_enzyme_Vmax",
              label = "Vmax",
              value = paste0("Vmax_", as.character(eqns$n.eqns.no.del + 1))
            )
          ),
          conditionalPanel(
            condition = "!input.eqn_options_enzyme_useVmax",
            textInput(
              inputId = "eqn_enzyme_kcat",
              label = "kcat",
              value = paste0("kcat_", as.character(eqns$n.eqns.no.del + 1))
            )
          ),
          textInput(
            inputId = "eqn_enzyme_Km",
            label = "Km",
            value = paste0("Km_", as.character(eqns$n.eqns.no.del + 1))
          )
        ),
        column(
          width = 3,
          offset = 1,
          pickerInput(
            inputId = "eqn_enzyme_product",
            label = "Product",
            choices = sort(vars$species),
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
      condition = "input.eqn_enzyme_law == 'Other'",
      "Other enzyme laws will be added in these tabs in the future"
    )
    
  )#end div
})

output$eqnCreate_equationBuilder_synthesis <- renderUI({
  
  div(
    fluidRow(
      column(
        width = 4,
        conditionalPanel(
          condition = "input.eqn_syn_law == 'rate'",
          pickerInput(
            inputId = "eqn_syn_rate_var",
            label   = "Species to synthesize",
            choices = sort(vars$species),
            options = pickerOptions(liveSearch = TRUE
                                     ,liveSearchStyle = "startsWith") 
          ),
          textInput(
            inputId = "eqn_syn_rate_RC",
            label = "Rate Constant",
            value = paste0("k_syn", as.character(eqns$n.eqns.no.del + 1))
          )
        ),
        conditionalPanel(
          condition = "input.eqn_syn_law == 'byFactor'",
          pickerInput(
            inputId = "eqn_syn_sby_var",
            label   = "Species to synthesize",
            choices = sort(vars$species),
            options = pickerOptions(liveSearch = TRUE
                                    ,liveSearchStyle = "startsWith") 
          ),
          pickerInput(
            inputId = "eqn_syn_sby_factor",
            label = "Factor causing synthesis",
            choices = sort(vars$species)
          ),
          textInput(
            inputId = "eqn_syn_sby_RC",
            label = "Rate Constant",
            value = paste0("k_s", as.character(eqns$n.eqns.no.del + 1))
          )
        )
      )
    )
  )
})

output$eqnCreate_equationBuilder_degradation <- renderUI({
  
  div(
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId = "eqn_deg_var",
          label   = "Species to degrade",
          choices = sort(vars$species),
          options = pickerOptions(liveSearch = TRUE
                                  ,liveSearchStyle = "startsWith") 
        )
      ),
      column(
        width = 4,
        conditionalPanel(
          condition = "input.eqn_deg_to_products",
          lapply(seq(input$eqn_deg_num_products), function(i){
            pickerInput(
              inputId = paste0("eqn_deg_product_", as.character(i))
              ,label = paste0("Product ", as.character(i))
              ,choices = sort(vars$species)
              ,options = pickerOptions(liveSearch = TRUE
                                       ,liveSearchStyle = "startsWith"))
          })
        )
      )
    ),
    hr(),
    conditionalPanel(
      condition = "input.eqn_deg_law == 'rate'",
        fluidRow(
          column(
            width = 8,
            splitLayout(
              textInput(
                inputId = "eqn_deg_rate_RC",
                label = "Rate Constant",
                value = paste0("k_d", as.character(eqns$n.eqns.no.del + 1))
              ),
              div(
                style = "padding-top:38px; padding-left:15px;",
                checkboxInput(inputId = "eqn_deg_rate_conc_dependent"
                              ,label = "Concentration Dependent"
                              ,value = TRUE)
              )
            )
          )  
        )
    ),
    conditionalPanel(
      condition = "input.eqn_deg_law == 'byEnzyme'",
      conditionalPanel(
        condition = "!input.eqn_deg_use_Vmax",
        fluidRow(
          column(
            width = 4,
            pickerInput(
              inputId = "eqn_deg_enzyme",
              label = "Enzyme",
              choices = sort(vars$species)
            )
          ),
          column(
            width = 4,
            textInput(
              inputId = "eqn_deg_kcat",
              label = "kcat",
              value = paste0("k_d", as.character(eqns$n.eqns.no.del+1))
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.eqn_deg_use_Vmax",
        fluidRow(
          column(
            width = 4,
            textInput(
              inputId = "eqn_deg_Vmax",
              label = "Vmax",
              value = paste0("Vmax_", as.character(eqns$n.eqns.no.del+1))
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 4,
          textInput(
            inputId = "eqn_deg_Km",
            label = "Km",
            value = paste0("Km_", as.character(eqns$n.eqns.no.del + 1))
          )
        )
      )
    )
  )
})

output$eqnCreate_equationBuilder_simp_diff <- renderUI({
  #number_RHS_equations = as.numeric(input$eqnCreate_num_of_eqn_RHS)
  #number_LHS_equations = as.numeric(input$eqnCreate_num_of_eqn_LHS)
  
  div(
    fluidRow(column(width=3
                    ,pickerInput(inputId="simp_diff_var1"
                                 ,label="Var1"
                                 ,choices=sort(vars$species)
                                 ,options = pickerOptions(liveSearch = TRUE
                                                          ,liveSearchStyle = "startsWith")))
             ,column(width=3
                     ,textInput(inputId="simp_diff_PS_Var"
                                ,label = "Diffusion Constant"
                                ,value = paste0("PS", as.character(eqns$n.eqns.no.del+1))))
             ,column(width=3
                     ,pickerInput(inputId="simp_diff_var2"
                                  ,label="Var2"
                                  ,choices=sort(vars$species)
                                  ,options = pickerOptions(liveSearch = TRUE
                                                           ,liveSearchStyle = "startsWith")))
    )#end fluidRow
    ,fluidRow(column(width=4,
                     checkboxInput(inputId="simp_diff_wayOfDiffusion"
                                   ,label="This diffusion is one way"
                                   ,value = FALSE)
    ))
  )#end div
})

output$eqnCreate_equationBuilder_custom_rate <- renderUI({
  
  div(
    "This is a custom equation entering. Please make sure things are spelled correctly as errors will occur if these equations are improper.",
    "Add any parameters in the 'Parameter Values' tab that would be used in this equation.",
    "These are often used for algebraic equations that need to be added to the model.",
    hr(),
    fluidRow(
      column(
        width = 4,
        textInput(
          inputId = "eqnCreate_custom_eqn_lhs",
          label = "Variable",
          value = "")
    ),
    column(
      width = 1,
      div(style = "padding-top:35px; padding-left: 12px;",
          "=")), 
    column(
      width = 7,
      textInput(
        inputId = "eqnCreate_custom_eqn_rhs",
        label = "Equation",
        value = ""
      )
    ))
  )
})

output$eqnCreate_equationBuilder_time_equation <- renderUI({
  
  div(
    "This is a custom equation entering. Please make sure things are spelled correctly as errors will occur if these equations are improper.",
    "There is a box below to add any parameters that could be used in your time dependent equations.",
    "Please note that 't' is used for time",
    hr(),
    fluidRow(
      column(
        width = 4,
        textInput(inputId = "eqnCreate_time_dependent_parameters",
                  label = "Parameters to add",
                  value = "")
        ),
      column(
        width = 4,
        align = "left",
        div(style = "padding-top: 30px;",
            actionButton(
              inputId = "eqnCreate_time_dependent_store_new_parameter",
              label = "Store Parameter"))
        
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 4,
        textInput(
          inputId = "eqnCreate_time_dependent_firstvar",
          label = "Time Dependent Variable",
          value = ""
        )
      ),
      column(width = 1,
             div(style = "padding-top: 38px; padding-left: 12px",
                 "=")
             ),
      column(
        width = 7,
        textInput(
          inputId = "eqnCreate_time_dependent_equation",
          label = "Equation",
          value = ""
        )
      )
    )
  )
})



