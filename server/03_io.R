# This file contains all inputs and output related server functions for species
# entering or leaving compartments.

watier.IO <- Waiter$new(
  html =  tagList(
    div(
      style = "color:black",
      spin_whirly(),
      hr(),
      h4("Storing Input/Output...")
    )
  ),
  color = transparent(0.7)
)

# Update UI --------------------------------------------------------------------

# Render Text ------------------------------------------------------------------

# Flow in
output$CIO_fi_vo_text <- renderText(
  out <- paste0("Flow Value (",
                rv.UNITS$units.selected$Volume, 
                "/", 
                rv.UNITS$units.selected$Duration, 
                ")"
  )
)

# Flow out
output$CIO_fo_vo_text <- renderText(
  out <- paste0("Flow Value (",
                rv.UNITS$units.selected$Volume, 
                "/", 
                rv.UNITS$units.selected$Duration, 
                ")"
  )
)

# Flow between
output$CIO_fb_vo_text <- renderText(
  out <- paste0("Flow Value (",
                rv.UNITS$units.selected$Volume, 
                "/", 
                rv.UNITS$units.selected$Duration, 
                ")"
  )
)  

# Flow between 2
output$CIO_fb_sv1_text <- renderText(
  out <- paste0("Flow Value (",
                rv.UNITS$units.selected$Volume, 
                "/", 
                rv.UNITS$units.selected$Duration, 
                ")"
  )
)

# Clearance
output$CIO_clearance_unit_text <- renderText(
  # out <- paste0("Value (",
  #               "1/",
  #               rv.UNITS$units.selected$Duration,
  #               ")")
  out <- "Value"
)

# Simple Diffusion
output$CIO_simpdiff_unit_text <- renderText(
  # out <- paste0("Value (",
  #               rv.UNITS$units.selected$Volume, "/",
  #               rv.UNITS$units.selected$Duration,
  #               ")")
  out <- "Value"
)

# Facilitated Diffusion
output$CIO_fd_vmax_unit_text <- renderText(
  # Vmax 
  # out <- paste0("Value (",
  #               rv.UNITS$units.selected$For.Var, "/",
  #               rv.UNITS$units.selected$Duration,
  #               ")")
  out <- "Value"
)

output$CIO_fd_km_unit_text <- renderText(
  # Km
  # out <- paste0("Value (",
  #               rv.UNITS$units.selected$For.Var,
  #               ")")
  out <- "Value"
)

# Render UI --------------------------------------------------------------------
## Render for flowbetween ------------------------------------------------------
output$CIO_flow_between_render_compartments <- renderUI({
  c.names <- rv.COMPARTMENTS$compartments.names
  in.choices <- c.names[! c.names %in% input$CIO_flowbetween_compartment_out]
  num_flow_to_add <- as.numeric(input$CIO_flowbetween_number_split) - 1
  if(input$CIO_flowbetween_split && num_flow_to_add > 0) {
    div(
      lapply(seq(num_flow_to_add), function(i){
        pickerInput(
          inputId = paste0("CIO_flowbetween_compartment_in_", 
                           as.character(i+1)),
          label = "Flow Into",
          choices = in.choices
        )
      })
    )
  }
})

output$CIO_flow_between_render_species <- renderUI({
  num_flow_to_add <- as.numeric(input$CIO_flowbetween_number_split) - 1
  if(input$CIO_flowbetween_split && num_flow_to_add > 0) {
    div(
      lapply(seq(num_flow_to_add), function(i){
        pickerInput(
          inputId = paste0("CIO_flowbetween_species_in_",
                           as.character(i+1)),
          label = paste0("Species In ", as.character(i+1)),
          choices = unlist(rv.SPECIES$species.df %>%
                dplyr::filter(Compartment %in%
                   eval(parse(
                     text =
                       paste0("input$CIO_flowbetween_compartment_in_",
                              as.character(i + 1))
                   ))) %>%
                select(Name),
                use.names = FALSE)
        )
      })
    )
  }
})

output$CIO_flow_between_render_flow_variables <- renderUI({
  num_flow_to_add <- as.numeric(input$CIO_flowbetween_number_split) - 1
  if(input$CIO_flowbetween_split && num_flow_to_add > 0) {
    div(
      lapply(seq(num_flow_to_add), function(i){
        textInput(
          inputId = paste0("CIO_flowbetween_flow_variable_in_",
                           as.character(i + 1)),
          label = paste0("Flow Variable ", as.character(i + 1)),
          value = paste0("F_in_",
                         rv.IO$IO.id.counter,
                         ".",
                         as.character(i + 1))
        )
      })
    )
  }
})

output$CIO_flow_between_render_flow_values <- renderUI({
  num_flow_to_add <- as.numeric(input$CIO_flowbetween_number_split) - 1
  to.units <- "Flow Value" 
                     
  if(input$CIO_flowbetween_split && num_flow_to_add > 0) {
    div(
      lapply(seq(num_flow_to_add), function(i){
        textInput(
          inputId = paste0("CIO_flowbetween_flow_value_in_",
                           as.character(i + 1)),
          label = to.units,
          value = 0
        )
      })
    )
  }
})


# Events -----------------------------------------------------------------------
## Flow In ---------------------------------------------------------------------
observeEvent({input$CIO_flow_in_compartment
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    
    for.choice <- 
      rv.SPECIES$species.df %>% 
      filter(Compartment == input$CIO_flow_in_compartment) %>%
      select(Name)
    
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session, 
                      "CIO_flow_in_species", 
                      choices = for.choice)
  })

## Flow out --------------------------------------------------------------------
observeEvent({input$CIO_flow_out_compartment
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    
    for.choice <- 
      rv.SPECIES$species.df %>% 
      filter(Compartment == input$CIO_flow_out_compartment) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    

    updatePickerInput(session, 
                      "CIO_flow_out_species", 
                      choices = for.choice)
  })

## Flow between - out ----------------------------------------------------------
observeEvent({input$CIO_flowbetween_compartment_out
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    # For out species
    for.choice <- 
      rv.SPECIES$species.df %>% 
      filter(Compartment == input$CIO_flowbetween_compartment_out) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session, 
                      "CIO_flowbetween_species_out", 
                      choices = for.choice)
    
    # For into compartment 1 (that is hard coded)
    # Want to remove the selected compartment out from choices in
    c.names <- rv.COMPARTMENTS$compartments.names
    c.1.choices <- c.names[! c.names %in% input$CIO_flowbetween_compartment_out]
    # if one compartment this will be empty and throw error
    #c.1.choices <- ifelse(is_empty(c.1.choices), NULL, c.1.choices)
    updatePickerInput(session, 
                      "CIO_flowbetween_compartment_in_1",
                      choices = c.1.choices)
  })

## Flow between - in -----------------------------------------------------------
observeEvent({input$CIO_flowbetween_compartment_in_1
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
  req(!is_empty(rv.SPECIES$species.df))
  if (is.null(input$CIO_flowbetween_compartment_in_1)) {
    for.choice <- NULL
  } else {
    for.choice <-
      rv.SPECIES$species.df %>%
      filter(Compartment == input$CIO_flowbetween_compartment_in_1) %>%
      select(Name) %>%
      unlist(use.names = FALSE)
  }
  
  updatePickerInput(session,
                    "CIO_flowbetween_species_in_1",
                    choices = for.choice)
})

## Clearance -------------------------------------------------------------------
observeEvent({input$CIO_clearance_compartment
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    
    for.choice <- 
      rv.SPECIES$species.df %>% 
      dplyr::filter(Compartment == input$CIO_clearance_compartment) %>%
      dplyr::select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session, 
                      "CIO_clearance_species", 
                      choices = for.choice,
                      selected = for.choice[1])
  })

## Simple Diffusion ------------------------------------------------------------
observeEvent({input$CIO_simpdiff_compartment1
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    
    for.choice <- 
      rv.SPECIES$species.df %>% 
      dplyr::filter(Compartment == input$CIO_simpdiff_compartment1) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session, 
                      "CIO_simpdiff_species1", 
                      choices = for.choice)
  })

observeEvent({input$CIO_simpdiff_compartment2
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    
    for.choice <- 
      rv.SPECIES$species.df %>% 
      dplyr::filter(Compartment == input$CIO_simpdiff_compartment2) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session,
                      "CIO_simpdiff_species2", 
                      choices = for.choice)
  })

## Facilitated Diffusion ------------------------------------------------------
observeEvent({input$CIO_facilitatedDiff_compartment1
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))

    for.choice <- 
      rv.SPECIES$species.df %>% 
      dplyr::filter(Compartment == input$CIO_facilitatedDiff_compartment1) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session, 
                      "CIO_facilitatedDiff_species1", 
                      choices = for.choice)
  })

observeEvent({input$CIO_facilitatedDiff_compartment2
  rv.SPECIES$species.df
  rv.COMPARTMENTS$compartments}, {
    req(!is_empty(rv.SPECIES$species.df))
    
    for.choice <- 
      rv.SPECIES$species.df %>% 
      dplyr::filter(Compartment == input$CIO_facilitatedDiff_compartment2) %>%
      select(Name)
    for.choice <- unlist(for.choice, use.names = FALSE)
    
    updatePickerInput(session, 
                      "CIO_facilitatedDiff_species2", 
                      choices = for.choice)
  })


# ADD IO -----------------------------------------------------------------------
observeEvent(input$CIO_add_IO, {
  
  watier.IO$show()
  shinyjs::disable("CIO_add_IO")
  Sys.sleep(0.5)
  
  # Dataframe Storage
  direction <- NA
  type      <- NA  # Type of Input/Output 
  compartment.out     <- NA  # Compartment from
  compartment.in      <- NA  # Compartment to
  compartment.out.id  <- NA
  compartment.in.id   <- NA
  species.out     <- NA  # Species from
  species.in      <- NA  # Species to
  species.out.id  <- NA
  species.in.id   <- NA
  flow.rate <- NA  # Flow Rate Constant
  flow.unit <- NA  # Flow Rate Unit
  flow.spec <- NA  # Species in Flow
  sol.const <- NA  # Solubility Constant (PS)
  sol.unit  <- NA
  fac.Vmax  <- NA  # Facilitated Diffusion Vmax
  fac.Km    <- NA  # Facilitated Diffusion Km
  fac.Vmax.u<- NA
  fac.Km.u  <- NA
  log       <- NA
  
  is.reversible <- FALSE
  
  # Parameter Storage
  params          <- c()
  param.descript  <- c()
  param.units     <- c()
  unit.descript   <- c()
  base.units      <- c()
  base.vals       <- c()
  param.vals      <- c()

  ## Flow In -------------------------------------------------------------------
  if (input$CIO_IO_options == "FLOW_IN") {
    direction         <- "Input"
    display           <- "Flow In"
    type              <- input$CIO_IO_options
    law               <- "flow"
    compartment.in    <- input$CIO_flow_in_compartment
    species.in        <- input$CIO_flow_in_species
    compartment.in.id <- FindId(compartment.in)
    species.in.id     <- FindId(species.in)
    
    flow.rate <- input$CIO_flow_in_rate_constant
    flow.unit <- paste0(rv.UNITS$units.selected$Volume, "/",
                        rv.UNITS$units.selected$Duration)
    
    f.v <- input$CIO_flow_in_value
    log       <- paste0("Flow into compartment (",
                        compartment.in,
                        ") with species (",
                        species.in, 
                        ") at rate of ",
                        flow.rate, " ", flow.unit, ".")
    f.u       <- paste0(rv.UNITS$units.selected$Volume, "/",
                        rv.UNITS$units.selected$Duration)
    b.u  <- paste0(rv.UNITS$units.base$Volume, "/", 
                   rv.UNITS$units.base$Duration)
    
    u.d  <- "volume <div> time"
    d    <- paste0("Flow rate into ",
                   compartment.in)
    # Convert base unit if needed
    if (f.u != b.u) {
      b.v <- UnitConversion(u.d, f.u, b.u, as.numeric(f.v))
    } else {
      b.v <- f.v
    }
    
    params          <- c(params, flow.rate)
    param.descript  <- c(param.descript, d)
    param.vals      <- c(param.vals,f.v)
    param.units     <- c(param.units, flow.unit)
    unit.descript   <- c(unit.descript, u.d)
    base.units      <- c(base.units, b.u)
    base.vals       <- c(base.vals, b.v)
    
    laws <- Flow(species.in, flow.rate)
    
    description <- paste0("Flow of ", 
                          species.in, 
                          " into ",
                          compartment.in,
                          " with rate, ",
                          flow.rate)
  } 
  else if (input$CIO_IO_options == "FLOW_OUT") {
  ## Flow out ------------------------------------------------------------------
    direction          <- "Output"
    display            <- "Flow In"
    type               <- input$CIO_IO_options
    law                <- "flow"
    compartment.out    <- input$CIO_flow_out_compartment
    species.out        <- input$CIO_flow_out_species
    compartment.out.id <- FindId(compartment.out)
    species.out.id     <- FindId(species.out)
    
    flow.rate   <- input$CIO_flow_out_rate_constant
    flow.unit   <- paste0(rv.UNITS$units.selected$Volume, "/",
                          rv.UNITS$units.selected$Duration)
    
    f.v <- input$CIO_flow_out_value
    
    log       <- paste0("Flow out of compartment (",
                        compartment.out,
                        ") with species (",
                        species.out, 
                        ") at rate of ",
                        flow.rate, " ", flow.unit, ".")
    
    f.u       <- paste0(rv.UNITS$units.selected$Volume, "/",
                        rv.UNITS$units.selected$Duration)
    
    b.u  <- paste0(rv.UNITS$units.base$Volume, "/", 
                   rv.UNITS$units.base$Duration)
    
    u.d  <- "volume <div> time"
    d    <- paste0("Flow rate out of ",
                   compartment.out)
    
    if (f.u != b.u) {
      b.v <- UnitConversion(u.d, f.u, b.u, as.numeric(f.v))
    } else {
      b.v <- f.v
    }
    
    params          <- c(params, flow.rate)
    param.descript  <- c(param.descript, d)
    param.vals      <- c(param.vals,f.v)
    param.units     <- c(param.units, flow.unit)
    unit.descript   <- c(unit.descript, u.d)
    base.units      <- c(base.units, b.u)
    base.vals       <- c(base.vals, b.v)
    
    laws <- Flow(species.out, flow.rate)
    
    description <- paste0("Flow of ", 
                          species.out, 
                          " out of ",
                          compartment.out,
                          " with rate, ",
                          flow.rate)
  } 
  else if (input$CIO_IO_options == "FLOW_BETWEEN") {
  ## Flow between --------------------------------------------------------------
    # browser()
    direction <- "Both"
    type      <- input$CIO_IO_options
    
    # Out Flow Components
    compartment.out     <- input$CIO_flowbetween_compartment_out
    species.out         <- input$CIO_flowbetween_species_out
    compartment.out.id  <- FindId(compartment.out)
    species.out.id      <- FindId(species.out)
    
    # Out Flow Parameter
    f.out     <- input$CIO_flowbetween_flow_variable_out
    f.v       <- input$CIO_flowbetween_flow_value_out
    d         <- paste0("Flow rate out from ", compartment.out)
    f.u       <- paste0(rv.UNITS$units.selected$Volume, "/",
                        rv.UNITS$units.selected$Duration)
    b.u       <- paste0(rv.UNITS$units.base$Volume, "/",
                        rv.UNITS$units.base$Duration)
    u.d       <- "volume <div> time"
    
    # Convert base unit if needed
    if (f.u != b.u) {
      b.v <- UnitConversion(u.d, f.u, b.u, as.numeric(f.v))
    } else {
      b.v <- f.v
    }
    
    params          <- c(params, f.out)
    param.descript  <- c(param.descript, d)
    param.vals      <- c(param.vals,f.v)
    param.units     <- c(param.units, f.u)
    unit.descript   <- c(unit.descript, u.d)
    base.units      <- c(base.units, b.u)
    base.vals       <- c(base.vals, b.v)
    
    
    # In Flow Components (this could have multiple components)
    if (!input$CIO_flowbetween_split) {
      # No Splits
      compartment.in  <- input$CIO_flowbetween_compartment_in_1
      species.in  <- input$CIO_flowbetween_species_in_1
      compartment.in.id   <- FindId(compartment.in)
      species.in.id   <- FindId(species.in)
      n.split <- 1
      f.in <- f.out
      log   <- paste0("Flow between ", 
                      compartment.out, " and ", compartment.in,
                      " at flow of ", f.out, ".")
    } else {
      # Input Flow is Split into Multiple Flows
      compartment.in    <- c()
      species.in        <- c()
      compartment.in.id <- c()
      species.in.id     <- c()
      
      f.in    <- c()
      f.u     <- c()
      f.v     <- c()
      b.u     <- c()
      u.d     <- c()
      d       <- c()
      b.v     <- c()
      n.split <- input$CIO_flowbetween_number_split
      # browser()
      for (i in seq(n.split)) {
        compartment.in <- 
          c(compartment.in, 
            eval(parse(text = paste0("input$CIO_flowbetween_compartment_in_",
                                     as.character(i))))
            )
        compartment.in.id <- c(compartment.in.id, FindId(compartment.in[i]))
        species.in <- 
          c(species.in, 
            eval(parse(text = paste0("input$CIO_flowbetween_species_in_",
                                     as.character(i))))
          )
        species.in.id <- c(species.in.id, FindId(species.in[i]))
        f.in <- 
          c(f.in, 
            eval(parse(text = paste0("input$CIO_flowbetween_flow_variable_in_",
                                     as.character(i))))
          )
        
        f.v <- 
          c(f.v,
            eval(parse(text = paste0("input$CIO_flowbetween_flow_value_in_",
                                     as.character(i)))))
          
        
        b.u  <- c(b.u, paste0(rv.UNITS$units.base$Volume, "/",
                      rv.UNITS$units.base$Duration))
        f.u  <- c(f.u, paste0(rv.UNITS$units.selected$Volume, "/",
                      rv.UNITS$units.selected$Duration))
        u.d  <- c(u.d, "volume <div> time")
        d    <- c(d, paste0("Flow rate from ",compartment.out, " to ", compartment.in[i]))
        
        # Convert base unit if needed
        cur.idx <- length(b.u)
        if (f.u[cur.idx] != b.u[cur.idx]) {
          b.v <- c(b.v, UnitConversion(u.d[cur.idx], 
                                       f.u[cur.idx], 
                                       b.u[cur.idx], 
                                       as.numeric(f.v[cur.idx]
                                                  )))
        } else {
          b.v <- c(b.v, f.v[cur.idx])
        }
      }
      
      log       <- paste0("Flow between compartments.")
      
      params          <- c(params, f.in)
      param.descript  <- c(param.descript, d)
      param.vals      <- c(param.vals,f.v)
      param.units     <- c(param.units, f.u)
      unit.descript   <- c(unit.descript, u.d)
      base.units      <- c(base.units, b.u)
      base.vals       <- c(base.vals, b.v)
    }
    laws <- FlowBetween(species.out,
                        species.in,
                        compartment.in,
                        compartment.out,
                        params
                        )
  } 
  else if (input$CIO_IO_options == "CLEARANCE") {
  ## Clearance -----------------------------------------------------------------
    direction           <- "Output"
    display             <- "Clearance"
    type                <- input$CIO_IO_options
    law                 <- "clearance"
    compartment.out     <- input$CIO_clearance_compartment
    species.out         <- input$CIO_clearance_species
    compartment.out.id  <- FindId(compartment.out)
    species.out.id      <- FindId(species.out)
    
    flow.rate <- input$CIO_clearance_rate_constant
    flow.unit <- paste0("1/", rv.UNITS$units.selected$Duration)
    
    f.v <- input$CIO_clearance_value
    
    log       <- paste0("Clearance of ",
                        paste0(input$CIO_clearance_species, collapse = ", "),
                        " by flow rate of ",
                        flow.rate, " (", flow.unit, ").")
    
    f.u  <- paste0("1/", rv.UNITS$units.selected$Duration)
    b.u  <- paste0("1/", rv.UNITS$units.base$Duration)
    
    u.d  <- "num <div> time"
    d    <- paste0("Clearance rate constant for ",
                   species.out, 
                   " of compartment ", 
                   compartment.out)
    
    if (f.u != b.u) {
      b.v <- UnitConversion(u.d, f.u, b.u, as.numeric(f.v))
    } else {
      b.v <- f.v
    }
    
    params          <- c(params, flow.rate)
    param.descript  <- c(param.descript, d)
    param.vals      <- c(param.vals,f.v)
    param.units     <- c(param.units, flow.unit)
    unit.descript   <- c(unit.descript, u.d)
    base.units      <- c(base.units, b.u)
    base.vals       <- c(base.vals, b.v)
    
    # Search compartment rv with comp.id, Ggab volume variable
    compVol <- rv.COMPARTMENTS$compartments[[compartment.out.id]]$Volume
    
    laws <- Clearance(species.out, flow.rate, compVol)
    
    description <- paste0("Flow of ", 
                          species.out, 
                          " out of ",
                          compartment.out,
                          " with rate, ",
                          flow.rate)
  } 
  else if (input$CIO_IO_options == "SIMPDIFF") {
  ## Simple Diffusion ----------------------------------------------------------
    direction        <- "Both"
    type             <- input$CIO_IO_options
    compartment.out  <- input$CIO_simpdiff_compartment1
    compartment.in   <- input$CIO_simpdiff_compartment2
    species.out      <- input$CIO_simpdiff_species1
    species.in       <- input$CIO_simpdiff_species2
    
    # Store Ids
    compartment.in.id   <- FindId(compartment.in)
    species.in.id       <- FindId(species.in)
    compartment.out.id  <- FindId(compartment.out)
    species.out.id      <- FindId(species.out)
    
    # Parameter Storage
    # Parameter Variable
    sol.const <- input$CIO_simpdiff_rate_constant
    # Value
    f.v   <- input$CIO_simpdiff_value
    # Unit
    f.u   <- paste0(rv.UNITS$units.selected$Volume, "/",
                        rv.UNITS$units.selected$Duration)
    
    # Base Imot
    b.u  <- paste0(rv.UNITS$units.base$Volume, "/", 
                       rv.UNITS$units.base$Duration)
    
    # Unit Description
    u.d  <- "volume <div> time"
    
    # Base value determination
    if (f.u != b.u) {
      b.v <- UnitConversion(u.d, f.u, b.u, as.numeric(f.v))
    } else {
      b.v <- f.v
    }
    
    # Reaction Description
    sol.d    <- paste0("Solubility constant for the simple diffusion of ",
                       species.out, 
                       " to ", 
                       species.in)
    
    # Log Outputs
    log       <- paste0("Simple Diffusion of ",
                        species.out,
                        " to ",
                        species.in,
                        " from compartment ",
                        compartment.out, " to ", compartment.in)
    
    # Store to output vectors
    params          <- c(params, sol.const)
    param.descript  <- c(param.descript, sol.d)
    param.vals      <- c(param.vals,f.v)
    param.units     <- c(param.units, f.u)
    unit.descript   <- c(unit.descript, u.d)
    base.units      <- c(base.units, b.u)
    base.vals       <- c(base.vals, b.v)
    
    laws <- SimpleDiffusion(species.out, species.in, sol.const)
    
  } 
  else if (input$CIO_IO_options == "FACILITATED_DIFF") {
  ## Facilitated Diffusion ------------------------------------------------------
    direction       <- "Both"
    type            <- input$CIO_IO_options
    compartment.out <- input$CIO_facilitatedDiff_compartment1
    compartment.in  <- input$CIO_facilitatedDiff_compartment2
    species.out     <- input$CIO_facilitatedDiff_species1
    species.in      <- input$CIO_facilitatedDiff_species2
    
    # Store Ids
    compartment.in.id   <- FindId(compartment.in)
    species.in.id       <- FindId(species.in)
    compartment.out.id  <- FindId(compartment.out)
    species.out.id      <- FindId(species.out)
    
    # Parameters
    Vmax.var  <- input$CIO_facilitatedDiff_Vmax
    Vmax.val  <- input$CIO_facilitatedDiff_Vmax_value
    Vmax.unit <- paste0(rv.UNITS$units.selected$For.Var, "/",
                        rv.UNITS$units.selected$Duration)
    Vmax.b.u  <- paste0(rv.UNITS$units.base$For.Var, "/",
                        rv.UNITS$units.base$Duration)
    Vmax.u.d  <- paste0("conc (",
                        input$GO_species_unit_choice,
                        ") <div> time")
    
    Vmax.d <- paste0("Maximum velocity for the facilitated Diffusion of ",
                     species.out,
                     " to ",
                     species.in
    )
    
    # Base value determination
    if (Vmax.unit != Vmax.b.u) {
      Vmax.b.v <- 
        UnitConversion(Vmax.u.d, Vmax.unit, Vmax.b.u, as.numeric(Vmax.val))
    } else {
      Vmax.b.v <- Vmax.val
    }
    
    Km.var     <- input$CIO_facilitatedDiff_Km
    Km.val     <- input$CIO_facilitatedDiff_Km_value
    Km.unit    <- rv.UNITS$units.selected$For.Var
    Km.b.u     <- rv.UNITS$units.base$For.Var
    Km.unit.d  <- paste0("conc (",input$GO_species_unit_choice, ")")
    
    Km.d <- paste0("Michaelis Menten constant for the ", 
                   "facilitated Diffusion of ",
                   species.out,
                   " to ",
                   species.in)
    
    # Base value determination
    if (Km.unit != Km.b.u) {
      Km.b.v <- UnitConversion(u.d, Km.unit, Km.b.u, as.numeric(Km.val))
    } else {
      Km.b.v <- Km.val
    }
    
    log       <- paste0("Facilated Diffusion of ",
                        species.out,
                        " to ",
                        species.in,
                        " from compartment ",
                        compartment.out, " to ", compartment.in)
    
    params          <- c(params, Vmax.var, Km.var)
    param.descript  <- c(param.descript, Vmax.d, Km.d)
    param.vals      <- c(param.vals, Vmax.val, Km.val)
    param.units     <- c(param.units, Vmax.unit, Km.unit)
    unit.descript   <- c(unit.descript, Vmax.u.d, Km.unit.d)
    base.units      <- c(base.units, Vmax.b.u, Km.b.u)
    base.vals       <- c(base.vals, Vmax.b.v, Km.b.v)
    
    laws <- FacilitatedDiffusion(species.out, Vmax.var, Km.var)
    
  }
  # browser()
  ## Store/Error Check ---------------------------------------------------------
  # error.check <- CheckParametersForErrors(params, 
  #                                         rv.SPECIES$species.names,
  #                                         names(rv.PARAMETERS$parameters),
  #                                         allowRepeatParams = TRUE)
  # 
  # passed.error.check <- error.check[[1]]
  # param.already.defined <- error.check[[2]]
  
  passed.error.check <- TRUE
  for (i in seq_along(params)) {
    par.error.DS <- list("Name" = params[i],
                         "UnitDescription" = unit.descript[i])
    error.check <- CheckParametersForErrors(par.error.DS,
                                            rv.SPECIES$species,
                                            rv.PARAMETERS$parameters,
                                            rv.COMPARTMENTS$compartments)
    passed.check <- error.check[[1]]
    param.already.defined <- error.check[[2]]
    # Break loop and return error message if parameter fails check
    if (!passed.check) {passed.error.check <- FALSE}
  }
  
  if (passed.error.check) {
    # Create InputOutput ID
    ids <- GenerateId(rv.ID$id.io.seed, "IO")
    IO.id <- ids$id
    rv.ID$id.io.seed <- ids$seed
    par.ids <- c()
    for (i in seq(length(params))) {
      if (params[i] %in% rv.PARAMETERS$parameters.names) {
        #APPEND
        
        # Find parameter id
        par.id <- FindId(params[i])
        par.ids <- c(par.ids, par.id)
        
        type.old  <- SplitEntry(rv.PARAMETERS$parameters[[par.id]]$Type)
        type.note <- SplitEntry(rv.PARAMETERS$parameters[[par.id]]$Type.Note)
        used.in   <- SplitEntry(rv.PARAMETERS$parameters[[par.id]]$Used.In)
        is.custom <- rv.PARAMETERS$parameters[[par.id]]$Custom
        old.par.des <- 
          SplitEntry(rv.PARAMETERS$parameters[[par.id]]$Description)
        
        new.type      <- collapseVector(c(type.old, "Input/Output"))
        new.type.note <- collapseVector(c(type.note, type))
        new.used.in   <- collapseVector(c(used.in, IO.id))
        new.par.des   <- collapseVector(c(old.par.des, param.descript[i]))
        
        # Write out to parameter
        to.par.list <- list("Name"            = params[i],
                            "ID"              = par.id,
                            "Value"           = as.numeric(param.vals[i]),
                            "Unit"            = param.units[i],
                            "UnitDescription" = unit.descript[i],
                            "BaseUnit"        = base.units[i],
                            "BaseValue"       = as.numeric(base.vals[i]),
                            "Description"     = new.par.des,
                            "Type"            = new.type,
                            "Type.Note"       = new.type.note,
                            "Used.In"         = new.used.in,
                            "Custom"          = is.custom
        )
        
        # Append parameter entry
        rv.PARAMETERS$parameters[[par.id]] <- to.par.list
        
      } else {
        # Generate Parameter ID
        par.gen <- GenerateId(rv.ID$id.param.seed, "parameter")
        rv.ID$id.param.seed <- par.gen$seed
        par.id <- par.gen$id
        par.ids <- c(par.ids, par.id)
        # Store ID to database
        idx.to.add <- nrow(rv.ID$id.df) + 1
        rv.ID$id.df[idx.to.add, ] <- c(par.id, params[i])
        
        # Write out to parameter
        to.par.list <- list("Name"            = params[i],
                            "ID"              = par.id,
                            "Value"           = as.numeric(param.vals[i]),
                            "Unit"            = param.units[i],
                            "UnitDescription" = unit.descript[i],
                            "BaseUnit"        = base.units[i],
                            "BaseValue"       = as.numeric(base.vals[i]),
                            "Description"     = param.descript[i],
                            "Type"            = "Input/Output",
                            "Type.Note"       = type,
                            "Used.In"         = IO.id,
                            "Custom"          = FALSE)
        
        # Store to parameter list
        rv.PARAMETERS$parameters[[par.id]] <- to.par.list
      }
    }
    # browser()
    
    
    # Extract reaction laws 
    rate.law    <- laws$string
    p.rate.law  <- laws$pretty.string
    latex.law   <- laws$latex
    mathjax.law <- laws$mj
    mathml.law  <- laws$mathml
    
    
    s.id.all <- c(species.out.id, species.in.id)
    s.id.all <- s.id.all[complete.cases(s.id.all)]
    
    c.id.all <- c(compartment.out.id, compartment.in.id)
    c.id.all <- c.id.all[complete.cases(c.id.all)]
    
    # Collapse variables
    compartment.out.collapsed  <- paste0(compartment.out, collapse = ", ")
    compartment.in.collapsed   <- paste0(compartment.in, collapse = ", ")
    species.out.collapsed      <- paste0(species.out, collapse = ", ")
    species.in.collapsed       <- paste0(species.in, collapse = ", ")
    par.collapsed              <- paste0(params, collapse = ", ")
    
    # Collapse Id Vars
    compartment.in.id.collapsed  <- paste0(compartment.in.id, collapse = ", ")
    compartment.out.id.collapsed <- paste0(compartment.out.id, collapse = ", ")
    par.ids.collapsed            <- paste0(par.ids, collapse = ", ")
    s.id.collapsed               <- paste0(s.id.all, collapse = ", ")
    species.in.id.collapsed      <- paste0(species.in.id, collapse = ", ")
    species.out.id.collapsed     <- paste0(species.out.id, collapse = ", ")
    comp.ids.collapsed           <- paste0(c.id.all, collapse = ", ")
    # browser()
    
    # TODO: Store I/0 to species ids (IO.ids)
    # Check to make sure there is a species id
    if (isTruthy(s.id.all)) {
      # Loop through species id to begin addition
      for (i in seq_along(s.id.all)) {
        # Check that the species id has IO.ids already or if its NA
        if (is.na(rv.SPECIES$species[[s.id.all[i]]]$IO.ids)) {
          # If its NA, make current id  the id
          rv.SPECIES$species[[s.id.all[i]]]$IO.ids <- IO.id
        } else {
          # Else paste0 collapse current id with ", "
          items <- 
            strsplit(
              rv.SPECIES$species[[s.id.all[i]]]$IO.ids, ", ")[[1]]
          items <- c(items, IO.id)
          rv.SPECIES$species[[s.id.all[i]]]$IO.ids <- 
            paste0(items, collapse = ", ")
        }
      }
    }
    
    # Create Overall lists
    to.list <- list("ID"               = IO.id,
                    "Direction"        = direction,
                    "Type"             = type,
                    "Compartment.Out"  = compartment.out.collapsed,
                    "Compartment.In"   = compartment.in.collapsed,
                    "Species.Out"      = species.out.collapsed,
                    "Species.In"       = species.in.collapsed,
                    "Species.Out.Ids"  = species.out.id.collapsed,
                    "Species.In.Ids"   = species.in.id.collapsed,
                    "Parameters"       = par.collapsed,
                    "Compartment.Ids"  = comp.ids.collapsed,
                    "Species.Ids"      = s.id.collapsed, 
                    "Parameter.Ids"    = par.ids.collapsed,
                    "Equation.Text"    = NA,
                    "Equation.Latex"   = NA,
                    "Equation.MathJax" = NA,
                    "String.Rate.Law"  = rate.law,
                    "Pretty.Rate.Law"  = p.rate.law,
                    "Latex.Rate.Law"   = latex.law,
                    "MathJax.Rate.Law" = mathjax.law,
                    "MathMl.Rate.Law"  = mathml.law,
                    "Reversible"       = is.reversible,
                    "Description"      = log
    )
    
    rv.IO$InputOutput[[IO.id]] <- to.list
    
    
    # Create Individual Lists Depending on IO type
    if (input$CIO_IO_options == "FLOW_IN") {
      to.add <- list("ID"             = IO.id,
                     "Compartment"    = compartment.in.collapsed,
                     "Compartment.Id" = comp.ids.collapsed,
                     "Species"        = species.in.collapsed,
                     "Species.Id"     = s.id.collapsed,
                     "Flow.Parameter" = par.collapsed,
                     "Parameter.Id"   = par.ids.collapsed)
      
     rv.IO$Flow.In[[IO.id]] <- to.add 
     
    } else if (input$CIO_IO_options == "FLOW_OUT") {
      to.add <- list("ID"             = IO.id,
                     "Compartment"    = compartment.in.collapsed,
                     "Compartment.Id" = comp.ids.collapsed,
                     "Species"        = species.in.collapsed,
                     "Species.Id"     = s.id.collapsed,
                     "Flow.Parameter" = par.collapsed,
                     "Parameter.Id"   = par.ids.collapsed)
      
      rv.IO$Flow.Out[[IO.id]] <- to.add
      
    } else if (input$CIO_IO_options == "FLOW_BETWEEN") {
      # Break par.ids
      flow.out.id <- par.ids[1]
      flow.in.id <- paste0(par.ids[seq(2, n.split+1)], collapse = ", ")
      flow.out <- f.out
      flow.in <- paste0(f.in, collapse = ", ")
      
      to.add <- list("ID" = IO.id,
                     "n.Split" = n.split,
                     "Compartment.Out"    = compartment.out.collapsed,
                     "Compartment.Out.Id" = compartment.out.id.collapsed,
                     "Compartment.In"     = compartment.in.collapsed,
                     "Compartment.In.Id"  = compartment.in.id.collapsed,
                     "Speciespecies.out"        = species.out.collapsed,
                     "Speciespecies.out.Id"     = species.out.id.collapsed,
                     "Speciespecies.in"         = species.in.collapsed,
                     "Speciespecies.in.Id"      = species.in.id.collapsed,
                     "Flow.out"           = flow.out,
                     "Flow.in"            = flow.in,
                     "Flow.out.id"        = flow.out.id,
                     "Flow.in.id"         = flow.in.id)
      
      rv.IO$Flow.Between[[IO.id]] <- to.add 
      
    } else if (input$CIO_IO_options == "CLEARANCE") {
      to.add <- list("ID"             = IO.id,
                     "Compartment"    = compartment.in.collapsed,
                     "Compartment.Id" = comp.ids.collapsed,
                     "Species"        = species.in.collapsed,
                     "Species.Id"     = s.id.collapsed,
                     "Flow.Parameter" = par.collapsed,
                     "Parameter.Id"   = par.ids.collapsed)
      
      rv.IO$Clearance[[IO.id]] <- to.add 
      
    } else if (input$CIO_IO_options == "SIMPDIFF") {
      to.add <- list("ID"               = IO.id,
                     "Compartment.1"    = compartment.out,
                     "Compartment.1.Id" = compartment.out.id,
                     "Compartment.2"    = compartment.in,
                     "Compartment.2.Id" = compartment.in.id,
                     "Species.1"        = species.out,
                     "Species.1.Id"     = species.out.id,
                     "Species.2"        = species.in,
                     "Species.2.Id"     = species.in.id,
                     "PS"               = par.collapsed,
                     "PS.id"            = par.ids.collapsed)
      
      rv.IO$Simple.Diffusion[[IO.id]] <- to.add 
      
    } else if (input$CIO_IO_options == "FACILITATED_DIFF") {
      to.add <- list("ID" = IO.id,
                     "Compartment.Out"    = compartment.out,
                     "Compartment.Out.Id" = compartment.out.id,
                     "Compartment.In"     = compartment.in,
                     "Compartment.In.Id"  = compartment.in.id,
                     "Speciespecies.out"        = species.out,
                     "Speciespecies.out.Id"     = species.out.id,
                     "Speciespecies.in"         = species.in,
                     "Speciespecies.in.Id"      = species.in.id,
                     "Vmax"               = Vmax.var,
                     "Km"                 = Km.var,
                     "Vmax.id"            = par.ids[1],
                     "Km.id"              = par.ids[2])
      
      rv.IO$Facilitated.Diffusion[[IO.id]] <- to.add
    }
    
    
    # Success Modal
    # sendSweetAlert(
    #   session = session,
    #   title = "Success !!",
    #   text = paste0("Succesffuly added ", type),
    #   type = "success"
    # )
    # 
    
    # Generate/Regenerate Differential Equations
    solveForDiffEqs()
    
    rv.IO$IO.logs[length(rv.IO$IO.logs) + 1] <- log
    
    rv.IO$IO.id.counter <- rv.IO$IO.id.counter + 1
    
    # Flow In Rate Value
    updateTextInput(
      session = session, 
      inputId = "CIO_flow_in_rate_constant",
      value = paste0("F_in_", rv.IO$IO.id.counter)
    )
    
    # Flow Out Rate Value
    updateTextInput(
      session = session, 
      inputId = "CIO_flow_out_rate_constant",
      value = paste0("F_out_", rv.IO$IO.id.counter)
    )
    
    # Flow Between - Out
    updateTextInput(
      session = session, 
      inputId = "CIO_flowbetween_flow_variable_out",
      value = paste0("F_out_", rv.IO$IO.id.counter)
    )
    
    # Flow Between - In
    updateTextInput(
      session = session, 
      inputId = "CIO_flowbetween_flow_variable_in_1",
      value = paste0("F_in_", rv.IO$IO.id.counter)
    )
    
    # Clearance Rate Value
    updateTextInput(
      session = session, 
      inputId = "CIO_clearance_rate_constant",
      value = paste0("ke_", rv.IO$IO.id.counter)
    )
    
    # Simple Diffusion PS Value
    updateTextInput(
      session = session, 
      inputId = "CIO_simpdiff_rate_constant",
      value = paste0("PS_", rv.IO$IO.id.counter)
    )
    
    # Facilitated Diffusion Vmax Value
    updateTextInput(
      session = session, 
      inputId = "CIO_facilitatedDiff_Vmax_value",
      value = 0
    )
    
    # Facilitated Diffusion Km Value
    updateTextInput(
      session = session, 
      inputId = "CIO_facilitatedDiff_Km_value",
      value = 0
    )
    
    
    # Close Modal Logic
    if (!input$checkbox_modal_io_keep_open) {
      toggleModal(
        session = session, 
        modalId = "modal_add_IO",
        toggle = "close"
      )
    }
  }
  
  watier.IO$hide()
  
  shinyjs::enable("CIO_add_IO")
  
})


# Event Change: IO#rv.IO$InputOutput
observeEvent(rv.IO$InputOutput, {
  rv.IO$IO.df <- bind_rows(rv.IO$InputOutput)
  
  updatePickerInput(
    session = session,
    inputId = "PI_delete_select_io",
    choices = seq(length(rv.IO$InputOutput))
  )
})

# Delete IO Button -------------------------------------------------------------
observeEvent(input$modal_delete_io_button, {
  # Remove Input/Output Reaction from model
  # This event will clear the selected IO modules and remove them from the 
  # rv.IO datastructures. Additionally, it will check each parameter that is 
  # used in these modules and see if they are used elsewhere. If they are not, 
  # then those parameters will be removed from the model. Lastly, it will 
  # remove the ID for the remove modules from the species that use it. 

  # Grab IO reactions to remove
  IOs.to.delete <- as.numeric(input$PI_delete_select_io)
  io.ids <- rv.IO$IO.df$ID[IOs.to.delete]
  
  # REMOVE ASSOCIATED SPECIES
  for (id in io.ids) {
    species.to.check <- rv.IO$InputOutput[[id]]$Species.Ids
    species.to.check <- strsplit(species.to.check, ", ")[[1]]
    for (spec.id in species.to.check) {
      # Find the species entry
      entry <- rv.SPECIES$species[[spec.id]]
      IO.ids <- strsplit(entry$IO.ids, ", ")[[1]]
      new.ids <- IO.ids[!IO.ids == id]
      if (isTruthy(new.ids)) {
        rv.SPECIES$species[[spec.id]]$IO.ids <- paste0(new.ids, collapse = ", ")
      } else {
        rv.SPECIES$species[[spec.id]]$IO.ids <- NA
      }
    }
  }
  
  
  # REMOVE ASSOCIATED PARAMETERS
  # For each IO grab parameters in used in reaction
  for (id in io.ids) {
    pars.to.check <- rv.IO$InputOutput[[id]]$Parameter.Ids
    pars.to.check <- strsplit(pars.to.check, ", ")[[1]]
    for (par.id in pars.to.check) {
      # For each parameter id, pull where its used ($Used.In)
      cur.par <- rv.PARAMETERS$parameters[[par.id]]
      # Grab where its used
      used.in <- strsplit(cur.par$Used.In, ", ")[[1]]
      # Remove Current IO from cur.par
      new.used.in <- used.in[!used.in == id]
      
      # Check length of new.used in
      if (isTruthy(new.used.in)) {
        new.used.in <- paste0(used.in[!used.in == id], collapse = ", ")
        rv.PARAMETERS$parameters[[par.id]]$Used.In <- new.used.in
      } else {
        # Remove Parameter from parameter table (and id database?)
        rv.PARAMETERS$parameters[[par.id]] <- NULL 
        # Remove From ID Database
        row.idx <- which(rv.ID$id.df[, 1] %in% par.id)
        rv.ID$id.df <- rv.ID$id.df[-row.idx, ]
      }
    }
  }
  
  # REMOVE SUB IO RV LISTS
  for (id in io.ids) {
    # Pull subtype
    sub.type <- rv.IO$InputOutput[[id]]$Type
    # Remove appropriate subtype from model
    switch (sub.type,
            FLOW_IN          = {rv.IO$Flow.In[[id]] <- NULL},
            FLOW_OUT         = {rv.IO$Flow.Out[[id]] <- NULL},
            FLOW_BETWEEN     = {rv.IO$Flow.Between[[id]] <- NULL},
            CLEARANCE        = {rv.IO$Clearance[[id]] <- NULL},
            SIMPDIFF         = {rv.IO$Simple.Diffusion[[id]] <- NULL},
            FACILITATED_DIFF = {rv.IO$Facilitated.Diffusion[[id]] <- NULL},
    )
  }
  
  # REMOVE INPUT/OUTPUT FROM MODEL
  for (i in io.ids) {
    rv.IO$InputOutput[[i]] <- NULL
  }
  
  # Recalculate Differential Equations
  solveForDiffEqs()
  
  if (input$checkbox_modal_delete_io_keep_modal_active) {
    toggleModal(session,
                "modal_delete_io",
                toggle = "close")
  }
  
})

output$deleteIO_table_viewer <- renderRHandsontable({
  
  io.num <- as.numeric(input$PI_delete_select_io)
  myindex = io.num - 1
  
  to.show <- rv.IO$IO.df %>%
    select(Type, 
           Compartment.Out, 
           Compartment.In, 
           Species.Out, 
           Species.In,
           Parameters)
  
  colnames(to.show) <- c("Type",
                         "Compartment Out",
                         "Compartment In",
                         "Species Out",
                         "Species In",
                         "Parameters")
  

  rhandsontable(to.show,
                myindex = myindex) %>%
    hot_cols(renderer = 
               "function(instance, td, row, col, prop, value, cellProperties) {
       Handsontable.renderers.TextRenderer.apply(this, arguments);
       if (instance.params) {
       mhrows = instance.params.myindex;
       mhrows = mhrows instanceof Array ? mhrows : [mhrows];
       }
       if (instance.params && mhrows.includes(row)) td.style.background = '#FFCCCB';
      }"
    )
})


# Logs -------------------------------------------------------------------------
output$CIO_IO_Logs <- renderText({
  
  if (length(rv.IO$IO.logs) < 1) {
    "Output Logs will appear here."
  } else {
    paste0("(", 
           seq(length(rv.IO$IO.logs)),
           ") ",
           rv.IO$IO.logs, 
           collapse = "<br>")
  }
})

# Table Render: IO -------------------------------------------------------------
output$createModel_IO_logs_table <- renderRHandsontable(
  
  if (length(rv.IO$IO.df) == 0) {
    temp <- data.frame(c("Logs for Input/Output will appear here."))
    temp <- transpose(temp)
    colnames(temp) <- c("Instructions")
    rhandsontable(temp,
                  rowHeaders = NULL,
                  colHeaderWidth = 100,
                  stretchH = "all",
                  readOnly = TRUE
    )
  } else {
    to.show <- rv.IO$IO.df %>%
      select(Type, 
             Compartment.Out, 
             Compartment.In, 
             Species.Out, 
             Species.In,
             Parameters)
    
    colnames(to.show) <- c("Type",
                           "Compartment Out",
                           "Compartment In",
                           "Species Out",
                           "Species In",
                           "Parameters")
    
    rhandsontable(to.show, 
                  width = "100%",
                  readOnly = TRUE,
                  stretchH = "all",
                  fillHandle = FALSE) %>%
      hot_cols(
        manualColumnMove = FALSE,
        manualColumnResize = TRUE,
        halign = "htCenter",
        valign = "htMiddle",
        renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (row % 2 == 0) {
              td.style.background = '#f9f9f9';
             } else {
              td.style.background = 'white';
             };
           }"
      ) %>%
      hot_context_menu(
        allowRowEdit = FALSE,
        allowColEdit = FALSE
      )
  }
)
