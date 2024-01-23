
observeEvent(input$bttn_debug_reset_vars, {
  print("Reseting all variables in model")
  reset_all_storage_variables()
})

observeEvent(input$bttn_debug_display_units, {
  print(rv.UNITS$units.selected)
  print("choices")
  print(rv.UNITS$units.choices)
  print("base")
  print(rv.UNITS$units.base)
})

observeEvent(input$bttn_debug_reset_diff_mathjax, {
  
  # loop through reaction structure
  for (i in seq_along(rv.REACTIONS$reactions)) {
    # pull string laws
    str.law <- rv.REACTIONS$reactions[[i]]$String.Rate.Law
    # solve for latex and mathjax
    converted.laws <- ConvertRateLaw(str.law)
    latex.law <- converted.laws$latex
    mathjax.law <- converted.laws$mathjax
    # restore
    rv.REACTIONS$reactions[[i]]$Latex.Rate.Law <- latex.law
    rv.REACTIONS$reactions[[i]]$MathJax.Rate.Law <- mathjax.law
  }

})

# View Variables ---------------------------------------------------------------
observeEvent(input$debug_view_variables, {

  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall (Species)")
  )
  rv.DEBUG$button_pressed_last <- "Species"
  
})

observeEvent(input$debug_view_compartments, {
  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall (Compartments)")
  )
  rv.DEBUG$button_pressed_last <- "Compartments"
})

observeEvent(input$debug_view_equations, {
  
  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall", 
                "Mass Action",
                "Regulated Mass Action",
                "Synthesis",
                "Degradation (Rate)", 
                "Degradation (By Enzyme)", 
                "Michaelis Menten"),
    selected = "Overall"
  )
  rv.DEBUG$button_pressed_last <- "Equations"
})

observeEvent(input$debug_view_ids, {
  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall (IDs)")
  )
  rv.DEBUG$button_pressed_last <- "Ids"
})

# Debug Input Output
observeEvent(input$debug_view_IO, {
  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall (IO)",
                "Flow.In",
                "Flow.Out",
                "Flow.Between",
                "Clearance",
                "Simple.Diffusion",
                "Facilitated.Diffusion")
  )
  rv.DEBUG$button_pressed_last <- "IO"
})

observeEvent(input$debug_view_parameters, {
  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall (Parameters)")
  )
  rv.DEBUG$button_pressed_last <- "Parameters"
  
  print(rv.PARAMETERS$parameters)

})

observeEvent(input$debug_view_differential_eqns, {
  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall (DEQs)")
  )
  rv.DEBUG$button_pressed_last <- "DEQs"
})

observeEvent(input$debug_view_custom_laws, {
  updatePickerInput(
    session = session,
    inputId = "debug_filter_searchType",
    choices = c("Overall (Custom Laws)")
  )
  rv.DEBUG$button_pressed_last <- "CustomLaw"
})

observeEvent(input$debug_view_custom_eqns, {
  updatePickerInput(
    session = session, 
    inputId = "debug_filter_searchType",
    choices =  c("Overall (Custom Eqns)")
  )
  rv.DEBUG$button_pressed_last <- "CustomEqn"
})


observeEvent(input$debug_filter_searchType, {
  print(input$debug_filter_searchType)
  if (rv.DEBUG$button_pressed_last == "Equations") {
    if (input$debug_filter_searchType == "Overall") {
      rv.LOGS$variable.debug.button <- print(rv.REACTIONS$reactions)
      rv.LOGS$variable.debug.table  <- rv.REACTIONS$reactions.df
    } else if (input$debug_filter_searchType == "Mass Action") {
      rv.LOGS$variable.debug.button <- print(rv.REACTIONS$massAction)
      rv.LOGS$variable.debug.table  <- rv.REACTIONS$massAction.df
    } else if (input$debug_filter_searchType == "Synthesis") {
      rv.LOGS$variable.debug.button <- print(rv.REACTIONS$synthesis)
      rv.LOGS$variable.debug.table  <- rv.REACTIONS$synthesis.df
    } else if (input$debug_filter_searchType == "Regulated Mass Action") {
      rv.LOGS$variable.debug.button <- print(rv.REACTIONS$massActionwReg)
      rv.LOGS$variable.debug.table  <- rv.REACTIONS$massActionwReg.df
    } else if (input$debug_filter_searchType == "Degradation (Rate)") {
      rv.LOGS$variable.debug.button <- print(rv.REACTIONS$degradation.by.rate)
      rv.LOGS$variable.debug.table  <- rv.REACTIONS$degradation.by.rate.df
    } else if (input$debug_filter_searchType == "Degradation (By Enzyme)") {
      rv.LOGS$variable.debug.button <- print(rv.REACTIONS$degradation.by.enzyme)
      rv.LOGS$variable.debug.table  <- rv.REACTIONS$degradation.by.enzyme.df
    } else if (input$debug_filter_searchType == "Michaelis Menten") {
      rv.LOGS$variable.debug.button <- print(rv.REACTIONS$michaelisMenten)
      rv.LOGS$variable.debug.table  <- rv.REACTIONS$michaelisMenten.df
    }
  } 
  else if (rv.DEBUG$button_pressed_last == "Compartments") {
    rv.LOGS$variable.debug.button <- print(rv.COMPARTMENTS$compartments)
    rv.LOGS$variable.debug.table  <- rv.COMPARTMENTS$compartments.df
  } 
  else if (rv.DEBUG$button_pressed_last == "Species") {
    rv.LOGS$variable.debug.button <- print(rv.SPECIES$species)
    rv.LOGS$variable.debug.table  <- rv.SPECIES$species.df
  } 
  else if (rv.DEBUG$button_pressed_last == "Ids") {
    rv.LOGS$variable.debug.button <- print(rv.ID$id.df)
    rv.LOGS$variable.debug.table  <- rv.ID$id.df
  } 
  else if (rv.DEBUG$button_pressed_last == "IO") {
    if (input$debug_filter_searchType == "Overall (IO)") {
      rv.LOGS$variable.debug.button <- print(rv.IO$InputOutput)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    } else if (input$debug_filter_searchType == "Flow.In") {
      rv.LOGS$variable.debug.button <- print(rv.IO$Flow.In)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    } else if (input$debug_filter_searchType == "Flow.Out") {
      rv.LOGS$variable.debug.button <- print(rv.IO$Flow.Out)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    } else if (input$debug_filter_searchType == "Flow.Between") {
      rv.LOGS$variable.debug.button <- print(rv.IO$Flow.Between)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    } else if (input$debug_filter_searchType == "Clearance") {
      rv.LOGS$variable.debug.button <- print(rv.IO$Clearance)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    } else if (input$debug_filter_searchType == "Simple.Diffusion") {
      rv.LOGS$variable.debug.button <- print(rv.IO$Simple.Diffusion)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    } else if (input$debug_filter_searchType == "Facilitated.Diffusion") {
      rv.LOGS$variable.debug.button <- print(rv.IO$Facilitated.Diffusion)
      rv.LOGS$variable.debug.table  <- rv.IO$IO.df
    }
  } 
  else if (rv.DEBUG$button_pressed_last == "Parameters") {
    rv.LOGS$variable.debug.button <- print(rv.PARAMETERS$parameters)
    rv.LOGS$variable.debug.table  <- rv.PARAMETERS$parameters.df
  }
  else if (rv.DEBUG$button_pressed_last == "DEQs") {
    rv.LOGS$variable.debug.button <- print(rv.DE$de.equations.list)
    rv.LOGS$variable.debug.table  <- bind_rows(rv.DE$de.equations.list)
  }
  else if (rv.DEBUG$button_pressed_last == "CustomLaw") {
    rv.LOGS$variable.debug.button <- print(rv.CUSTOM.LAWS$cl.reaction)
    rv.LOGS$variable.debug.table  <- bind_rows(rv.CUSTOM.LAWS$cl.reaction)
    print(rv.REACTIONLAWS$laws)
  }
  else if (rv.DEBUG$button_pressed_last == "CustomEqn") {
    rv.LOGS$variable.debug.button <- print(rv.CUSTOM.EQNS$ce.equations)
    rv.LOGS$variable.debug.table  <- bind_rows(rv.CUSTOM.EQNS$ce.equations)
  }
  
})


output$debug_text_view <- renderPrint(
  print(rv.LOGS$variable.debug.button)
)

output$debug_table_view <- renderRHandsontable(
  rhandsontable(rv.LOGS$variable.debug.table,
                width = "100%",
                readOnly = TRUE)
)
