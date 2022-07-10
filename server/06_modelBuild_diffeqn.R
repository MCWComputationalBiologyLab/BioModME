############################## DiffEQ Server #################################
solveForDiffEqs <- function() {
  # jPrint("Solving For differential Equations")
  # jPrint(eqns$eqn.info)
  # jPrint(vars$species)
  # jPrint(IO$input.info)
  # jPrint(IO$output.info)
  # jPrint(IO$bool.input.added)
  # jPrint(IO$bool.output.added)
  results <- calc_differential_equations(eqns$eqn.info,
                                         eqns$eqn.chem,
                                         eqns$eqn.enzyme,
                                         eqns$eqn.syn,
                                         eqns$eqn.deg,
                                         vars$species, 
                                         IO$input.info, 
                                         IO$output.info,
                                         IO$bool.input.added,
                                         IO$bool.output.added,
                                         DE$custom.diffeq.var,
                                         input$diffeq_multi_custom_eqns,
                                         DE$custom.diffeq.df)
  DE$eqns <- unlist(results["diff.eqns"])
  DE$eqns.in.latex <- unlist(results["latex.diff.eqns"])
}

observeEvent(vars$species, {
  picker.choices <- c()
  i = 0
  jPrint(vars$species)
  for (var in vars$species) {
    i = i + 1
    choice <- paste0(i, ") ", 'd(', var, ")/dt")
    picker.choices <- c(picker.choices, choice)
  }
  updatePickerInput(session, "diffeq_var_to_custom", choices = picker.choices)
})

observeEvent(DE$custom.diffeq.var, {
  picker.choices <- DE$custom.diffeq.var
  updatePickerInput(session, "diffeq_multi_custom_eqns", choices = picker.choices)
})

observeEvent(input$diffeq_custom_eqn_button, {
  new.eqn <- input$diffeq_custom_eqn
  idx <- as.numeric(strsplit(input$diffeq_var_to_custom, ")")[[1]][1])

  DE$eqns[idx] <- new.eqn
  DE$custom.diffeq.var <- c(DE$custom.diffeq.var, vars$species[idx])
  DE$custom.diffeq <- c(DE$custom.diffeq, new.eqn)
  DE$custom.diffeq.df[nrow(DE$custom.diffeq.df)+1, ] <- c(vars$species[idx], new.eqn)
  jPrint(DE$custom.diffeq.df)
})

observeEvent(input$diffeq_generate_equations, {
  solveForDiffEqs()
})

output$diffeq_display_diffEqs <- renderText({
  # paste(paste0('d(', vars$species, ")/dt = ", DE$eqns), collapse="<br><br>")
  
  if (length(vars$species) == 0) {
    "No variables entered"
  }
  else {
    n_eqns = length(vars$species)
    eqns_to_display <- c()
    for (i in seq(n_eqns)) {
      if (input$diffeq_option_simplify) {
        new_eqn <- paste0("(",i, ") ", 'd(', vars$species[i], ")/dt = ", Deriv::Simplify(DE$eqns[i]))
      } else {
        new_eqn <- paste0("(",i, ") ", 'd(', vars$species[i], ")/dt = ", DE$eqns[i])
      }
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br><br>")
  }
})

