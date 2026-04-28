# Renders the "About" tab of the equation builder modal.
# Switches on input$eqnCreate_reaction_law and looks up educational content
# from the equation_about_content registry (see 02_equations_about_content.R).

output$equationBuilder_about <- renderUI({
  law <- input$eqnCreate_reaction_law
  type_of_eqn <- input$eqnCreate_type_of_equation

  is_custom <- isTRUE(type_of_eqn %in% c("rate_eqn", "time_dependent")) ||
    isTRUE(law == "create_custom") ||
    (isTRUE(nzchar(law)) && startsWith(law, "user_custom_law_"))

  entry_key <- if (is_custom) "CUSTOM_EQUATION" else law
  entry <- equation_about_content[[entry_key]]

  if (is.null(entry)) {
    return(
      div(
        style = "padding: 12px;",
        tags$em(
          "No description available for this rate law yet. ",
          "Use the Description tab to add your own notes."
        )
      )
    )
  }

  withMathJax(
    div(
      style = "padding: 6px 4px;",
      h4(entry$display_name),
      hrTitle("Math", position = "left"),
      div(
        style = "padding: 8px 0;",
        HTML(paste0("$$", entry$math, "$$"))
      ),
      hrTitle("Biology", position = "left"),
      HTML(entry$biology),
      hrTitle("Purpose", position = "left"),
      HTML(entry$purpose),
      hrTitle("Use Cases", position = "left"),
      HTML(entry$use_cases)
    )
  )
})
