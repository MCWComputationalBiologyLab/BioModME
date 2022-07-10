
# mass action 
output$mathjax_MA <- renderUI({
  "This page follows the law of mass action for chemical reactions."
  
  withMathJax(

    "$$\\ce{ A <=> B}$$",
    "$$A \\ce{<=>}GH$$",
    paste0("$$\\ce{", "Bean3w", " + ", "A", "<->[{c23bg2h}][{text below}]", "C25", "}$$"),
    paste0("$$", "Bean3w", " + ", "A", "\\ce{", "<->[{$c23bg2h$}][{$text below$}]", "}", "C25", "$$"),
    paste0("$$", "Bean3w", " + ", "A", "\\ce{", "<->}[{$c23bg2h$}][{$text below$}]", "C25", "$$")
    
  )
})

# mass action with regulation
output$mathjax_MA_with_regulators <- renderUI({
  "This page follows mass action regulation in chemical reactions."
})

# enzyme - michelis menten
output$enzyme_MM <- renderUI({
  if (input$eqn_options_enzyme_useVmax) {
    withMathJax(
      # sprintf('The resulting enzyme kinetic reaction
      #            $$\\frac{d}{dt} = \\frac{V_{max}S}{K_{m} + S}
      #         = %s$$', input$eqn_enzyme_Vmax))
      tags$b("The resulting enzyme kinetic reaction:"),
      br(),
      paste0("$$\\frac{d}{dt} = \\frac{V_{max}S}{K_{m} + S} => $$ ", 
             sprintf("$$\\frac{%s*%s}{%s + %s}$$",
                     input$eqn_enzyme_Vmax, 
                     input$eqn_enzyme_substrate, 
                     input$eqn_enzyme_Km, 
                     input$eqn_enzyme_substrate)),
      br()
    )
  } else {
    withMathJax(
      # sprintf('The resulting enzyme kinetic reaction
      #            $$\\frac{d}{dt} = \\frac{V_{max}S}{K_{m} + S}
      #         = %s$$', input$eqn_enzyme_Vmax))
      tags$b("The resulting enzyme kinetic reaction:"),
      br(),
      paste0("$$\\frac{d}{dt} = \\frac{k_{cat}*E*S}{K_{m} + S} = $$ ", 
             sprintf("$$\\frac{%s*%s*%s}{%s + %s}$$",
                     input$eqn_enzyme_kcat,
                     input$eqn_enzyme_enzyme, 
                     input$eqn_enzyme_substrate, 
                     input$eqn_enzyme_Km, 
                     input$eqn_enzyme_substrate)),
      br(),
      "where $$\\begin{aligned} S &= Substrate, \\\\
              E &= Enzyme, \\\\
              K_{m} &= Michelis Menton Constant
              \\end{aligned}$$"
    )
  }
  
})