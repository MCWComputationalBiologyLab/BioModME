
# Equation Summary -------------------------------------------------------------
output$ReactionEquationsBox <- renderUI({
  
  # Check overflow
  overflow.type <- ifelse(input$CI_summary_hide_scrollbars, "hidden", "auto")
  # box.title = paste0("<font-s")
  # Get reactions to show
  reactions.mj <- unname(sapply(rv.REACTIONS$reactions,
                                get,
                                x = "Equation.MathJax"))
  title.style = paste0("font-size:",
                       as.character(input$NI_summary_box_title_font_size),
                       "px; font-weight: bold; margin-bottom: 0px;")
  
  text.size <- input$NI_summary_reactions_mathjax_font_size
  box(
    id = "box_summary_reactions",
    width = 12,
    # title = HTML("<font-size=42px><b>Reactions</b></font size>"),
    title = tags$p("Reactions", style = title.style),
    # title = h3("Reactions", style = "font-size:42px; font-weight:bold"),
    div(
      style = paste0("height:370px; overflow-y:", overflow.type, ";"),
      lapply(seq_along(reactions.mj), function(i){
        div(
          style = paste0("overflow-x:", overflow.type, ";"),
          withMathJax(
            paste0("$$(", i, ") \\: \\:", substr(reactions.mj[i], 
                                               3, 
                                               nchar(reactions.mj[i])
                                               )
                   )
          )
        )
      })  
    ),
    tags$head(
      tags$style(
        paste0("#box_summary_reactions .MathJax_Display {font-size:",
               text.size,
               "%;}")
      )
    )
  )
})

output$summary_reaction_equations <- renderText({
  # Grab the reactiosn
  to.display <- unname(sapply(rv.REACTIONS$reactions,
                              get,
                              x = "Equation.Text"))
  
  if (length(to.display) == 0) {
    paste("No equations entered")
  } else {
    eqns_to_display <- c()
    for (i in seq_along(to.display)) {
      new_eqn <- paste0("(",i, ") ", to.display[i])
      eqns_to_display <- c(eqns_to_display, new_eqn)
    }
    paste(eqns_to_display, collapse = "<br><br>")
  }
})

# Differential Equations Summary -----------------------------------------------
output$DifferentialEquationsBox <- renderUI({
  
  # Mathjax Output
  uiOutput("summary_DE_mathjax")
})

output$summary_DE_mathjax <- renderUI({
  text.size <- input$TI_summary_de_mathjax_font_size
  overflow.type <- ifelse(input$CI_summary_hide_scrollbars, "hidden", "auto")
  title.style = paste0("font-size:",
                       as.character(input$NI_summary_box_title_font_size),
                       "px; font-weight: bold; margin-bottom: 0px;")
  box(
    id = "box_summary_diff_eqns",
    width = 12,
    title = tags$p("Differential Equations", style= title.style),
    # title = HTML("<b>Differential Equations</b></font size>"),
    lapply(seq(length(rv.DE$de.equations.list)), function(i){
      div(
        style = paste0("overflow-x:", overflow.type, ";"),
        withMathJax(
          buildMathjaxEqn(rv.DE$de.equations.list[[i]],
                          i,
                          rv.DE$de.equations.list[[i]]$Compartment.vol,
                          TRUE)
        )
      )
    }),
    tags$head(
      tags$style(
        paste0("#box_summary_diff_eqns .MathJax_Display
             {font-size:",
             text.size,
             "%;}")
      )
    )
  )
})

# Variable Summary -------------------------------------------------------------
output$summary_variable_table <- renderDT({ 
  
  # Build Variable Table
  my.table <- rv.SPECIES$species.df %>%
    select(Name, Value, Unit)
  
  colnames(my.table) <- c("Species", "Value", "Unit")
  
  font.size <- paste0(as.character(input$NI_summary_table_font_size), "%")
  header.size <- paste0(as.character(input$NI_summary_table_header_font_size), 
                        "px")
  overflow.type <- ifelse(input$CI_summary_hide_scrollbars, "hidden", "370px")
  
  DT::datatable(
    my.table,
    class = "cell-border stripe",
    rownames = FALSE,
    editable = TRUE,
    options = list(
      autoWidth = TRUE,
      # colnames = c("Species", "Value", "Unit"),
      columnDefs = list(list(className = "dt-center", targets = "_all")),
      pageLength = -1,
      ordering = FALSE,
      dom = "t",
      scrollY = overflow.type,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': 'white',
        'color': 'black', 'font-size': '20px'});",
        "}"
      )

    )
  ) %>%
  formatStyle(
    columns = c("Species", "Value", "Unit"), fontSize = font.size)
  })

# Parameter Summary ------------------------------------------------------------
output$summary_parameter_table <- renderDT({ 
  # Build Paramter Table
  my.table <- rv.PARAMETERS$parameters.df %>%
    select("Name", "Value", "Unit")
  
  colnames(my.table) <- c("Parameter", "Value", "Unit")
  
  font.size <- paste0(as.character(input$NI_summary_table_font_size), "%")
  overflow.type <- ifelse(input$CI_summary_hide_scrollbars, "hidden", "370px")
  
  DT::datatable(
    my.table,
    class = "cell-border stripe",
    rownames = FALSE,
    # colnames = c("Parameter", "Value", "Unit"),
    editable = TRUE,
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(className = "dt-center", targets = "_all")),
      pageLength = -1,
      ordering = FALSE,
      dom = "t",
      scrollY = overflow.type,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': 'white', 
        'color': 'black', 'font-size': '20px'});",
        "}"
      )
    )
  ) %>% 
  formatStyle(columns = c("Parameter", "Value", "Unit"), fontSize = font.size)
})



# Plot Summary -----------------------------------------------------------------
output$summary_plot <- renderPlot({
  to.plot <- CreatePlot(rv.RESULTS$results.model.final,
                        input$lineplot_yvar,
                        input$choose_color_palette,
                        input$line_size_options,
                        input$line_legend_title,
                        input$line_show_dots,
                        input$line_axis_confirm,
                        input$line_xaxis_min,
                        input$line_xaxis_max,
                        input$line_xstep,
                        input$line_yaxis_min,
                        input$line_yaxis_max,
                        input$line_ystep,
                        input$line_title,
                        input$line_xlabel,
                        input$line_xtitle_location,
                        input$line_axis_text_size,
                        input$line_axis_title_size,
                        input$line_ylabel,
                        input$line_ytitle_location,
                        input$line_axis_text_size,
                        input$line_axis_title_size,
                        input$line_title_text_size,
                        input$line_title_location,
                        input$line_legend_position,
                        input$line_legend_title_size,
                        input$line_legend_font_size,
                        input$line_panel_colorPicker_checkbox,
                        input$line_panel_colorPicker,
                        input$line_plotBackground_color_change,
                        input$line_plotBackground_colorPicker,
                        input$show_overlay_data,
                        data.scatter(),
                        input$plot_data_import_x,
                        input$plot_data_import_y)
})

output$summary_plotly <- renderPlotly({
  to.plot <- CreatePlot(rv.RESULTS$results.model.final,
                        input$lineplot_yvar,
                        input$choose_color_palette,
                        input$line_size_options,
                        input$line_legend_title,
                        input$line_show_dots,
                        input$line_axis_confirm,
                        input$line_xaxis_min,
                        input$line_xaxis_max,
                        input$line_xstep,
                        input$line_yaxis_min,
                        input$line_yaxis_max,
                        input$line_ystep,
                        input$line_title,
                        input$line_xlabel,
                        input$line_xtitle_location,
                        input$line_axis_text_size,
                        input$line_axis_title_size,
                        input$line_ylabel,
                        input$line_ytitle_location,
                        input$line_axis_text_size,
                        input$line_axis_title_size,
                        input$line_title_text_size,
                        input$line_title_location,
                        input$line_legend_position,
                        input$line_legend_title_size,
                        input$line_legend_font_size,
                        input$line_panel_colorPicker_checkbox,
                        input$line_panel_colorPicker,
                        input$line_plotBackground_color_change,
                        input$line_plotBackground_colorPicker,
                        input$show_overlay_data,
                        data.scatter(),
                        input$plot_data_import_x,
                        input$plot_data_import_y
  )
  
  ggplotly(to.plot, 
           tooltip = c("x", "y", "colour"))
})


observeEvent(input$CI_summary_hide_scrollbars, {
  # Turn off scrollbars if selected
  # useful for taking screenshot without scrolls
  # tags$head(
  #   tags$style(
  #     paste0("#box_summary_diff_eqns .MathJax_Display
  #            {font-size:",
  #            text.size,
  #            "%;}")
  #   )
  # )
})