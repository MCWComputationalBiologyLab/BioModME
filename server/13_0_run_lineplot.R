######################## Line Plot Sever #####################################



# Functions --------------------------------------------------------------------

# Gets evenly placed separations of color hues
gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#this function talkes multiple inputs, and factors them into one column, 
#creating a second column of corresponding groups
#groups are stored in variable :Variable, call with gatherData()$Variable
#data stores in cariable: Value, called same way
gatherData <- function(data, varsToSelect){
  if (!is.null(varsToSelect)) {
    selectedData <- gather(select(data.frame(data),
                                  "time",
                                  all_of(varsToSelect)),
                           Variable,
                           Value,
                           -one_of("time")
    )
  }
}

theme_output <- function(theme_input){
  
  switch (theme_input,
          gray     = {theme_gray()},
          classic  = {theme_classic()},
          void     = {theme_void()},
          dark     = {theme_dark()},
          bw       = {theme_bw()},
          linedraw = {theme_linedraw()},
          light    = {theme_light()},
          minimal  = {theme_minimal()}
  )
}

color_palettes <- function(palette_input, n){
  switch(palette_input,
         viridis  = {col.out <- viridis(n)},
         magma    = {col.out <- viridis(n, option = "magma")},
         inferno  = {col.out <- viridis(n, option = "inferno")},
         plasma   = {col.out <- viridis(n, option = "plasma")},
         cividis  = {col.out <- viridis(n, option = "cividis")},
         rocket   = {col.out <- viridis(n, option = "rocket")},
         mako     = {col.out <- viridis(n, option = "mako")},
         turbo    = {col.out <- viridis(n, option = "turbo")},
         custom   = {col.out <- "CUSTOM"}
  )
  return(col.out)
}

CreatePlot <- function(modelResults,
                       concentrations, 
                       colorPalette,
                       lineSize,
                       legendTitle,
                       optionShowDots,
                       optionCustomAxis,
                       xAxisMin,
                       xAxisMax,
                       xAxisStep,
                       yAxisMin,
                       yAxisMax,
                       yAxisStep,
                       plotTitle,
                       xAxisLabel,
                       xAxisLabelLocation,
                       xAxisLabelSize,
                       xAxisSize,
                       yAxisLabel,
                       yAxisLabelLocation,
                       yAxisLabelSize,
                       yAxisSize,
                       titleSize,
                       titleLocation,
                       legendLocation,
                       legendTitleSize,
                       legendItemsSize,
                       optionOverridePanelColor,
                       plotPanelColor,
                       optionOverridePlotColor,
                       plotBackgroundColor,
                       optionOverlayData,
                       dataToOverlay, 
                       overlayX,
                       overlayY

) {
  # Inputs
  #   @modelResults - final results from model
  #   @concentrations - string vector of column names to plot for concentrations
  #   @colorPalette - selected color palette input (i.e custom, magma, inferno)
  #   @lineSize - selected line size for plotted concentrations
  #   @legendTitle - title of legend for figure
  #   @optionShowDats - boolean that will show individual points of lineplot
  #   @optionCustomAxis - boolean to allow custom x,y axis numbering and spacing
  #   @xAxisMin - custom axis lowest x axis value
  #   @xAxisMax - custom axis highest x axis value
  #   @xAxisStep - Axis steps between xAxisMin & xAxisMax
  #   @yAxisMin - custom axis lowest y axis value
  #   @yAxisMax - custom axis highest y axis value
  #   @yAxisStep - Axis steps between yAxisMin & yAxisMax
  #   @plotTitle - Title to display on plot
  #   @xAxisLabel - Label to display on xAxis
  #   @yAxisLabel - Label to display on yAxis
  #   @optionOverlayData - Boolean to plot OverlayData (Scatterplot)
  #   @dataToOverlay - Data to plot with Scatter, first column time, then data

  
  # PrintVar(concentrations) 
  # PrintVar(colorPalette)
  # PrintVar(lineSize)
  # PrintVar(legendTitle)
  # PrintVar(optionShowDots)
  # PrintVar(optionCustomAxis)
  # PrintVar(xAxisMin)
  # PrintVar(xAxisMax)
  # PrintVar(xAxisStep)
  # PrintVar(yAxisMin)
  # PrintVar(yAxisMax)
  # PrintVar(yAxisStep)
  # PrintVar(plotTitle)
  # PrintVar(xAxisLabel)
  # PrintVar(xAxisLabelLocation)
  # PrintVar(xAxisLabelSize)
  # PrintVar(xAxisSize)
  # PrintVar(yAxisLabel)
  # PrintVar(yAxisLabelLocation)
  # PrintVar(yAxisLabelSize)
  # PrintVar(yAxisSize)
  # PrintVar(titleSize)
  # PrintVar(titleLocation)
  # PrintVar(legendLocation)
  # PrintVar(legendTitleSize)
  # PrintVar(legendItemsSize)
  # PrintVar(optionOverridePanelColor)
  # PrintVar(plotPanelColor)
  # PrintVar(optionOverridePlotColor)
  # PrintVar(plotBackgroundColor)
  
  # use gather on incoming results to put them into a plottable data structure
  selectedData <- gatherData(modelResults, concentrations)
  n <- length(unique(selectedData$Variable))
  
  # gather vector of selected line type inputs and evaluate to vector
  type_line <- paste0("c(", 
                      paste0("input$line_type", 
                            unique(sort(selectedData$Variable)), 
                            collapse = ", "),
                      ")"
                )
  
  type_line <- eval(parse(text = type_line))
  
  # Find selected color palletes and create
  cols_line <- color_palettes(colorPalette, n)
  # rewrite with the custom values if user chose custom
  if (cols_line[1] == "CUSTOM") {
    cols_line <-
      paste0("c(",
             paste0("input$cols_line", 
                    unique(sort(selectedData$Variable)), 
                    collapse = ", "),
             ")")
    cols_line <- eval(parse(text = cols_line))
  }
  
  # Begin Plotting Data
  #ggplot function to print using geom_line
  g_line <- ggplot(selectedData) +
    geom_line(
      aes(linetype = Variable,
          x = selectedData[,1],
          y = Value,
          color = Variable),
      size = lineSize)

  # browser()
  # Overlay data
  if (optionOverlayData & length(overlayY) > 0) {
    #Change first column
    colnames(dataToOverlay)[1] <- "time"
    # Select columns to be used
    data <- dataToOverlay %>% select(time, all_of(overlayY))
    
    # Melt loaded data
    data.m <- reshape2::melt(data, id.vars = "time")
    g_line <-
      g_line + geom_point(
                data = data.m,
                mapping =
                  aes(x = time,
                      y = value,
                      color = variable,
                      #I know this doesn't have linetype but the plotly plot
                      # adds a 1 to the legend value (C2,1) for example. If
                      # I give it a linetype it removes this.  Have to look into
                      # aesthetics more but time constraints I'm moving on with
                      # temp fix. 
                      linetype = variable))
  }
  
  g_line <- g_line + 
    # Select the colors of the lines
    scale_color_manual(name = legendTitle,
                       values = cols_line,
                       labels = unique(selectedData$Variable)) +
    # Select the show type of the lines
    scale_linetype_manual(name = legendTitle,
                          values = type_line,
                          labels = unique(selectedData$Variable)) +
    #this adds title, xlabel, and ylabel to graph based upon text inputs
    labs(
      title = plotTitle,
      x = xAxisLabel,
      y = yAxisLabel,
      shape = legendTitle,
      colour = legendTitle
    ) +
    theme_output(input$theme_output_line) +
    theme(
      # Plot title size and position
      plot.title      = element_text(hjust = titleLocation,
                                     size = titleSize),
      # Legend title size, and item sizes
      legend.title    = element_text(size = legendTitleSize),
      legend.text     = element_text(size = legendItemsSize),
      # x/y axis size and locations
      axis.title.x    = element_text(hjust = xAxisLabelLocation,
                                     size = xAxisSize),
      axis.title.y    = element_text(hjust = yAxisLabelLocation,
                                     size = yAxisSize),
      # x/y axis label text size
      axis.text.x     = element_text(size = xAxisLabelSize),
      axis.text.y     = element_text(size = yAxisLabelSize),
      # Position of legend relative to plot
      legend.position = legendLocation
    )
  
  # Option to show lineplot dots
  if (optionShowDots) {g_line <- g_line + geom_point()}
  
  # Options for Custom Axis Choices
  if (optionCustomAxis) {
    g_line <-
      g_line + scale_x_continuous(
        limits = c(xAxisMin, xAxisMax),
        breaks = seq(
          from = xAxisMin,
          to = xAxisMax,
          by = xAxisStep
        )
      ) +
      scale_y_continuous(
        limits = c(yAxisMin, yAxisMax),
        breaks = seq(
          from = yAxisMin,
          to = yAxisMax,
          by = yAxisStep
        )
      )
  }
  
  if (is.null(concentrations)) {
    g_line <- g_line +
      #this adds title, xlabel, and ylabel to graph based upon text inputs
      labs(title = "Go to Inputs dropdown and select variable to plot")
  } else {
    g_line <- g_line +
      #this adds title, xlabel, and ylabel to graph based upon text inputs
      labs(
        title = plotTitle,
        x = xAxisLabel,
        y = yAxisLabel
      )
  }
  
  # Change Plotting Panel Color
  if (optionOverridePanelColor) {
    g_line <- g_line +
      theme(
        panel.background = element_rect(
          fill = plotPanelColor,
          colour = plotPanelColor
        )
      )
  }
    
  # Change Plot Background Color  
  if (optionOverridePlotColor) {
    g_line <-
      g_line + theme(
        plot.background = element_rect(
          fill = plotBackgroundColor,
          colour = plotBackgroundColor
        )
      )
  }
  
  return(g_line)
}

#this is the function that creates the ggplot object for the line plot
plotLineplotInput <- function(data) {
  #calls data function and stores it to selectedData
  selectedData <- data
  n = length(unique(selectedData$Variable))
  #n = length(unique(selectedData$variable))
  type_line <-
    paste0("c(", paste0("input$line_type", 
                        unique(sort(data$Variable)), 
                        collapse = ", "), ")")
  type_line <- eval(parse(text = type_line))
  #create vector of cols for lines
  
  cols_line <- color_palettes(input$choose_color_palette, n)
  # rewrite with the custom values if user chose custom
  if (cols_line[1] == "CUSTOM") {
    cols_line <-
      paste0("c(",
             paste0("input$cols_line", 
                    unique(sort(data$Variable)), 
                    collapse = ", "),
             ")")
    cols_line <- eval(parse(text = cols_line))
  }
  
  #ggplot function to print using geom_line
  g_line <- ggplot(selectedData, aes(x = time, y = Value, color = Variable)) +
    #g_line <- ggplot(selectedData, aes(x = selectedData[,1], y = Value)) +
    geom_line(aes(linetype = Variable),
              size = input$line_size_options) +
    #scale_fill_brewer(palette = "Dark2") + 
    #scale_color_viridis(discrete = FALSE, option = "D") + 
    scale_color_manual(name = input$line_legend_title,
                       values = cols_line) +
    scale_linetype_manual(name = input$line_legend_title,
                          values = type_line)
  
  if (input$line_show_dots) {
    g_line <- g_line + geom_point()
  }
  else{
    g_line <- g_line
  }
  
  if (input$line_axis_confirm) {
    g_line <-
      g_line + scale_x_continuous(
        limits = c(input$line_xaxis_min, input$line_xaxis_max),
        breaks = seq(
          from = input$line_xaxis_min,
          to = input$line_xaxis_max,
          by = input$line_xstep
        )
      ) +
      scale_y_continuous(
        limits = c(input$line_yaxis_min, input$line_yaxis_max),
        breaks = seq(
          input$line_yaxis_min,
          input$line_yaxis_max,
          input$line_ystep
        )
      )
  } else{
    g_line <- g_line
  }
  
  if (is.null(input$lineplot_yvar)) {
    g_line <- g_line +
      #this adds title, xlabel, and ylabel to graph based upon text inputs
      labs(title = "Go to Inputs dropdown and select variable to plot") +
      theme(
        plot.title = element_text(
          hjust = input$line_title_location,
          size = input$line_title_text_size
        )
        ,
        axis.title.x = element_text(
          hjust = input$line_xtitle_location,
          size = input$line_x_axis_title_size
        )
        ,
        axis.title.y = element_text(
          hjust = input$line_ytitle_location,
          size = input$line_y_axis_title_size
        )
      )
  } else {
    g_line <- g_line +
      #this adds title, xlabel, and ylabel to graph based upon text inputs
      labs(
        title = input$line_title,
        x = input$line_xlabel,
        y = input$line_ylabel
      ) +
      #hjust is used to center the title, size is used to change the text size of the title
      theme_output(input$theme_output_line) +
      theme(
        plot.title = element_text(
          hjust = input$line_title_location,
          size = input$line_title_text_size
        ),
        legend.position = input$line_legend_position,
        legend.title = element_text(
          size = input$line_legend_title_size),
        legend.text = element_text(
          size = input$line_legend_font_size),
        axis.title.x = element_text(
          hjust = input$line_xtitle_location,
          size = input$line_axis_title_size
        ),
        axis.title.y = element_text(
          hjust = input$line_ytitle_location,
          size = input$line_axis_title_size
        ),
        axis.text.x = element_text(size = input$line_axis_text_size),
        axis.text.y = element_text(size = input$line_axis_text_size)
        
      )
  }
  
  if (input$line_panel_colorPicker_checkbox) {
    g_line <- g_line +
      theme(
        panel.background = element_rect(
          fill = input$line_panel_colorPicker,
          colour = input$line_panel_colorPicker
        )
      )
  } else {
    g_line <- g_line
  }
  
  if (input$line_plotBackground_color_change) {
    g_line <-
      g_line + theme(
        plot.background = element_rect(
          fill = input$line_plotBackground_colorPicker,
          colour = input$line_plotBackground_colorPicker
        )
      )
  } else {
    g_line <- g_line
  }
}

# UI Renders -------------------------------------------------------------------

#Renders the color panel for each different stratified categorical variable
#(each at varied distance color levels)
output$line_color_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change 
  #more
  lev <-
    sort(unique(gsub(
      " ", "_", gatherData(rv.RESULTS$results.model.final, 
                           input$lineplot_yvar)$Variable
    )))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i) {
    fluidRow(
      div(
        style = "display: block;
                 vertical-align:top;
                 position: relative;
                 z-index:100;
                 left: 100px;
                 top: 5px;",
        h5(lev[i])
      ),
      div(
        style = "display: block;
                 vertical-align:top; 
                 position: absolute; 
                 width: 65%",
        colourpicker::colourInput(
          inputId = paste0("cols_line", lev[i]),
          label = NULL,
          value = cols[i],
          showColour = "background"
        )
      )
    )
  })
})

#This provides the dynamically allocated number of line type options for each 
# variable in the line plots
output$line_type_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  lev <- sort(unique(gsub(" ", "_",gatherData(rv.RESULTS$results.model.final, 
                                              input$lineplot_yvar)$Variable)))
  
  lapply(seq_along(lev), function(i){
    selectInput(inputId = paste0("line_type", lev[i]),
                label = paste0("Line type: ", lev[i]),
                choices = c("solid" = "solid",
                            "Dashed" = "dashed",
                            "Dotted" = "dotted",
                            "Long Dash" = "longdash",
                            "Dot-Dash" = "dotdash"))
  })
})

# Ui to determine how the plots will be displayed ------------------------------
output$model_plotType <- renderUI({
  div(
    if (input$lineplot_choose_plot_mode == 'compare_mode') {
      fluidRow(
        column(
          width = 6,
          jqui_resizable(plotOutput(outputId = 'LinePlot'))
        ),
        column(
          width = 6,
          jqui_resizable(plotOutput(outputId = 'LinePlot_to_compare'))
        )
      )
    } else if (input$lineplot_choose_plot_mode == 'overlay_data_mode') {
      fluidRow(
        column(
          width = 9,
          jqui_resizable(plotOutput(outputId = "lineplot_overlay_scatterplot"))
        )
      )
    } else if (input$lineplot_choose_plot_mode == "normal_plot") {
      fluidRow(
        column(
          width = 12,
          jqui_resizable(plotOutput(outputId = 'LinePlot'))
        )
      )
    })
})


# Output Options ---------------------------------------------------------------

# This allows this pickers to fill even when dropdown is not selected
outputOptions(output, "line_color_options_popdown", suspendWhenHidden = FALSE)
outputOptions(output, "line_type_options_popdown", suspendWhenHidden = FALSE)


# Events -----------------------------------------------------------------------
observeEvent(input$reset_input, {
  shinyjs::reset("form")
})

observeEvent(input$execute_run_model, {
  updateTabsetPanel(session = session,
                    "line_options_tabbox",
                    selected = "Style")
  
  # Set x-axis Label
  updateTextInput(session,
                  "line_xlabel",
                  label = "X Label",
                  value = paste0("Time (",
                                 rv.RESULTS$results.time.units,
                                 ")"
                  )
  )
  
  # Set y-axis Label
  updateTextInput(session,
                  "line_ylabel",
                  label = "Y Label",
                  value = paste0("Concentration (",
                                 rv.UNITS$units.base$Count,
                                 "/",
                                 rv.UNITS$units.base$Volume,
                                 ")"
                  )
  )
})

# Renderplots for all plot options --------------------------------------------- 

output$main_lineplot <- renderPlot({
  if (nrow(rv.RESULTS$results.model.final) != 0) {
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
    return(to.plot)
  } else {
    plot(1, 1, type="n", xlab="", ylab="", xaxt='n', yaxt='n')
    text(1, 1, "Execute Model For Plot.", cex=1.5)
  }
  
})

output$lineplot_plotly <- renderPlotly({
  
  if (nrow(rv.RESULTS$results.model.final) != 0) {
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
  } else {
    p <- plot_ly() %>%
      add_trace(
        x = 1,
        y = 1,
        type = 'scatter',
        mode = 'text',
        text = "Execute Model For Plot",
        textfont = list(size = 18)
      ) %>%
      layout(
        xaxis = list(showticklabels = FALSE),
        yaxis = list(showticklabels = FALSE)
      )
    p
  }
})
  
output$lineplot_overlay_scatterplot <- renderPlot({
  print(PlotLineplotOverlay())
}, bg = "transparent"
)

output$downloadLine <- downloadHandler(
  filename = function(){
    paste(input$line_download_title, input$line_download_radiobuttons, sep = "")
  },
  content = function(file){
    ggsave(file, plotLineplotInput())
  }
)

# Tables for refreshing plot ---------------------------------------------------
output$plot_param_table <- renderRHandsontable({
  req(length(rv.PARAMETERS$parameters) > 0)
  
  for.table <- rv.PARAMETERS$parameters.df %>%
    select("Name", "Value", "Unit", "Description")
  
  rhandsontable(
    for.table,
    rowHeaders = NULL,
    colHeaderWidth = 100,
    stretchH = "all"
  ) %>%
    hot_col("Name", readOnly = TRUE) %>%
    hot_cols(
      colWidth = c(30, 15, 15, 90),
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
           }") %>%
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(
      allowRowEdit = FALSE,
      allowColEdit = FALSE) %>%
    hot_validate_numeric(col = 2, min = 0)
})

observeEvent(input$plot_param_table$changes$changes, {
  xi  = input$plot_param_table$changes$changes[[1]][[1]]
  yi  = input$plot_param_table$changes$changes[[1]][[2]]
  old = input$plot_param_table$changes$changes[[1]][[3]]
  new = input$plot_param_table$changes$changes[[1]][[4]]
  
  # Find parameter name that was changed
  
  plotted.table <- rv.PARAMETERS$parameters.df
  
  # Apply Filters
  
  plotted.table <- plotted.table %>% 
    filter(!Custom)
  
  if (input$parameters_filter_type != "All") {
    plotted.table <- plotted.table %>%
      filter(Type == input$parameters_filter_type)
  }
  
  plotted.table <- plotted.table %>%
    select("Name", "Value", "Unit", "Description")
  
  par.name <- unname(unlist(plotted.table[xi+1, 1]))
  par.id   <- FindId(par.name)
  
  if (yi == 0) {
    # PARAMETER NAME CHANGE
    rv.PARAMETERS$parameters[[par.id]]$Name <- new
    
    rv.LOGS$IO.logs <- RenameVarInVector(old, new, rv.LOGS$IO.logs)
    
    names.list <- names(rv.REACTIONS)
    for (name in names.list) {
      rv.REACTIONS[[name]] <- 
        replace_word_recursive(rv.REACTIONS[[name]], old, new)
      
      rv.REACTIONS[[name]] <- 
        replace_latex_variable_recursive(rv.REACTIONS[[name]], 
                                         Var2Latex(old), 
                                         Var2Latex(new))
    }
    
    names.list <- names(rv.IO)
    for (name in names.list) {
      rv.IO[[name]] <- 
        replace_word_recursive(rv.IO[[name]], old, new)
      rv.IO[[name]] <- 
        replace_latex_variable_recursive(rv.IO[[name]],
                                         Var2Latex(old), 
                                         Var2Latex(new))
    }
    
    # If volume change in compartment data structure
    if (rv.PARAMETERS$parameters[[par.id]]$Type == "Compartment") {
      # Find which compartment has this volume
      for (i in seq(length(rv.COMPARTMENTS$compartments))) {
        # If the volume name == old volume name 
        if (rv.COMPARTMENTS$compartments[[i]]$Volume == old) {
          rv.COMPARTMENTS$compartments[[i]]$Volume = new
          break
        }
      }
    }
    
    # Change parameter name in ID database
    idx.for.id <- which(rv.ID$id.df[, 2] %in% old)
    var.id <- rv.ID$id.df[idx.for.id, 1]
    rv.ID$id.df[idx.for.id, 2] <- new
    
  } 
  else if (yi == 1) {
    # PARAMETER VALUE CHANGE
    # browser()
    # Set booleans
    conversion.needed <- FALSE
    
    # Parameter value change 
    rv.PARAMETERS$parameters[[par.id]]$Value <- new
    
    # Change base value of parameter if needed
    selected.unit <- rv.PARAMETERS$parameters[[par.id]]$Unit
    base.unit     <- rv.PARAMETERS$parameters[[par.id]]$BaseUnit
    
    if (is.na(selected.unit) || is.na(base.unit)) {
      # Account for parameters with no units
      rv.PARAMETERS$parameters[[par.id]]$BaseValue <- new
      
    } else {
      if (selected.unit != base.unit) {
        # Perform unit conversion
        conversion.needed <- TRUE
        descriptor <- rv.PARAMETERS$parameters[[par.id]]$UnitDescription
        converted.value <- UnitConversion(descriptor,
                                          selected.unit,
                                          base.unit,
                                          as.numeric(new))
        rv.PARAMETERS$parameters[[par.id]]$BaseValue <- converted.value
      } else {
        rv.PARAMETERS$parameters[[par.id]]$BaseValue <- new
      }
      
      # If volume change in compartment data structure
      if (rv.PARAMETERS$parameters[[par.id]]$Type == "Compartment") {
        # Find which compartment has this volume
        vol.name <- rv.PARAMETERS$parameters[[par.id]]$Name
        for (i in seq(length(rv.COMPARTMENTS$compartments))) {
          if (rv.COMPARTMENTS$compartments[[i]]$Volume == vol.name) {
            if (conversion.needed) {
              rv.COMPARTMENTS$compartments[[i]]$BaseValue <- converted.value
            } else {
              rv.COMPARTMENTS$compartments[[i]]$BaseValue <- new
            }
            rv.COMPARTMENTS$compartments[[i]]$Value <- new
            break
          }
        }
      }
    }
    
    
  } 
  else if (yi == 2) {
    # UNIT CHANGE
    
    # Check if no unit exists, then skip and reassign NA
    # Note Rhandsontable stores NA as NULL, hence the null check
    if (!is.null(old)) {
      # check if units are acceptable
      descriptor <- rv.PARAMETERS$parameters[[par.id]]$UnitDescription
      
      # Check to make sure units entered are the right ones
      comparison <- UnitCompare(descriptor,
                                new,
                                rv.UNITS$units.choices)
      
      if (comparison$is.match) {
        # Parameter unit change
        new <- Unit_Dict_Convert(UNIT_MAPPING, new)
        
        rv.PARAMETERS$parameters[[par.id]]$Unit <- new
        rv.REFRESH$refresh.param.table <- rv.REFRESH$refresh.param.table + 1
        
        # We take current value on table as unitvalue
        # We take current unit as the previous units
        # We take base unit as new Units
        # The converted value will be the new base unit value
        
        # Perform Conversion for base value if needed
        from.unit <- rv.PARAMETERS$parameters[[par.id]]$Unit
        to.unit   <- rv.PARAMETERS$parameters[[par.id]]$BaseUnit
        from.val  <- rv.PARAMETERS$parameters[[par.id]]$Value
        
        if (from.unit != to.unit) {
          # Perform unit conversion for base
          descriptor <- rv.PARAMETERS$parameters[[par.id]]$UnitDescription
          converted.value <- UnitConversion(descriptor,
                                            from.unit,
                                            to.unit,
                                            as.numeric(from.val))
          rv.PARAMETERS$parameters[[par.id]]$BaseValue <- converted.value
        } else {
          rv.PARAMETERS$parameters[[par.id]]$BaseValue <- from.val
        }
        
        # If volume change in compartment data structure change unit there
        if (rv.PARAMETERS$parameters[[par.id]]$Type == "Compartment") {
          # Find which compartment has this volume and change unit/basevalue
          vol.name <- rv.PARAMETERS$parameters[[par.id]]$Name
          for (i in seq(length(rv.COMPARTMENTS$compartments))) {
            if (rv.COMPARTMENTS$compartments[[i]]$Volume == vol.name) {
              rv.COMPARTMENTS$compartments[[i]]$Unit <- 
                rv.PARAMETERS$parameters[[par.id]]$Unit
              
              rv.COMPARTMENTS$compartments[[i]]$BaseValue <- 
                rv.PARAMETERS$parameters[[par.id]]$BaseValue
              break
            }
          }
        }
      } else {
        # if unit conversion isn't allowed
        rv.REFRESH$refresh.param.table <- rv.REFRESH$refresh.param.table + 1
        rv.PARAMETERS$parameters[[par.id]]$Unit <- old
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = comparison$message,
          type = "error"
        )
      }
    } else {
      # Reassign NA
      rv.REFRESH$refresh.param.table <- rv.REFRESH$refresh.param.table + 1
      rv.PARAMETERS$parameters[[par.id]]$Unit <- NA
    }
    
  } else if (yi == 3) {
    # Parameter description change
    rv.PARAMETERS$parameters[[par.id]]$Description <- new
  }
})
# This table should mirror the table on the main page of the app
# In the future we should create a module for it but for now we will
# do a copy and paste.
output$plot_var_table <- renderRHandsontable({
  req(length(rv.SPECIES$species) > 0)
  
  df.by.comp <- select(rv.SPECIES$species.df, 
                       Name, 
                       Value, 
                       Unit, 
                       Compartment, 
                       Description)
  
  df.by.comp <- as.data.frame(df.by.comp)
  
  colnames(df.by.comp) <- c("Name",
                            "Value",
                            "Unit",
                            "Compartment",
                            "Description"
  )
  
  rv.SPECIES$plotted.var.table <- df.by.comp
  
  rhandsontable(
    df.by.comp,
    # overflow = "visible",
    # rowHeaders = NULL,
    # selectCallback = TRUE,
    colHeaderWidth = 100,
    stretchH = "all"
  ) %>%
    hot_cols(
      colWidth = c(30, 20, 20, 30, 60),
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
           }") %>%
    hot_col(c("Name", "Compartment"), readOnly = TRUE) %>%
    hot_rows(rowHeights = 30) %>%
    hot_context_menu(allowRowEdit = FALSE,
                     allowColEdit = FALSE
    )
})

observeEvent(input$plot_var_table$changes$changes, {
  # browser()
  xi = input$plot_var_table$changes$changes[[1]][[1]]
  yi = input$plot_var_table$changes$changes[[1]][[2]]
  old = input$plot_var_table$changes$changes[[1]][[3]]
  new = input$plot_var_table$changes$changes[[1]][[4]]
  
  # Find which variable is being changed
  var.name  <- rv.SPECIES$plotted.var.table[xi+1, 1]
  search.id <- FindId(var.name)
  # If Name changed
  if (yi == 0) {
    # SPECIES NAME CHANNGE
    
    # Check if name change is a valid new name
    
    # Find id of variable name 
    # Find variable id and change corresponding name 
    idx.for.id <- which(rv.ID$id.df[, 2] %in% old)
    var.id <- rv.ID$id.df[idx.for.id, 1]
    rv.ID$id.df[idx.for.id, 2] <- new
    
    # Search Other Areas Affected by Var Name Change
    # Steps: 
    #  Search eqn df for id.
    # Rename Parameters Found in Reaction Lists
    # rv.REACTIONS <- replace_word_recursive(rv.REACTIONS, old, new)
    # browser()
    names.list <- names(rv.REACTIONS)
    for (name in names.list) {
      rv.REACTIONS[[name]] <- 
        replace_word_recursive(rv.REACTIONS[[name]], old, new)
      
      rv.REACTIONS[[name]] <- 
        replace_latex_variable_recursive(rv.REACTIONS[[name]], 
                                         Var2Latex(old), 
                                         Var2Latex(new))
    }
    
    names.list <- names(rv.IO)
    for (name in names.list) {
      rv.IO[[name]] <- 
        replace_word_recursive(rv.IO[[name]], old, new)
      rv.IO[[name]] <- 
        replace_latex_variable_recursive(rv.IO[[name]],
                                         Var2Latex(old), 
                                         Var2Latex(new))
    }
    
    # Change name in species list
    rv.SPECIES$species[[search.id]]$Name <- new
    
    
    # Reset differential equations with new name
    solveForDiffEqs()
    
  } else if (yi == 1) {
    # CHANGE SPECIES VALUE
    rv.SPECIES$species[[search.id]]$Value <- new
    
    # Change the base value of the value if needed.
    select.unit <- rv.SPECIES$species[[search.id]]$Unit
    base.unit   <- rv.SPECIES$species[[search.id]]$BaseUnit
    if (select.unit != base.unit) {
      # Perform Unit Conversion
      descriptor <- rv.SPECIES$species[[search.id]]$UnitDescription
      converted.value <- UnitConversion(descriptor,
                                        select.unit,
                                        base.unit,
                                        as.numeric(new))
      rv.SPECIES$species[[search.id]]$BaseValue <- converted.value
    } else {
      # Simply Overwrite BaseValue
      rv.SPECIES$species[[search.id]]$BaseValue <- new
    }
  } else if (yi == 2) {
    # CHANGE SPECIES UNIT
    descriptor <- rv.SPECIES$species[[search.id]]$UnitDescription
    
    # Check to make sure units entered are the right ones
    comparison <- UnitCompare(descriptor,
                              new,
                              rv.UNITS$units.choices)
    
    if (comparison$is.match) {
      new <- Unit_Dict_Convert(UNIT_MAPPING, new)
      rv.REFRESH$refresh.species.table <- rv.REFRESH$refresh.species.table + 1
      # Change units
      rv.SPECIES$species[[search.id]]$Unit  <- new
      
      # Change base value of variable concentration if needed
      from.unit <- rv.SPECIES$species[[search.id]]$Unit
      to.unit   <- rv.SPECIES$species[[search.id]]$BaseUnit
      from.val  <- as.numeric(rv.SPECIES$species[[search.id]]$Value)
      
      if (from.unit != to.unit) {
        # Perform Unit Conversion
        new.value <- UnitConversion(descriptor,
                                    from.unit, 
                                    to.unit,
                                    from.val)
        
        rv.SPECIES$species[[search.id]]$BaseValue <- new.value
      } else {
        rv.SPECIES$species[[search.id]]$BaseValue <- from.val
      }
      
    } else {
      rv.SPECIES$species[[search.id]]$Unit  <- old
      rv.REFRESH$refresh.species.table <- rv.REFRESH$refresh.species.table + 1
      sendSweetAlert(
        session = session,
        title = "Error...",
        text = comparison$message,
        type = "error"
      )
    }
    
  } else if (yi == 3) {
    # CHANGE SPECIES COMPARTMENT
    rv.SPECIES$species[[search.id]]$Compartment <- new
  } else if (yi == 4) {
    # CHANGE SPECIES DESCRIPTION
    rv.SPECIES$species[[search.id]]$Description <- new
  }
  
  rv.SPECIES$species.df <- bind_rows(rv.SPECIES$species)
  
})


# Overlay Data -----------------------------------------------------------------
data.scatter <- reactive({
  req(input$plot_data_import)
  #fread(input$data$datapath, na.strings=c("", NA))
  if(endsWith(input$plot_data_import$datapath, ".csv")){
    out <- read.csv(input$plot_data_import$datapath)
  } else if(endsWith(input$plot_data_import$datapath, ".txt")){
    out <- read.table(input$plot_data_import$datapath,header = T)
  }else if(endsWith(input$plot_data_import$datapath, ".xls")){
    out <- read_excel(input$plot_data_import$datapath)
  } else if(endsWith(input$plot_data_import$datapath, ".xlsx")){
    out <- read_xlsx(input$plot_data_import$datapath,sheet=1)
  }
  
  species <- colnames(out)[-1]

  return(out)
})

observeEvent(input$plot_data_import, {
  req(data.scatter())
  
  updatePickerInput(session,
                    "plot_data_import_x",
                    choices = colnames(data.scatter()),
                    selected = colnames(data.scatter())[1]
  )
  
  updatePickerInput(session,
                    "plot_data_import_y",
                    choices = colnames(data.scatter()),
                    selected = colnames(data.scatter())[2]
  )
  
  updateCheckboxInput(session,
                      "show_overlay_data",
                      value = TRUE)
})

output$plot_import_data_table <- renderRHandsontable({
  
  rows.in.table <- nrow(data.scatter())
  
  # Set table message if no data loaded
  if (rows.in.table == 0) {
    
  } else {
    # Load parameter table with appropriate parameters
    rhandsontable(
      data.scatter(),
      readOnly = TRUE
      )
  }
  
})


# Download Plot Button ---------------------------------------------------------
output$lineplot_download_plots <- downloadHandler(
  filename = function(){
    paste(input$lineplot_download_title, 
          input$lineplot_download_radiobuttons, 
          sep="")
  },
  content = function(file){
    ggsave(file, CreatePlot(rv.RESULTS$results.model.final,
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
                  ),
           width = input$lineplot_download_width, 
           height = input$lineplot_download_height, 
           dpi = input$lineplot_download_dpi, 
           units = input$lineplot_download_units
    )
  }
)
