######################## Line Plot Sever #####################################

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________

# SUBTAB: LinePlot
# LINEPLOT000

#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
#__________________________________________________________________________________________________________
gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

observeEvent(input$reset_input, {
  shinyjs::reset("form")
})

# ****These update functions are now located in the post processing tab*********

#updates filter_2 variable choices based on items selected in checkbox selct boxes
# observeEvent(input$execute_run_model, {
#   req(nrow(results$model.final)>1)
#   jPrint("Updating xvar")
#   updatePickerInput(session
#                     ,"lineplot_xvar"
#                     ,choices = colnames(results$model.final[1]))
# })
# 
# #updates filter_2 variable choices based on items selected in checkbox selct boxes
# observeEvent(input$execute_run_model, {
#   req(nrow(results$model.final)>1)
#   jPrint("Updating y var")
#   updateSelectizeInput(session,
#                     "lineplot_yvar"
#                     ,choices  = colnames(results$model.final)[2:ncol(results$model.final)]
#                     ,selected = colnames(results$model.final)[2:ncol(results$model.final)]
#   )
# })
observeEvent(input$execute_run_model, {
  updateTabsetPanel(session = session,
                    "line_options_tabbox",
                    selected = "Style")
  # updateTabsetPanel(session = session,
  #                   "line_options_tabbox",
  #                   selected = "Color")
})

#changes xlabel for line plot to selected input
observeEvent(input$lineplot_xvar,{
  updateTextInput(session,
                  "line_xlabel",
                  label = "X Label",
                  value = input$lineplot_factor_var)
})

#changes xlabel for line plot to selected input
observeEvent(input$lineplot_yvar,{
  updateTextInput(session,
                  "line_ylabel",
                  label = "Y Label",
                  value = 'Values')
})
 #Renders the color panel for each different stratified categorical variable (each at varied distance color levels)
output$line_color_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  lev <- sort(unique(gsub(" ", "_",gatherData(results$model.final)$Variable)))
  cols <- gg_fill_hue(length(lev))
  
  lapply(seq_along(lev), function(i){
      fluidRow(
          div(style="display: block;
                 vertical-align:top;
                 position: relative;
                 z-index:100;
                 left: 100px;
                 top: 5px;",
              h5(lev[i])),
          div(style = "display: block;vertical-align:top; position: absolute; width: 65%",
              colourpicker::colourInput(inputId = paste0("cols_line", lev[i]),
                          label = NULL,
                          value = cols[i],
                          showColour = "background"
                          )
              )
        )
  })
})
outputOptions(output, "line_color_options_popdown", suspendWhenHidden = FALSE)

#This provides the dynamically allocated number of line type options for each variable in the line plots
output$line_type_options_popdown <- renderUI({
  #if this require isn't here bad things happen but I think I need to change more
  lev <- sort(unique(gsub(" ", "_",gatherData(results$model.final)$Variable)))
  
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
outputOptions(output, "line_type_options_popdown", suspendWhenHidden = FALSE)
#this function talkes multiple inputs, and factors them into one column, creating a second column of corresponding groups
#groups are stored in variable :Variable, call with gatherData()$Variable
#data stores in cariable: Value, called same way
gatherData <- function(data){
  req(input$lineplot_yvar)
  selectedData <- gather(select(data.frame(data),
                                #colnames(results$model.final)[1],
                                "time",
                                input$lineplot_yvar),
                         Variable,
                         Value,
                         #-one_of(input$lineplot_xvar)
                         -one_of("time")
                         )
  #selectedData <- melt(data, id.vars = "time")
}

theme_output <- function(theme_input){
  if (theme_input == 'gray') {
    theme_gray()}
  else if (theme_input == 'classic') {
    theme_classic()}
  else if (theme_input == 'void') {
    theme_void()}
  else if (theme_input == 'dark') {
    theme_dark()}
  else if (theme_input == 'bw') {
    theme_bw()}
  else if (theme_input == 'linedraw') {
    theme_linedraw()}
  else if (theme_input == 'light') {
    theme_light()}
  else if (theme_input == 'minimal') {
    theme_minimal()}
  
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

#this is the function that creates the ggplot object for the line plot
plotLineplotInput <- function(data){
  #calls data function and stores it to selectedData
  selectedData <- data
  n = length(unique(selectedData$Variable))
  #n = length(unique(selectedData$variable))
  type_line <-  paste0("c(", paste0("input$line_type", unique(sort(data$Variable)), collapse = ", "), ")")
  type_line <- eval(parse(text = type_line))
  #create vector of cols for lines

  cols_line <- color_palettes(input$choose_color_palette, n)
  # rewrite with the custom values if user chose custom
  if (cols_line[1] == "CUSTOM") {
    cols_line <- paste0("c(", paste0("input$cols_line", unique(sort(data$Variable)), collapse = ", "), ")")
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
  
  if (input$line_show_dots) {g_line <- g_line + geom_point()}
  else{g_line <- g_line}
  
  if(input$line_axis_confirm){
    g_line <- g_line + scale_x_continuous(limits=c(input$line_xaxis_min, input$line_xaxis_max)
                                          ,breaks = seq(from=input$line_xaxis_min, to=input$line_xaxis_max, by=input$line_xstep)) + 
      scale_y_continuous(limits=c(input$line_yaxis_min, input$line_yaxis_max)
                         ,breaks = seq(input$line_yaxis_min, input$line_yaxis_max, input$line_ystep)) 
  }else{g_line <- g_line}

if (is.null(input$lineplot_yvar)) {
  g_line <- g_line +
    #this adds title, xlabel, and ylabel to graph based upon text inputs
    labs(title = "Go to Inputs dropdown and select variable to plot") + 
    theme(plot.title = element_text(hjust = input$line_title_location, size = input$line_title_text_size)
          ,axis.title.x = element_text(hjust = input$line_xtitle_location, size = input$line_x_axis_title_size)
          ,axis.title.y = element_text(hjust = input$line_ytitle_location, size = input$line_y_axis_title_size)
    )
} else {
  g_line <- g_line +
    #this adds title, xlabel, and ylabel to graph based upon text inputs
    labs(title = input$line_title,
         x = input$line_xlabel,
         y = input$line_ylabel) +
    #hjust is used to center the title, size is used to change the text size of the title
    theme_output(input$theme_output_line) +
    theme(plot.title = element_text(hjust = input$line_title_location, size = input$line_title_text_size)
          ,legend.position = input$line_legend_position
          ,legend.title = element_text(size = input$line_legend_title_size)
          ,legend.text = element_text(size = input$line_legend_font_size)
          ,axis.title.x = element_text(hjust = input$line_xtitle_location, size = input$line_axis_title_size)
          ,axis.title.y = element_text(hjust = input$line_ytitle_location, size = input$line_axis_title_size)
          ,axis.text.x = element_text(size = input$line_axis_text_size)
          ,axis.text.y = element_text(size = input$line_axis_text_size)

    )
}
  
  if (input$line_panel_colorPicker_checkbox) {
    g_line <- g_line + theme(panel.background = element_rect(fill = input$line_panel_colorPicker,
                                                             colour = input$line_panel_colorPicker))
  } else {g_line <- g_line}
  
  if (input$line_plotBackground_color_change) {
    g_line <- g_line + theme(plot.background = element_rect(fill = input$line_plotBackground_colorPicker,
                                                             colour = input$line_plotBackground_colorPicker))
  } else {g_line <- g_line}
  
}


# Ui to determine how the plots will be displayed ------------------------------
output$model_plotType <- renderUI({
  div
  (
    if (input$lineplot_choose_plot_mode == 'compare_mode') {
      fluidRow(column(width = 6,
                      jqui_resizable(plotOutput(outputId = 'LinePlot')))
               ,column(width = 6
                       ,jqui_resizable(plotOutput(outputId = 'LinePlot_to_compare')))
               ) # end fluid row
    } else if (input$lineplot_choose_plot_mode == 'loop_mode') {
      fluidRow(column(width = 12,
                      jqui_resizable(plotOutput(outputId = 'LinePlot')))
      )
    } else if (input$lineplot_choose_plot_mode == 'overlay_data_mode') {
      fluidRow(column(width = 9,
                      jqui_resizable(plotOutput(outputId = "lineplot_overlay_scatterplot")))
               ,column(width = 3,
                       box(title = NULL
                           ,status = "success"
                           ,solidHeader = FALSE
                           ,collapsible = TRUE
                           ,width = NULL,
                           "This box will have a load data option,
                           select data to scatter x,y,
                           and maybe a size/color option."
                           ,fileInput(inputId = "overlay_scatter_input", 
                                      label = "Import File")
                           ,selectInput(inputId = 'overlay_scatter_xcol',
                                       label = 'X Variable',
                                       choices = character())
                           ,selectInput(inputId = 'overlay_scatter_ycol',
                                       label = 'Y Variable',
                                       choices = character()))
                       ))
    } else if (input$lineplot_choose_plot_mode == "normal_plot") {
      fluidRow(column(width = 12,
                       jqui_resizable(plotOutput(outputId = 'LinePlot'))))
    }
  )
  
  
})

# Renderplots for all plot options --------------------------------------------- 

output$LinePlot <- renderPlot({
    
    print(plotLineplotInput(gatherData(results$model.final)))
})

output$lineplot_plotly <- renderPlotly({
  data <- gatherData(results$model.final)
  #tryCatch({ggplotly(plotLineplotInput(gatherData(results$model.final)))})
  ggplotly(plotLineplotInput(data), tooltip = c("x", "y", "colour"))
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


#-------------------------------------------------------------------------------

# This section covers the plotting of DataOverlay

#-------------------------------------------------------------------------------

PlotLineplotOverlay <- function(){  #---still have to add scatter plot somehow
  #calls data function and stores it to selectedData
  selectedData <- gatherData()
  
  #create vector of cols for lines
  cols_line <- paste0("c(", paste0("input$cols_line", unique(sort(gatherData()$Variable)), collapse = ", "), ")")
  cols_line <- eval(parse(text = cols_line))
  
  #create vector of linetypes for lines
  type_line <-  paste0("c(", paste0("input$line_type", unique(sort(gatherData()$Variable)), collapse = ", "), ")")
  type_line <- eval(parse(text = type_line))
  #print(type_line)
  
  #ggplot function to print using geom_line
  #g_line <- ggplot(selectedData, aes(x = selectedData[,1], y = selectedData$Value, color = selectedData$Variable)) +
  g_line <- ggplot(selectedData) +
    geom_line(aes(linetype = Variable, x = selectedData[,1], y = Value, color = Variable),
              size = input$line_size_options) +
    geom_point(data = overlay_scatter_data(),
                mapping = aes(x = overlay_scatter_data()[[input$overlay_scatter_xcol]],
                              y = overlay_scatter_data()[[input$overlay_scatter_ycol]])) +
    scale_color_manual(name = input$line_legend_title,
                       values = cols_line) +
    scale_linetype_manual(values = type_line)
  
  
  
  if (input$line_show_dots) {g_line <- g_line + geom_point()}
  else{g_line <- g_line}
  
  g_line <- g_line +
    #this adds title, xlabel, and ylabel to graph based upon text inputs
    labs(title = input$line_title,
         x = input$line_xlabel,
         y = input$line_ylabel) +
    #hjust is used to center the title, size is used to change the text size of the title
    theme_output_line() +
    theme(plot.title = element_text(hjust = 0.5, size = 22),
          #allows user to change position of legend
          legend.position = input$line_legend_position)
  
}


# Scatterplot Overlay ----------------------------------------------------------
overlay_scatter_data <- reactive({
  req(input$overlay_scatter_input)
  #fread(input$data$datapath, na.strings=c("", NA))
  if (endsWith(input$overlay_scatter_input$datapath, ".csv")) {
    read.csv(input$overlay_scatter_input$datapath)
  } else if (endsWith(input$overlay_scatter_input$datapath, ".txt")) {
    read.table(input$overlay_scatter_input$datapath,header = T)
  }else if (endsWith(input$overlay_scatter_input$datapath, ".xls")) {
    read_excel(input$overlay_scatter_input$datapath)
  } else if (endsWith(input$overlay_scatter_input$datapath, ".xlsx")) {
    read_xlsx(input$overlay_scatter_input$datapath,sheet = 1)
  }
})
observeEvent(input$overlay_scatter_input, {
  req(overlay_scatter_data())
  updateSelectInput(session
                    ,"overlay_scatter_xcol"
                    ,choices = colnames(overlay_scatter_data())
                    ,selected = colnames(overlay_scatter_data())[1])
  updateSelectInput(session
                    ,"overlay_scatter_ycol"
                    ,choices = colnames(overlay_scatter_data())
                    ,selected = colnames(overlay_scatter_data())[2])
})

# 
# output$dyGraph <- renderDygraph({
#   selectedData <- gatherData(results$model.final)
#   dygraph(selectedData, main = "test")
# })
