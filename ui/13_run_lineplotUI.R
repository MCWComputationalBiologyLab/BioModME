js2 <- paste0(c(
  "var selectinput = document.getElementById('lineplot_yvar');",
  "selectinput.selectize.setValue(-1, false);",
  "selectinput.selectize.selectall();",
  "$('#select + .selectize-control .item').removeClass('active');"),
  collapse = "\n")

TAB_RUN_LINEPLOT <- tabItem(
  tabName = "TAB_RUN_LINEPLOT",
  fluidRow(
    column(
      width = 3,
      pickerInput(inputId = "lineplot_choose_plot_mode"
                   ,label = "Choose Plot Mode"
                   ,choices = c("Normal Plot" = "normal_plot"
                                ,"Loop Mode" = "loop_mode"
                                ,"Side-by-Side Comparison" = "compare_mode"
                                ,"Overlay Data" = "overlay_data_mode"))
    ),
    column(
      width = 2,
      pickerInput(
        inputId = "lineplot_choose_plot_renderer",
        label = "Plot Renderer",
        choices = c("Interactive (plotly)" =  "plotly",
                    "Standard (ggplot2)" = "ggplot2")
      )
    )
  ),  
  br(),
  fluidRow(
    column(
      width = 6,
#-------------------------Input Dropdown Button---------------------------------
      dropdownButton(
        label = "Variables",
        icon = icon("sliders-h"),
        circle = FALSE,
        status = "dropdownbutton",
        size = "lg",
        # pickerInput(inputId = 'lineplot_xvar',
        #             label = 'x variable',
        #             choices = character()),
        div(id = "form",
            selectizeInput(inputId = 'lineplot_yvar',
                           label = NULL,
                           choices = character(),
                           multiple = TRUE,
                           options = list(
                             placeholder = "Select Variables",
                             plugins = list('remove_button')
                             )
                          )
            ),
        fluidRow(
          column(
            width = 6,
            actionButton("select_all", "Select All", onclick = js2)
          ),
          column(
            width = 6,
            actionButton("reset_input", "Reset")
          )
        )
      ) #endDropDown
    ) #end column width=6
  )#end FluidRow
  # Generating plots for normal plotting mode
  ,fluidRow(
      column(
        width = 12,
        conditionalPanel(
          condition = "input.lineplot_choose_plot_mode == 'normal_plot'",
          conditionalPanel(
            condition = "input.lineplot_choose_plot_renderer == 'plotly'",
            withSpinner(jqui_resizable(plotlyOutput("lineplot_plotly")))
          ),
          conditionalPanel(
            condition = "input.lineplot_choose_plot_renderer == 'ggplot2'",
            withSpinner(jqui_resizable(plotOutput("LinePlot")))
          )
        )
    ),
    # Creating plots for loop mode
    column(
      width = 12,
      conditionalPanel(
        condition = "input.lineplot_choose_plot_mode == 'loop_mode'",
        conditionalPanel(
          condition = "input.lineplot_choose_plot_renderer == 'plotly'",
          withSpinner(jqui_resizable(plotlyOutput("lineplot_loop_plotly")))
        ),
        conditionalPanel(
          condition = "input.lineplot_choose_plot_renderer == 'ggplot2'",
          withSpinner(jqui_resizable(plotOutput("LinePlot_loop")))
        )
      )
    ),
    # Creating plots for comparison mode
    column(
      width = 12,
      conditionalPanel(
        condition = "input.lineplot_choose_plot_mode == 'compare_mode'",
        withSpinner(jqui_resizable(plotOutput("Lineplot_Compare")))
      )
    )
  ),


fluidRow(
  column(
    width = 3,
    style = "padding:0px;",
    dropdown(
      inputId = "lineplot_dropdown_1",
      label = "Axis Options",
      circle = FALSE,
      width = "500px",
      #icon = icon("gear"),
      up = FALSE,
      #status = "primary",
      radioGroupButtons(inputId="line_axis_options",
                        label="Edit",
                        choices=c("Labels" = "Labels"
                                  ,"Range" = "Axis Range"
                                  ,"Size" = "Size"
                                  ,"Position" = "Position")),
      conditionalPanel(condition="input.line_axis_options == 'Labels'",
                       textInput(inputId="line_title", 
                                 label="Title", 
                                 value = ""),
                       textInput(inputId="line_xlabel", 
                                 label="X Label", 
                                 value = ""),
                       textInput(inputId="line_ylabel", 
                                 label="Y Label", 
                                 value = "")
      )
      ,conditionalPanel(condition="input.line_axis_options=='Axis Range'",
                        fluidRow(
                          column(width=4,
                                 numericInput(inputId="line_xaxis_min", 
                                              label="x-axis min",
                                              value=1)),
                          column(width=4,
                                 numericInput(inputId="line_xaxis_max", 
                                              label="x-axis max", 
                                              value=10)),
                          column(width=4,
                                 numericInput(inputId="line_xstep",
                                              label="x step",
                                              value=2))
                        ),#end fluidRow
                        fluidRow(
                          column(width=4,
                                 numericInput(inputId="line_yaxis_min",
                                              label="y-axis min", 
                                              value=1)),
                          column(width=4,
                                 numericInput(inputId="line_yaxis_max", 
                                              label="y-axis max", 
                                              value=10)),
                          column(width=4,
                                 numericInput(inputId="line_ystep",
                                              label="y step",
                                              value=2))
                        ),#end fluidRow
                        switchInput(inputId="line_axis_confirm", 
                                    label="Change Axis", 
                                    labelWidth='80px')
      )#end ConditionalPanel
      ,conditionalPanel(condition="input.line_axis_options == 'Size'"
                        ,numericInput(inputId = "line_title_text_size"
                                      ,label = "title font size"
                                      ,value = 22
                                      ,min = 1)
                        ,hr()
                        ,numericInput(inputId = "line_axis_title_size"
                                      ,label = "Axis label font size"
                                      ,value = 14
                                      ,min = 1)
                        ,numericInput(inputId = "line_axis_text_size"
                                      ,label = "Axis plot text label font size"
                                      ,value = 14
                                      ,min = 1)
      )
      ,conditionalPanel(condition="input.line_axis_options=='Position'"
                        ,sliderInput(inputId = "line_title_location"
                                     ,label = "Title Position"
                                     ,min = 0
                                     ,max = 1
                                     ,value = 0.5
                                     ,step = 0.1)
                        ,sliderInput(inputId = "line_xtitle_location"
                                     ,label = "x-axis Title Position"
                                     ,min = 0
                                     ,max = 1
                                     ,value = 0.5
                                     ,step = 0.1)
                        ,sliderInput(inputId = "line_ytitle_location"
                                     ,label = "y-axis Title Position"
                                     ,min = 0
                                     ,max = 1
                                     ,value = 0.5
                                     ,step = 0.1)
      ) #end conditional panel: Position
    )
  ),
  column(
    width = 3,
    style = "padding:0px;",
    dropdown(
      inputId = "lineplot_dropdown_2",
      label = "Line Options",
      circle = FALSE,
      width = "500px",
      #icon = icon("gear"),
      #status = "primary",
      radioGroupButtons(inputId="line_line_options",
                        label="Edit",
                        choices=c("Color" = "Color"
                                  ,"Type" = "Type"
                                  ,"Weight" = "Weight")),
      conditionalPanel(
        condition = "input.line_line_options == 'Color'",
        pickerInput(inputId = "choose_color_palette",
                    label = "Line Color Palette",
                    choices = c("viridis",
                                "magma",
                                "inferno",
                                "plasma",
                                "cividis",
                                "rocket",
                                "mako",
                                "turbo",
                                "custom"),
                    selected = "turbo",
                    choicesOpt = list(content = c("<img src = 'palettes/viridis.jpg' width=70px><div class='jhr'>viridis</div></img>",
                                                  "<img src = 'palettes/magma.jpg' width=70px><div class='jhr'>magma</div></img>",
                                                  "<img src = 'palettes/inferno.jpg' width=70px><div class='jhr'>inferno</div></img>",
                                                  "<img src = 'palettes/plasma.jpg' width=70px><div class='jhr'>plasma</div></img>",
                                                  "<img src = 'palettes/cividis.jpg' width=70px><div class='jhr'>cividis</div></img>",
                                                  "<img src = 'palettes/rocket.jpg' width=70px><div class='jhr'>rocket</div></img>",
                                                  "<img src = 'palettes/mako.jpg' width=70px><div class='jhr'>mako</div></img>",
                                                  "<img src = 'palettes/turbo.jpg' width=70px><div class='jhr'>turbo</div></img>",
                                                  "<img src = 'palettes/custom_icon.jpg' width=30px><div class='jhr'>custom</div></img>"
                                                  
                    ))
        ),
        conditionalPanel(
          condition = "input.choose_color_palette == 'custom'",
          uiOutput("line_color_options_popdown")
        )
      ),
      conditionalPanel(
        condition = "input.line_line_options == 'Type'",
        uiOutput("line_type_options_popdown")
      ),
      conditionalPanel(
        condition = "input.line_line_options == 'Weight'",
        sliderInput(inputId = "line_size_options",
                    label = "Size of Lines",
                    min = 0,
                    max = 3,
                    step = 0.2,
                    value = 1)
        ,prettyCheckbox(inputId = "line_show_dots",
                        label = "Show Points",
                        value = FALSE)
      )
    )
  ),
  column(
    width = 3,
    style = "padding:0px;",
    dropdown(
      inputId = "lineplot_dropdown_3",
      label = "Plot Background",
      circle = FALSE,
      width = "500px",
      right = TRUE,
      #icon = icon("gear"),
      #status = "primary",
      fluidRow(
        selectInput(
          inputId = "theme_output_line",
          label = "Background Theme", 
          choices = c("gray"
                      ,"bw"
                      ,"linedraw"
                      ,"light"
                      ,"minimal"
                      ,"classic"
                      ,"void"
                      ,"dark"))
      ),
      fluidRow(
        column(
          width = 6,
          div(style = "padding-top: 30px;", 
              prettyCheckbox(inputId = "line_panel_colorPicker_checkbox", 
                             label = "Change Plot Background Color", 
                             value = FALSE))
        ),
        column(
          width = 6,
          conditionalPanel(
            condition = "input.line_panel_colorPicker_checkbox",
            colourInput(inputId = "line_panel_colorPicker", 
                        label = "Select Color", 
                        value = "grey")
          )
        )     
      ),
      fluidRow(
        column(
          width = 6,
          div(style = "padding-top: 30px;", 
              prettyCheckbox(inputId = "line_plotBackground_color_change", 
                             label = "Change Plot Background Color", 
                             value = FALSE))
        ),
        column(
          width = 6,
          conditionalPanel(
            condition = "input.line_plotBackground_color_change",
            colourInput(inputId = "line_plotBackground_colorPicker", 
                        label = "Select Color", 
                        value = "grey",
                        allowTransparent = TRUE)
          )
        )
      )
    )
  ),
  column(
    width = 3,
    style = "padding:0px;",
    dropdown(
      inputId = "lineplot_dropdown_4",
      label = "Legend",
      circle = FALSE,
      width = "500px",
      right = TRUE,
      #icon = icon("gear"),
      #status = "primary",
      selectInput(inputId = "line_legend_position",
                  label = "Location of Legend",
                  choices = c("Left" = "left", 
                              "Right" = "right", 
                              "Top" = "top", 
                              "Bottom" = "bottom", 
                              "No Legend" = "none"),
                  selected = "right"),
      textInput(inputId = "line_legend_title",
                label = "Legend Title",
                value = ""),
      numericInput(
        inputId = "line_legend_title_size",
        label = "Legend Title Font",
        value = 12,
        min = 1
      ),
      numericInput(
        inputId = "line_legend_font_size",
        label = "Legend Text Size",
        value = 12,
        min = 1
      )
    )
  )
)
  ,br()
  ,fluidRow(
    column(
      width = 12,
      conditionalPanel(
        condition = "input.lineplot_choose_plot_mode == 'loop_mode'",
        fluidRow(
          column(
            width = 6,
            prettyRadioButtons(
              inputId = "loop_select_table",
              label = "Change:",
              choices = c("Parameters",
                          "Initial Conditions",
                          "Time"),
              inline = TRUE
            )
          ),
          column(
            width = 6,
            align = "right",
            div(
              actionButton(
                inputId = "loop_mode_execute",
                label = "Refresh"
              ),
              actionButton(
                inputId = "loop_mode_store_variables",
                label = "Store"
              )
            )
          )
        ),
        hr(),
        fluidRow(
          column(
            width = 12,
            conditionalPanel(
              condition = "input.loop_select_table == 'Parameters'",
              rHandsontableOutput(outputId = "loop_mode_parameters")
            ),
            conditionalPanel(
              condition = "input.loop_select_table == 'Initial Conditions'",
              rHandsontableOutput(outputId = "loop_mode_ICs")
            ),
            conditionalPanel(
              condition = "input.loop_select_table == 'Time'",
              fluidRow(
                column(
                  width = 3,
                  textInput("loop_start_time", "Start Time", "0")
                  ),
                column(
                  width = 3,
                  textInput("loop_end_time", "End Time", "100"),
                  
                ),
                column(
                  width = 3,
                  textInput("loop_time_step", "Step", "1")
                )
              )
            )
          )
        )
      ),
      conditionalPanel(
        condition = "input.lineplot_choose_plot_mode == 'compare_mode'",
        box(
          title = NULL,
          width = 12,
          collapsible = FALSE,
          fluidRow(
            column(
              width = 9,
              fluidRow(
                pickerInput(
                  inputId = "model_compare_num_models",
                  label = "Number of Models",
                  choices = c(2,3,4),
                  # choicesOpt = list(
                  #   style = "height: 38px;"
                  # )
                ),
                textInput(
                  inputId = "compare_models_num_row",
                  label = "Subplot Rows",
                  value = "1"
                ),
                textInput(
                  inputId = "compare_models_num_col",
                  label = "Subplot Columns",
                  value = "2"
                )
              ) 
            ),
            column(
              width = 3,
              align = "right",
              div(style = "padding-top:25px;",
                actionBttn(inputId = "run_compared_model",
                           label = "Solve Models")
              )
              
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 3,
              pickerInput(inputId = "compare_models_select_vars",
                          label = "Select variables to compare",
                          choices = c(),
                          multiple = TRUE)
            )
          ),
          fluidRow(
            column(
              width = 12,
              #DTOutput("compare_models_DT")
              rHandsontableOutput(outputId = "compare_models_DT")
            )
          )
        )
      )
    )
   ),


  # tags$head(tags$style(".btn-group.special {display:flex;} .special .btn {flex:1;}")),
  # tags$head(tags$style("#lineplot_dropdown_1_state {flex:1;}
  #                        #lineplot_dropdown_2_state {flex:1;}
  #                        #lineplot_dropdown_3_state {flex:1;}
  #                        #lineplot_dropdown_4_state {flex:1;}")),
  # tags$head(tags$style("#lineplot_dropdown_1_state {flex:1;}
  #                       #lineplot_dropdown_2_state {flex:1;}
  #                       #lineplot_dropdown_3_state {flex:1;}
  #                       #lineplot_dropdown_4_state {flex:1;}
  #                       #lineplot_dropdown_1 {width:100%;border:1px solid black;}
  #                       #lineplot_dropdown_2 {width:100%;border:1px solid black;}
  #                       #lineplot_dropdown_3 {width:100%;border:1px solid black;}
  #                       #lineplot_dropdown_4 {width:100%;border:1px solid black;}"))
    tags$head(tags$style("#shiny-tab-TAB_RUN_LINEPLOT {min-height: 1500px;}"))

   # tags$style(".footer{position:fixed;bottom:10px; width:100%;}"),
   # footer = tags$div(
   #   class = "footer",
   #   actionButton(
   #     inputId = "button_test",
   #     label = "test"
   #   )
   # )


)