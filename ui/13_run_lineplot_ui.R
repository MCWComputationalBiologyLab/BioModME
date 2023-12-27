js2 <- paste0(c(
  "var selectinput = document.getElementById('lineplot_yvar');",
  "selectinput.selectize.setValue(-1, false);",
  "selectinput.selectize.selectall();",
  "$('#select + .selectize-control .item').removeClass('active');"),
  collapse = "\n")

TAB_RUN_LINEPLOT <- tabItem(
  tabName = "TAB_RUN_LINEPLOT",
  br(),
  fluidRow(
    column(
      width = 3,
      style = "padding: 0px;",
      # Variable Dropdown Button -----------------------------------------------
      dropdownButton(
        inputId = "lineplot_variable_dropdown_button",
        label = "Variables",
        icon = icon("sliders-h"),
        # size = "lg",
        circle = FALSE,
        div(
          id = "form",
          selectizeInput(
            inputId = 'lineplot_yvar',
            label = NULL,
            choices = character(),
            multiple = TRUE,
            options = list(placeholder = "Select Variables",
                           plugins = list('remove_button'))
          )
        ), 
        fluidRow(
          column(
            width = 12,
            align = "right",
            div(
              actionButton("select_all", "Select All", onclick = js2),
              actionButton("reset_input", "Reset")
            )
          )
        )
      ) #endDropDown
    ), #end column width=6
    column(
      width = 6,
      offset = 3,
      style = "padding: 0px;",
      align = "right",
      div(
        style = "display:inline-block; 
                 text_align:right;
                 padding-right:0px;",
        shinyWidgets::dropdownButton(
          inputId = "lineplot_download_dropdown",
          label = "Download", 
          circle = FALSE,
          icon = icon("download"),
          right = TRUE,
          # size = "lg",
          textInput(
            inputId ="lineplot_download_title",
            label= "Download Name",
            value = "Plot",
            width = NULL
          ),
          radioGroupButtons(
            inputId = "lineplot_download_radiobuttons",
            label = NULL,
            choices = c(".jpg", 
                        ".png", 
                        ".pdf"),
            individual = TRUE,
            checkIcon = list(
              yes = tags$i(class = "fa fa-circle", 
                           style = "color: steelblue"),
              no = tags$i(class = "fa fa-circle-o", 
                          style = "color: steelblue"))
          ),
          numericInput(
            inputId = "lineplot_download_width",
            label = "Width",
            value = 10
          ),
          numericInput(
            inputId = "lineplot_download_height",
            label = "Height",
            value = 5
          ),
          pickerInput(
            inputId = "lineplot_download_units",
            label = "Units",
            choices = c("in",
                        "cm",
                        "mm",
                        "px")
          ),
          numericInput(
            inputId = "lineplot_download_dpi",
            label = "dpi",
            value = 300
          ),
          downloadBttn(outputId="lineplot_download_plots",
                       label = "Download",
                       style = "unite",
                       color = "primary",
                       size = "sm",
                       block = FALSE,
                       no_outline = FALSE)
        )
      ),
      div(
        style = "display:inline-block; 
                 text_align:right;
                 padding-right: 0px;
                 padding-left: 0px",
        shinyWidgets::dropdownButton(
          inputId = "lineplot_options_dropdown",
          label = "Options", 
          circle = FALSE,
          right = TRUE,
          icon = icon("gear", verify_fa = FALSE),
          # size = "lg",
          selectizeInput(
            inputId = "lineplot_choose_plot_mode",
            label = "Choose Plot Mode",
            choices = c(
              "Standard" = "normal_plot",
              "Side-by-Side Comparisons" = "compare_mode"
            )
          ),
          selectizeInput(
            inputId = "lineplot_choose_plot_renderer",
            label = "Plot Renderer",
            choices = c("Interactive (plotly)" =  "plotly",
                        "Standard (ggplot2)" = "ggplot2")
          )
        )
      ),
      div(
        style = "display:inline-block; 
                 text_align:right;
                 padding-right: 0px;
                 padding-left: 0px",
        actionButton(
          inputId = "execute_run_model_on_viz_tab",
          label = "Resolve Model"
        )
      )
    )
  ),#end FluidRow
  # Plots --------------------------------------------------------------------
  # Generating plots for normal plotting mode
  fluidRow(
    column(
      width = 12,
      style = "padding:0px; border: 1px solid lightgrey;",
      conditionalPanel(
        condition = "input.lineplot_choose_plot_mode == 'normal_plot'",
        conditionalPanel(
          condition = "input.lineplot_choose_plot_renderer == 'plotly'",
          jqui_resizable(plotlyOutput("lineplot_plotly"))
        ),
        conditionalPanel(
          condition = "input.lineplot_choose_plot_renderer == 'ggplot2'",
          jqui_resizable(plotOutput("main_lineplot"))
        )
      )
    ),
    # Creating plots for comparison mode
    column(
      width = 12,
      conditionalPanel(
        condition = "input.lineplot_choose_plot_mode == 'compare_mode'",
        jqui_resizable(plotOutput("Lineplot_Compare"))
      )
    )
  ),
# Plot Option Dropdowns --------------------------------------------------------
  fluidRow(
    ## Axis Options ------------------------------------------------------------
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
        radioGroupButtons(
          inputId="line_axis_options",
          label = "Edit",
          choices = c(
            "Labels" = "Labels",
            "Range" = "Axis Range",
            "Size" = "Size",
            "Position" = "Position"
          )), 
        conditionalPanel(
          condition="input.line_axis_options == 'Labels'",
          textInput(
            inputId = "line_title",
            label = "Title",
            value = ""), 
          textInput(
            inputId = "line_xlabel",
            label = "X Label",
            value = "Time"), 
          textInput(
            inputId = "line_ylabel",
            label = "Y Label",
            value = "Concentration")
        ),
        conditionalPanel(
          condition="input.line_axis_options=='Axis Range'",
          fluidRow(
            column(
              width = 4,
              numericInput(
                inputId = "line_xaxis_min",
                label = "x-axis min",
                value = 1
              )
            ),
            column(
              width = 4,
              numericInput(
                inputId = "line_xaxis_max",
                label = "x-axis max",
                value = 10
              )
            ),
            column(
              width = 4,
              numericInput(
                inputId = "line_xstep",
                label = "x step",
                value = 2
              )
            )
          ), #end fluidRow
          fluidRow(
            column(
              width = 4,
              numericInput(
                inputId = "line_yaxis_min",
                label = "y-axis min",
                value = 1
              )
            ),
            column(
              width = 4,
              numericInput(
                inputId = "line_yaxis_max",
                label = "y-axis max",
                value = 10
              )
            ),
            column(
              width = 4,
              numericInput(
                inputId = "line_ystep",
                label = "y step",
                value = 2
              )
            )
          ), #end fluidRow
          switchInput(
            inputId = "line_axis_confirm",
            label = "Change Axis",
            labelWidth = '80px')
        ), #end ConditionalPanel
        conditionalPanel(
          condition="input.line_axis_options == 'Size'",
          numericInput(
            inputId = "line_title_text_size",
            label = "Title Font Size",
            value = 22,
            min = 1
          ),
          hr(),
          numericInput(
            inputId = "line_axis_title_size",
            label = "Axis Label Font Size",
            value = 14,
            min = 1
          ),
          numericInput(
            inputId = "line_axis_text_size",
            label = "Axis Font Size",
            value = 14,
            min = 1
          )
        ),
        conditionalPanel(
          condition="input.line_axis_options=='Position'",
          sliderInput(
            inputId = "line_title_location",
            label = "Title Position",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.1
          ),
          sliderInput(
            inputId = "line_xtitle_location",
            label = "x-axis Title Position",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.1
          ),
          sliderInput(
            inputId = "line_ytitle_location",
            label = "y-axis Title Position",
            min = 0,
            max = 1,
            value = 0.5,
            step = 0.1
          )
        ) #end conditional panel: Position
      )
    ),
    ## Line Options ------------------------------------------------------------
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
        radioGroupButtons(
          inputId = "line_line_options",
          label = "Edit",
          choices = c("Color" = "Color",
                      "Type" = "Type",
                      "Weight" = "Weight")),
        conditionalPanel(
          condition = "input.line_line_options == 'Color'",
          pickerInput(
            inputId = "choose_color_palette",
            label = "Line Color Palette",
            choices = c(
              "viridis",
              "magma",
              "inferno",
              "plasma",
              "cividis",
              "rocket",
              "mako",
              "turbo",
              "custom"
            ),
            selected = "turbo",
            choicesOpt = list(
              content =
                c(
                  "<img src = 'palettes/viridis.jpg'
                            width=70px><div class='jhr'>viridis</div></img>",
                  "<img src = 'palettes/magma.jpg'
                            width=70px><div class='jhr'>magma</div></img>",
                  "<img src = 'palettes/inferno.jpg'
                            width=70px><div class='jhr'>inferno</div></img>",
                  "<img src = 'palettes/plasma.jpg'
                            width=70px><div class='jhr'>plasma</div></img>",
                  "<img src = 'palettes/cividis.jpg'
                            width=70px><div class='jhr'>cividis</div></img>",
                  "<img src = 'palettes/rocket.jpg'
                            width=70px><div class='jhr'>rocket</div></img>",
                  "<img src = 'palettes/mako.jpg'
                            width=70px><div class='jhr'>mako</div></img>",
                  "<img src = 'palettes/turbo.jpg'
                            width=70px><div class='jhr'>turbo</div></img>",
                  "<img src = 'palettes/custom_icon.jpg'
                            width=30px><div class='jhr'>custom</div></img>"
                )
            )
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
          sliderInput(
            inputId = "line_size_options",
            label = "Size of Lines",
            min = 0,
            max = 3,
            step = 0.2,
            value = 1), 
          prettyCheckbox(
            inputId = "line_show_dots",
            label = "Show Points",
            value = FALSE)
        )
      )
    ),
    ## Plot Background ---------------------------------------------------------
    column(
      width = 3,
      style = "padding:0px;",
      dropdown(
        inputId = "lineplot_dropdown_3",
        label = "Plot Background",
        circle = FALSE,
        width = "500px",
        right = TRUE,
        fluidRow(
          selectInput(
            inputId = "theme_output_line",
            label = "Background Theme", 
            choices = c("gray",
                        "bw",
                        "linedraw",
                        "light",
                        "minimal",
                        "classic",
                        "void",
                        "dark"))
        ),
        fluidRow(
          column(
            width = 6,
            div(style = "padding-top: 30px;", 
                prettyCheckbox(
                  inputId = "line_panel_colorPicker_checkbox", 
                  label = "Change Plot Background Color",
                  value = FALSE))
          ),
          column(
            width = 6,
            conditionalPanel(
              condition = "input.line_panel_colorPicker_checkbox",
              colourInput(
                inputId = "line_panel_colorPicker", 
                label = "Select Color",
                value = "grey")
            )
          )     
        ),
        fluidRow(
          column(
            width = 6,
            div(style = "padding-top: 30px;", 
                prettyCheckbox(
                  inputId = "line_plotBackground_color_change", 
                  label = "Change Plot Background Color",
                  value = FALSE))
          ),
          column(
            width = 6,
            conditionalPanel(
              condition = "input.line_plotBackground_color_change",
              colourInput(
                inputId = "line_plotBackground_colorPicker", 
                label = "Select Color",
                value = "grey",
                allowTransparent = TRUE)
            )
          )
        )
      )
    ),
    ## Legend Options ----------------------------------------------------------
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
        selectInput(
          inputId = "line_legend_position",
          label = "Location of Legend",
          choices = c(
            "Left" = "left",
            "Right" = "right",
            "Top" = "top",
            "Bottom" = "bottom",
            "No Legend" = "none"
           ), 
          selected = "right"), 
        textInput(
          inputId = "line_legend_title",
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
  ),
  br(),
# Model Variables Box ----------------------------------------------------------
  fluidRow(
    column(
      width = 12,
      style = "padding: 0px;",
      box(
        id = "plot_box_change_vars",
        width = 12,
        title = "Model Variables",
        collapsible = TRUE,
        collapsed = TRUE,
        prettyRadioButtons(
          inputId = "RB_plot_change_variables_options",
          label = "Edit:",
          choices = c("Initial Conditions" = "ICs",
                      "Parameter Values" = "parameters",
                      "Time" = "time"),
          inline = TRUE,
          status = "primary",
          fill = TRUE
        ),
        conditionalPanel(
          condition = "input.RB_plot_change_variables_options == 'parameters'",
          fluidRow(
            column(
              width = 12,
              h5(shiny::tags$u("Change Model Parameters")),
              rHandsontableOutput(outputId = "plot_param_table")
            )
          )
        ),
        conditionalPanel(
          condition = "input.RB_plot_change_variables_options == 'time'",
          fluidRow(
            column(
              width = 12,
              div(
                style = "background-color:#F9F9F9;
                   border: 1px solid #c5c5c5;
                   border-radius: 12px;
                   padding: 10px 10px 10px 10px;",
                fluidRow(
                  column(
                    width = 12,
                    fluidRow(
                      textInput(
                        inputId = "plot_execute_time_start",
                        label = "Starting Time",
                        value = "0"),
                      textInput(
                        inputId = "plot_execute_time_end",
                        label = "End Time",
                        value = "10"),
                      textInput(
                        inputId = "plot_execute_time_step",
                        label = "Time Step",
                        value = "0.1"),
                      pickerInput(
                        inputId = "plot_execute_time_unit",
                        label = "Unit",
                        choices = measurements::conv_unit_options$duration
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.RB_plot_change_variables_options == 'ICs'",
          fluidRow(
            column(
              width = 12,
              rHandsontableOutput(outputId = "plot_var_table")
            )
          )
        )
      )
    )
  ),
# Import Data Box --------------------------------------------------------------
  fluidRow(
    column(
      width = 12,
      style = "padding: 0px;",
      box(
        id = "plot_box_import_data",
        width = 12,
        title = "Import Data",
        collapsible = TRUE,
        collapsed = TRUE,
        fluidRow(
          column(
            width = 3,
            fileInput("plot_data_import",
                      "Import Data")
          ),
          column(
            width = 3,
            pickerInput(
              inputId = "plot_data_import_x",
              label = "Time",
              choices = c()
            )
          ),
          column(
            width = 3,
            pickerInput(
              inputId = "plot_data_import_y",
              label = "y", 
              choices = c(),
              multiple = TRUE
            )
          ),
          column(
            width = 3,
            checkboxInput(
              inputId = "show_overlay_data",
              label = "Apply Overlay",
              value = FALSE
            )
          )
        ),
        fluidRow(
          column(
            width = 12, 
            rHandsontableOutput(
              outputId = "plot_import_data_table"
            )
          )
        )
      )
    )
  ),
    tags$head(tags$style("#shiny-tab-TAB_RUN_LINEPLOT {min-height: 1500px;}"))
)