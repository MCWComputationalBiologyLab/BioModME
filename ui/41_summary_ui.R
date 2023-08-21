
TAB_SUMMARY <- 
  tabItem(
    tabName = "TAB_SUMMARY", 
    fluidRow(
      id = "summary_fluidrow_one",
      column(
        width = 4,
        uiOutput("ReactionEquationsBox")
        ),
        column(
          width = 8,
          conditionalPanel(
            condition = "input.lineplot_choose_plot_renderer == 'plotly'",
            jqui_resizable(plotlyOutput("summary_plotly"))),
          conditionalPanel(
            condition = "input.lineplot_choose_plot_renderer == 'ggplot2'",
            jqui_resizable(plotOutput("summary_plot")))
        )), 
    fluidRow(
      id = "summary_fluidrow_two",
      column(
        width = 4,
        uiOutput("DifferentialEquationsBox")
      ),
      column(
        width = 4,
        DTOutput("summary_variable_table")),
      column(
        width = 4,
        DTOutput("summary_parameter_table"))
      )
)
