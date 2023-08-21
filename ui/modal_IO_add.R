shinyBS::bsModal(
  id = "modal_add_IO",
  title = NULL,
  trigger = "io_add_open_modal",
  size = "large",
  fluidRow(
    column(
      width = 3,
      div(
        style = "background-color:#F9F9F9;
                       border: 1px solid #c5c5c5;
                       border-radius: 12px;
                       padding: 10px 10px 10px 10px;",
        pickerInput(
          inputId = "CIO_IO_options",
          label = "Options",
          choices = c(
            "Flow In" = "FLOW_IN",
            "Flow Out" = "FLOW_OUT",
            "Flow Between Compartments" = "FLOW_BETWEEN",
            "Clearance" = "CLEARANCE",
            "Simple Diffusion" = "SIMPDIFF",
            "Facillitated Diffusion" = "FACILITATED_DIFF"
          )
        )
      ),
      conditionalPanel(
        condition = "input.CIO_IO_options == 'FLOW_BETWEEN'",
        br(),
        div(
          style = "background-color:#F9F9F9;
                       border: 1px solid #c5c5c5;
                       border-radius: 12px;
                       padding: 10px 10px 10px 10px;",
          checkboxInput(
            inputId = "CIO_flowbetween_split",
            label = "Split Flow",
            value = FALSE
          ),
          numericInput(
            inputId = "CIO_flowbetween_number_split",
            label = "Number of Splits",
            value = 2,
            min = 2,
            step = 1
          )
        )
      )
    ),
    column(
      width = 9,
      div(
        style = "background-color:#F9F9F9;
                       border: 1px solid #c5c5c5;
                       border-radius: 12px;
                       padding: 10px 10px 10px 10px;",
# Flow in ----------------------------------------------------------------------
        conditionalPanel(
          condition = "input.CIO_IO_options == 'FLOW_IN'",
          fluidRow(
            column(
              width = 3,
              style = "padding-right: 0px;",
              pickerInput(
                inputId = "CIO_flow_in_compartment",
                label = "Compartment",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-left: 0px;",
              pickerInput(
                inputId = "CIO_flow_in_species",
                label = "Species",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-right:0px;",
              textInput(
                inputId = "CIO_flow_in_rate_constant",
                label = "Flow Variable",
                value = "F_in_1"
              )
            ),
            column(
              width = 3,
              style = "padding-left:0px;",
              textInput(
                inputId = "CIO_flow_in_value",
                label = textOutput("CIO_fi_vo_text"),
                value = 0
              )
            )
          )
        ),
# Flow out ---------------------------------------------------------------------
        conditionalPanel(
          condition = "input.CIO_IO_options == 'FLOW_OUT'",
          fluidRow(
            column(
              width = 3,
              style = "padding-right:0px;",
              pickerInput(
                inputId = "CIO_flow_out_compartment",
                label = "Compartment",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-left:0px;",
              pickerInput(
                inputId = "CIO_flow_out_species",
                label = "Species",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-right:0px;",
              textInput(
                inputId = "CIO_flow_out_rate_constant",
                label = "Flow Variable",
                value = "F_out_1"
              )
            ),
            column(
              width = 3,
              style = "padding-left:0px;",
              textInput(
                inputId = "CIO_flow_out_value",
                label = textOutput("CIO_fo_vo_text"),
                value = 0
              )
            )
          )
        ),
# Flow Between -----------------------------------------------------------------
        conditionalPanel(
          condition = "input.CIO_IO_options == 'FLOW_BETWEEN'",
          # Compartment Out
          fluidRow(
            column(
              width = 3,
              style = "padding-right: 0px;",
              pickerInput(
                inputId = "CIO_flowbetween_compartment_out",
                label = "Flow Out Of",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-left:0px;",
              pickerInput(
                inputId = "CIO_flowbetween_species_out",
                label = "Species Out",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-right: 0px;",
              textInput(
                inputId = "CIO_flowbetween_flow_variable_out",
                label = "Flow Variable",
                value = "F_out_1"
              )
            ),
            column(
              width = 3,
              style = "padding-left: 0px;",
              textInput(
                inputId = "CIO_flowbetween_flow_value_out",
                label = textOutput("CIO_fb_vo_text"),
                value = 0
              )
            )
          ),
          hr(),
          # Flow in 1
          fluidRow(
            column(
              width = 3,
              style = "padding-right: 0px;",
              pickerInput(
                inputId = "CIO_flowbetween_compartment_in_1",
                label = "Flow Into",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-left: 0px;",
              pickerInput(
                inputId = "CIO_flowbetween_species_in_1",
                label = "Species In",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-right: 0px;",
              conditionalPanel(
                condition = "input.CIO_flowbetween_split",
                textInput(
                  inputId = "CIO_flowbetween_flow_variable_in_1",
                  label = "Flow Variable",
                  value = "F_in_1"
                )
              )
            ),
            column(
              width = 3,
              style = "padding-left: 0px;",
              conditionalPanel(
                condition = "input.CIO_flowbetween_split",
                textInput(
                  inputId = "CIO_flowbetween_flow_value_in_1",
                  label = textOutput("CIO_fb_sv1_text"),
                  value = 0
                )
              )
            )
          ),
          # Flow Split Renders
          fluidRow(
            column(
              width = 3,
              style = "padding-right:0px",
              uiOutput("CIO_flow_between_render_compartments")
            ),
            column(
              width = 3,
              style = "padding-left:0px;",
              uiOutput("CIO_flow_between_render_species")
            ),
            column(
              width = 3,
              style = "padding-right:0px",
              uiOutput("CIO_flow_between_render_flow_variables")
            ),
            column(
              width = 3,
              style = "padding-left:0px;",
              uiOutput("CIO_flow_between_render_flow_values")
            )
          )
        ),
# Clearance --------------------------------------------------------------------
        conditionalPanel(
          condition = "input.CIO_IO_options == 'CLEARANCE'",
          fluidRow(
            column(
              width = 3,
              style = "padding-right: 0px;",
              pickerInput(
                inputId = "CIO_clearance_compartment",
                label = "Compartment",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-left:0px",
              pickerInput(
                inputId = "CIO_clearance_species",
                label = "Species",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-right: 0px;",
              textInput(
                inputId = "CIO_clearance_rate_constant",
                label = "Flow Rate Variable",
                value = "ke_1"
              )
            ),
            column(
              width = 3,
              style = "padding-left: 0px",
              textInput(
                inputId = "CIO_clearance_value",
                label = textOutput("CIO_clearance_unit_text"),
                value = 0
              )
            )
          )
        ),
# Simple Diffusion -------------------------------------------------------------
        conditionalPanel(
          condition = "input.CIO_IO_options == 'SIMPDIFF'",
          fluidRow(
            column(
              width = 3,
              style = "padding-right: 0px",
              pickerInput(
                inputId = "CIO_simpdiff_compartment1",
                label = "Compartment",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-left: 0px",
              pickerInput(
                inputId = "CIO_simpdiff_species1",
                label = "Species",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-right: 0px",
              textInput(
                inputId = "CIO_simpdiff_rate_constant",
                label = "Diffusivity Coefficient",
                value = "PS_1"
              )
            ),
            column(
              width = 3,
              style = "padding-left: 0px",
              textInput(
                inputId = "CIO_simpdiff_value",
                label = textOutput("CIO_simpdiff_unit_text"),
                value = 0
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              style = "padding-right: 0px",
              pickerInput(
                inputId = "CIO_simpdiff_compartment2",
                label = "Compartment",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-left: 0px",
              pickerInput(
                inputId = "CIO_simpdiff_species2",
                label = "Species",
                choices = c()
              )
            )
          )
        ),
# Facilitated Diffusion --------------------------------------------------------
        conditionalPanel(
          condition = "input.CIO_IO_options == 'FACILITATED_DIFF'",
          fluidRow(
            column(
              width = 3,
              style = "padding-right: 0px;",
              pickerInput(
                inputId = "CIO_facilitatedDiff_compartment1",
                label = "From Compartment",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-left: 0px;",
              pickerInput(
                inputId = "CIO_facilitatedDiff_species1",
                label = "From Species",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-right: 0px;",
              textInput(
                inputId = "CIO_facilitatedDiff_Vmax",
                label = "Vmax",
                value = "fVmax_1"
              )
            ),
            column(
              width = 3,
              style = "padding-left: 0px;",
              textInput(
                inputId = "CIO_facilitatedDiff_Vmax_value",
                label = textOutput("CIO_fd_vmax_unit_text"),
                value = 0
              )
            )
          ),
          fluidRow(
            column(
              width = 3,
              style = "padding-right: 0px;",
              pickerInput(
                inputId = "CIO_facilitatedDiff_compartment2",
                label = "To Compartment",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-left: 0px;",
              pickerInput(
                inputId = "CIO_facilitatedDiff_species2",
                label = "To Species",
                choices = c()
              )
            ),
            column(
              width = 3,
              style = "padding-right: 0px;",
              textInput(
                inputId = "CIO_facilitatedDiff_Km",
                label = "Km",
                value = "fKm_1"
              )
            ),
            column(
              width = 3,
              style = "padding-left: 0px;",
              textInput(
                inputId = "CIO_facilitatedDiff_Km_value",
                label = textOutput("CIO_fd_km_unit_text"),
                value = 0
              )
            )
          )
        )
      )
    )
  ),
# Modal Footer -----------------------------------------------------------------
  hr(),
  fluidRow(
    column(
      width = 6,
      checkboxInput(
        inputId = "checkbox_modal_io_keep_open",
        label = "Keep Active on Add",
        value = FALSE
      )
    ),
    column(
      width = 6,
      align = "right",
      actionButton(inputId = "CIO_add_IO",
                   label = "Add")
    )
  )
)