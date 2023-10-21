shinyBS::bsModal(
  id = "modal_download_differential_equations",
  title = NULL,
  trigger = "bttn_diffeq_download_modal",
  size = "large",
  fluidRow(
    column(
      width = 4,
      class = "custom-sidebar",
      pickerInput(
        inputId = "PI_dde_choose_download_type",
        label = "Download as:",
        choices = c("PlainText" = "txt",
                    "mathML" = "mathml", 
                    "Latex" = "latex")
      ),
      conditionalPanel(
        condition = "input.PI_dde_choose_download_type == 'mathml'",
        pickerInput(
          inputId = "PI_dde_mathml_selection",
          label = "Select equation:",
          choices = c()
        )
      )
    ),
    column(
      width = 8,
      conditionalPanel(
        condition = "input.PI_dde_choose_download_type == 'txt'",
        div(class = "scrollable-output",
          verbatimTextOutput("vTO_displayEquations_txt")
        )
      ),
      conditionalPanel(
        condition = "input.PI_dde_choose_download_type == 'mathml'",
        div(class = "scrollable-output",
          verbatimTextOutput("vTO_displayEquations_mathml")
        )
      ),
      conditionalPanel(
        condition = "input.PI_dde_choose_download_type == 'latex'",
        div(class = "scrollable-output",
          verbatimTextOutput("vTO_displayEquations_latex")
        )
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 12,
      align = "right",
      downloadButton(
        outputId = "dbttn_download_diffequations_specific",
        label = "Download"
      )
    )
  ),
  # CSS for Modal
  tags$style(HTML("
    #modal_download_differential_equations .modal-dialog {
      max-width: 1200px;
    }
    #modal_download_differential_equations .modal-footer{
      display:none;
     }
  ")),
  tags$style("
  .custom-sidebar {
    background-color: #f5f5f5; /* Gray background */
    border-radius: 5px; /* Curved corners */
    padding: 10px; /* Some padding to space out the content */
  }
"),
tags$style("
  .scrollable-output {
    max-height: 400px;  /* You can adjust this value as needed */
    overflow-y: auto;   /* Enable vertical scrolling */
    border: 1px solid #ccc;
    padding: 10px;
  }
")

)

