shinyBS::bsModal(
  id = "modal_download_differential_equations",
  title = NULL,
  trigger = "bttn_diffeq_download_modal",
  size = "large",
  pickerInput(
    inputId = "PI_dde_choose_download_type",
    label = "Download as:",
    choices = c("PlainText" = "txt",
                "mathML" = "mathml", 
                "Latex" = "latex")
  ),
  hr(),
  conditionalPanel(
    condition = "input.PI_dde_choose_download_type == 'txt'",
    "text"
  ),
  conditionalPanel(
    condition = "input.PI_dde_choose_download_type == 'mathml'",
    "mathml"
  ),
  conditionalPanel(
    condition = "input.PI_dde_choose_download_type == 'latex'",
    "latex"
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
  "))
)

