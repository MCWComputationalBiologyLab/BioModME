TAB_GLOBAL_OPTIONS <- 
  tabItem(
    tabName = "TAB_GLOBAL_OPTIONS",
    fluidRow(
      column(
        width = 5,
        pickerInput(
          inputId = "GO_base_duration",
          label = "Duration",
          choices = measurements::conv_unit_options$duration,
          selected = "min"
        ),
        pickerInput(
          inputId = "GO_base_energy",
          label = "Energy",
          choices = measurements::conv_unit_options$energy,
          selected = "kJ"
        ),
        pickerInput(
          inputId = "GO_base_length",
          label = "Length",
          choices = measurements::conv_unit_options$length,
          selected = "m"
        ),
        pickerInput(
          inputId = "GO_base_mass",
          label = "Mass",
          choices = measurements::conv_unit_options$mass,
          selected = "g"
        ),
        pickerInput(
          inputId = "GO_base_volume",
          label = "Volume",
          choices = measurements::conv_unit_options$volume,
          selected = "L"
        ),
        pickerInput(
          inputId = "GO_base_flow",
          label = "Flow",
          choices = measurements::conv_unit_options$flow,
          selected = "l_per_min"
        ),
        pickerInput(
          inputId = "GO_base_count",
          label = "Count",
          choices = measurements::conv_unit_options$count,
          selected = "mol"
        ),
      ),
      column(
        width = 5,
        radioButtons(
          inputId = "GO_species_unit_choice",
          label = "Species Concentration:",
          choices = c("Mol", "Mass"),
          inline = TRUE
        )
      )
    )
  )