TAB_GLOBAL_OPTIONS <- 
  tabItem(
    tabName = "TAB_GLOBAL_OPTIONS",
    tabBox(
      width = 12,
      title = NULL,
      tabPanel(
        title = "Unit Definition",
        "Options below define the base unit that the application will use when
        generating units for parameters, speices, or other variables. These 
        values can be changed from the base in the variables respective table.",
        fluidRow(
          column(
            width = 5,
            pickerInput(
              inputId = "GO_base_duration",
              label = "Duration",
              choices = c("s", "min"),
              selected = "min"
            ),
            pickerInput(
              inputId = "GO_base_energy",
              label = "Energy",
              choices = ENERGY_CHOICES,
              selected = "kJ"
            ),
            pickerInput(
              inputId = "GO_base_length",
              label = "Length",
              choices = LENGTH_CHOICES,
              selected = "m"
            ),
            pickerInput(
              inputId = "GO_base_mass",
              label = "Mass",
              choices = MASS_CHOICES,
              selected = "g"
            ),
            pickerInput(
              inputId = "GO_base_volume",
              label = "Volume",
              choices = VOLUME_CHOICES,
              selected = "L"
            ),
            pickerInput(
              inputId = "GO_base_flow",
              label = "Flow",
              choices = FLOW_CHOICES,
              selected = "l_per_min"
            ),
            pickerInput(
              inputId = "GO_base_count",
              label = "Count",
              choices = COUNT_CHOICES,
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
    )
  )