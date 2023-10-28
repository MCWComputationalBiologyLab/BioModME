# Server for global options

# Units


observeEvent(input$GO_base_duration, {
  rv.UNITS$units.selected$Duration <- input$GO_base_duration
})

observeEvent(input$GO_base_energy, {
  rv.UNITS$units.selected$Energy <- input$GO_base_energy
})

observeEvent(input$GO_base_length, {
  rv.UNITS$units.selected$Length <- input$GO_base_length
})

observeEvent(input$GO_base_mass, {
  rv.UNITS$units.selected$Mass <- input$GO_base_mass
})

observeEvent(input$GO_base_volume, {
  rv.UNITS$units.selected$Volume <- input$GO_base_volume
})

observeEvent(input$GO_base_flow, {
  rv.UNITS$units.selected$Flow <- input$GO_base_flow
})

observeEvent(input$GO_base_count, {
  rv.UNITS$units.selected$Count <- input$GO_base_count
})

observeEvent(input$GO_species_unit_choice, {
  if (input$GO_species_unit_choice == "Mol") {
    rv.UNITS$units.selected$For.Var <- rv.UNITS$units.base$Count
    rv.UNITS$units.choices$For.Var <- rv.UNITS$units.choices$Count
  } else if (input$GO_species_unit_choice == "Mass") {
    rv.UNITS$units.selected$For.Var <- rv.UNITS$units.base$Mass
    rv.UNITS$units.choices$For.Var <- rv.UNITS$units.choices$Mass
  }
})
