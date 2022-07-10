#This tab corresponds to the "Input/Output" tab
#-------------------------------------------------------------------------
#  Justin Womack
#  January 20, 2021
#  Last Update: January 20, 2021
#  MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------

TAB_InOut <- tabItem(tabName = "TAB_InOut",
                     fluidRow(
                       column(
                         width = 4,
                         pickerInput(inputId = "IO_pageOptions"
                                     ,label = "Options"
                                     ,choices = c("New" = "New",
                                                  "Edit" = "Edit",
                                                  "Delete" = "Delete")
                         )
                       ),
                       
                       column(
                         width = 4,
                         
                       )
                       
                     )
                     ,br()
                     ,conditionalPanel(
                       condition = "input.IO_pageOptions == 'Delete'",
                       box(
                         title = NULL
                         ,closable = FALSE
                         ,width = NULL
                         ,solidHeader = FALSE
                         ,collapsible = FALSE
                         ,radioGroupButtons(
                           inputId = "IO_edit_inOrOut_delete",
                           label = "Select Type",
                           choices = c("Input", "Output"),
                           individual = TRUE,
                           checkIcon = list(
                             yes = icon("ok",
                                        lib = "glyphicon"))
                         )
                         
                         ,fluidRow(
                           column(
                             width = 4
                             ,conditionalPanel(condition = "input.IO_edit_inOrOut_delete == 'Input'",
                                               pickerInput(inputId = "Inout_delete_input_eqn"
                                                           ,label = "Select Input Eqn Number to delete"
                                                           ,choices = c())
                                              )
                             ,conditionalPanel(condition = "input.IO_edit_inOrOut_delete == 'Output'",
                                               pickerInput(inputId = "Inout_delete_output_eqn"
                                                           ,label = "Select Output Eqn Number to delete"
                                                           ,choices = c())
                                              )
                              )
                           ,column(
                             width = 2
                             ,div(style = "display:inline-block; vertical-align:top; padding-top:32px"
                             ,actionButton(inputId = "Inout_button_delete_IO_eqn"
                                           ,label = "Delete"
                                           ,width = "145px")
                             )
                             )
                          )
                         ,fluidRow(
                           column(
                             width = 12,
                             prettyCheckbox(
                               inputId = "InOut_delete_eqn_delete_parameters",
                               label = "Delete Associated Parameters",
                               value = TRUE,
                               status = "info"
                             ),
                             bsTooltip(
                               "InOut_delete_eqn_delete_parameters",
                               "If checked, all parameters associated with this Input/Output will be removed from the model",
                               "right",
                               options = list(container = "body")
                             )
                           ) 
                         )
                         
                        )
                       )
  #------------------------EDIT Conditional Panel-------------------------------
                     ,conditionalPanel(
                       condition = "input.IO_pageOptions == 'Edit'"
                       ,"NonFunctional"
                       ,fluidRow(
                         column(
                           width = 3
                           ,box(
                             title = NULL
                             ,closable = FALSE
                             ,width = NULL
                             ,solidHeader = FALSE
                             ,collapsible = FALSE
                             ,pickerInput(inputId = "IO_selectIO2Edit"
                                          ,label = "Select Input/Output To Edit"
                                          ,choices = c()
                                          )
                           )
                         )
                         ,column(
                           width = 9
                             ,box(
                               title = NULL
                               ,closable = FALSE
                               ,width = NULL
                               ,solidHeader = FALSE
                               ,collapsible = FALSE
                               ,radioGroupButtons(
                                 inputId = "IO_edit_inOrOut",
                                 label = "Select Type",
                                 choices = c("Input", "Output"),
                                 individual = TRUE,
                                 checkIcon = list(
                                   yes = icon("ok",
                                              lib = "glyphicon"))
                               )
                               ,hr()
  # Edit - Input ---------------------------------------------------------------
                               ,conditionalPanel(
                                 condition = "input.IO_edit_inOrOut == 'Input'",
                                 verbatimTextOutput("InOut_edit_showInForVar")
                                 ,fluidRow(
                                   column(
                                     width = 4
                                    ,pickerInput(inputId = "InOut_edit_typeOfIn"
                                                 ,label = "Select Input Type"
                                                 ,choices = c("Rate" = "Rate"
                                                              ,"Simple Diffusion" = "simp_diff"
                                                              , "Synthesis by factor" = "Synthesis")
                                                 )
                                    )
                                 ) #end FluidRow
                                 ,hr()
                                 ,conditionalPanel(
                                   condition = "input.InOut_edit_typeOfIn == 'Rate'"
                                   ,p("Type the name of the rate constant you wish to put into the model.  Edit the value in Parameter tab.")
                                   ,hr()
                                   ,fluidRow(
                                     column(
                                       width = 3
                                      ,textInput(inputId = "In_rate_id_edit"
                                                 ,label = "Rate Constant Name"
                                                 ,value = ""
                                                 ,placeholder = "r_in")
                                       )
                                    ,column(
                                      width = 9
                                     ,checkboxInput(inputId = "In_rate_multiply_with_species_edit"
                                                    ,label = "Multiply rate constant by the concentration of this variable"
                                                    ,value = FALSE)
                                     )
                                   )#end fluidRow
                                 )#end conditional Panel 'rate'
                                 ,conditionalPanel(
                                   condition = "input.InOut_edit_typeOfIn == 'Synthesis'",
                                   fluidRow(
                                     column(
                                       width = 4,
                                       pickerInput(inputId = "IO_factor_for_syn_edit"
                                                   ,label = "Factor causing synthesis"
                                                   ,choices = c(),
                                                   options = pickerOptions(liveSearch = TRUE,
                                                                           liveSearchStyle = "startsWith")
                                       )
                                       ,textInput(inputId = "IO_rc_for_syn_edit"
                                                  ,label = "Rate Constant"
                                                  ,value = ""
                                                  ,placeholder = "Enter Corresponding rate constant")
                                     )
                                   )
                                 )
                                 ,actionButton(inputId = "Inout_edit_addInVarToDf"
                                               ,label = "Add Input"
                                               ,width = "145px")
                               )
  # Edit - Output --------------------------------------------------------------
                               ,conditionalPanel(
                                 condition = "input.IO_edit_inOrOut == 'Output'"
                                 ,verbatimTextOutput("InOut_edit_showOutForVar")
                                 ,fluidRow(
                                   column(
                                     width = 4
                                    ,pickerInput(inputId = "InOut_edit_typeOfOut"
                                                 ,label = "Select Output Type"
                                                 ,choices = c("Rate" = "Rate"
                                                              ,"Simple Diffusion" = "simp_diff"
                                                              , "Degradation by Enzyme" = "Enzyme_Degradation"
                                                              ,"Mass Action Removal" = "mass_action")
                                                 )
                                    )
                                 ) #end FluidRow
                                 ,hr()
                                 ,conditionalPanel(
                                   condition = "input.InOut_edit_typeOfOut == 'Rate'"
                                   ,p("Type the name of the rate constant you wish to put into the model.  Edit the value in Parameter tab.")
                                   ,hr()
                                   ,fluidRow(
                                     column(
                                       width = 3
                                      ,textInput(inputId = "Out_rate_id_edit"
                                                 ,label = "Rate Constant Name"
                                                 ,value = ""
                                                 ,placeholder = "r_out")
                                      )
                                     ,column(
                                       width = 9
                                      ,checkboxInput(inputId = "Out_rate_multiply_with_species_edit"
                                                    ,label = "Multiply rate constant by the concentration of this variable"
                                                    ,value = FALSE)
                                     )
                                   )#end fluidRow
                                 )#end conditional Panel 'rate'
                                 ,conditionalPanel(
                                   condition = "input.InOut_edit_typeOfOut == 'Enzyme_Degradation'"
                                   ,textInput(inputId = "enzyme_deg_km_edit"
                                              ,label = "Km"
                                              ,value = "",
                                              placeholder = "Km_1")
                                   ,conditionalPanel(condition = "!input.enzyme_deg_vmax_opt_edit"
                                                     ,textInput(inputId = "enzyme_deg_Vmax_edit"
                                                                ,label = "Vmax"
                                                                ,value = ""
                                                                ,placeholder = "Vmax_1")
                                                     )
                                   
                                   ,checkboxInput(inputId = "enzyme_deg_vmax_opt_edit"
                                                  ,label = "Expand Vm"
                                                  ,value = FALSE),
                                   conditionalPanel(
                                     condition = "input.enzyme_deg_vmax_opt_edit"
                                     ,textInput(inputId = "enzyme_deg_kcat_edit"
                                               ,label = "kcat"
                                               ,value = "")
                                     ,pickerInput(inputId = "enzyme_deg_enzyme_edit"
                                                 ,label = "Enzyme",
                                                 choices = c())
                                     )              
                                 )
                                 ,conditionalPanel(
                                   condition = "input.InOut_edit_typeOfOut == 'mass_action'"
                                   ,pickerInput(inputId = "MA_species_edit"
                                                ,label = "Species that remove substrate"
                                                ,choices = c()
                                                ,multiple = TRUE)
                                   ,textInput(inputId = "MA_deg_rate_constant_edit"
                                              ,label = "Rate Constant"
                                              ,value = "")
                                 )
                                 ,actionButton(inputId = "Inout_addOutVarToDf_edit"
                                               ,label = "Edit Output"
                                               ,width = "145px")
                              )
                            )
                          )
                        )
                      )
  #------------------------NEW Conditional Panel--------------------------------
                     ,conditionalPanel(
                       condition = "input.IO_pageOptions == 'New'"
                       ,fluidRow(
                         column(
                           width = 3,
                           box(
                             title = NULL,
                             closable = FALSE,
                             width = NULL,
                             solidHeader = FALSE,
                             collapsible = FALSE,
                             enable_dropdown = FALSE,
                             pickerInput(
                               inputId = "InOut_selectVar",
                                label = "Select Variable",
                                choices = c(),
                                options = pickerOptions(
                                  liveSearch = TRUE,
                                  liveSearchStyle = "startsWith",
                                  dropupAuto = FALSE
                                 )
                                )
                                      ) #end box
                                        
                       ) #endColumn
                       ,column(
                         width = 9
                         ,box(title = NULL,
                              closable = FALSE, 
                              width = NULL, 
                              solidHeader = FALSE,
                              collapsible = FALSE,
                              enable_dropdown = FALSE,
                              fluidRow(
                                column(
                                  width = 3,
                                  div(
                                    style = "padding-top:30px;",
                                    radioGroupButtons(
                                      inputId = "InOut_radio",
                                      label = NULL,
                                      choices = c("Input", "Output"),
                                      individual = TRUE,
                                      checkIcon = list(
                                        yes = icon("ok",
                                                   lib = "glyphicon"))
                                    )
                                  ) 
                                ),
                                column(
                                  width = 4,
                                  conditionalPanel(
                                    condition = "input.InOut_radio == 'Input'",
                                    pickerInput(inputId = "InOut_typeOfIn"
                                                ,label = "Select Input Type"
                                                ,choices = c("Simple Diffusion" = "simp_diff")
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.InOut_radio == 'Output'",
                                    pickerInput(
                                      inputId = "InOut_typeOfOut",
                                      label = "Select Output Type",
                                      choices = c("Simple Diffusion" = "simp_diff",
                                                  "Mass Action Removal" = "mass_action")
                                      )
                                  )
                                )
                              )
                              ,conditionalPanel(
                                condition = "input.InOut_radio == 'Input'",
                                hr(),
                                fluidRow(
                                  column(
                                    width = 4,
                                    conditionalPanel(
                                      condition = "input.InOut_typeOfIn == 'simp_diff'",
                                      "This is where simple diffusion goes"
                                    )
                                  )
                                ),
                                fluidRow(
                                  column(
                                    offset = 10,
                                    width = 2,
                                    align = "right",
                                    actionButton(
                                      inputId = "Inout_addInVarToDf",
                                      label = "Add Input",
                                      width = "145px"
                                      )
                                  )
                                )
                              ) #end Conditional Panel
                              #condition  = output
                              ,conditionalPanel(condition = "input.InOut_radio == 'Output'"
                                                ,verbatimTextOutput("InOut_showOutForVar")
                                                
                                                #condition = output -> rate
                                                ,conditionalPanel(condition = "input.InOut_typeOfOut == 'Rate'"
                                                                  ,p("Type the name of the rate constant you wish to put into the model.  Edit the value in Parameter tab.")
                                                                  ,hr()
                                                                  ,fluidRow(column(width = 3
                                                                                   ,textInput(inputId = "Out_rate_id"
                                                                                              ,label = "Rate Constant Name"
                                                                                              ,value = ""
                                                                                              ,placeholder = "r_out"))
                                                                            
                                                                  )#end fluidRow
                                                                  ,checkboxInput(inputId = "Out_rate_multiply_with_species"
                                                                                 ,label = "Concentration Dependent"
                                                                                 ,value = FALSE)
                                                )#end conditional Panel 'rate'
                                                ,conditionalPanel(condition = "input.InOut_typeOfOut == 'Enzyme_Degradation'"
                                                                  # ,pickerInput(inputId = "enzyme_deg_substrate"
                                                                  #              ,label = "Substrate"
                                                                  #              ,choices = c())
                                                                  ,textInput(inputId = "enzyme_deg_km"
                                                                             ,label = "Km"
                                                                             ,value = "",
                                                                             placeholder = "Km_1")
                                                                  ,conditionalPanel(condition = "!input.enzyme_deg_vmax_opt"
                                                                                    ,textInput(inputId = "enzyme_deg_Vmax"
                                                                                               ,label = "Vmax"
                                                                                               ,value = ""
                                                                                               ,placeholder = "Vmax_1"))
                                                                  
                                                                  ,checkboxInput(inputId = "enzyme_deg_vmax_opt"
                                                                                 ,label = "Expand Vm"
                                                                                 ,value = FALSE),
                                                                  conditionalPanel(condition = "input.enzyme_deg_vmax_opt"
                                                                                   ,textInput(inputId = "enzyme_deg_kcat"
                                                                                              ,label = "kcat"
                                                                                              ,value = ""
                                                                                              ,placeholder = "kcat_1")
                                                                                   ,pickerInput(inputId = "enzyme_deg_enzyme"
                                                                                                ,label = "Enzyme",
                                                                                                choices = c()))
                                                                  
                                                )
                                                ,conditionalPanel(condition = "input.InOut_typeOfOut == 'mass_action'"
                                                                  ,pickerInput(inputId = "MA_species"
                                                                               ,label = "Species that remove substrate"
                                                                               ,choices = c()
                                                                               ,multiple = TRUE)
                                                                  ,textInput(inputId = "MA_deg_rate_constant"
                                                                             ,label = "Rate Constant"
                                                                             ,value = "")
                                                )
                                                ,actionButton(inputId = "Inout_addOutVarToDf"
                                                              ,label = "Add Output"
                                                              ,width = "145px")
                                                )
                         )#end box
                       )
                       ) #end fluidRow
                     )
                     
                       ,fluidRow(column(width = 12
                                         ,box(title = "Logs"
                                                  ,solidHeader = FALSE
                                                  ,collapsible = FALSE
                                                  ,closable = FALSE
                                                  ,width = 12
                                                  ,htmlOutput(outputId = "IO_Display_Logs")
                                                  )
                                )
                       )
                     
)#end tabitem