TAB_CREATE_CUSTOM_LAW <-
  tabItem(
    tabName = "TAB_CREATE_CUSTOM_LAW",
    fluidRow(
      column(
        width = 12,
        box(
          width = 12,
          fluidRow(
            column(
              width = 3,
              textInput(
                inputId = "TI_CC_law_name",
                label = "Law Name", 
                value = "CustomLaw1"
              )
            ),
            column(
              width = 9,
              textAreaInput(
                inputId = "TI_CC_law_description",
                label = "Description",
                value = "User defined custom law."
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 9, 
              textAreaInput(
                inputId = "TI_CC_enter_rate_law",
                label = "Rate Law",
                value = "",
                placeholder = "x1*p1*x2^2/(mod*y1)"
              )
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 3,
              tableLayout(
                labels = c("Reactants", "Products", "Modifiers"),
                widgets = list(
                  textInput("PI_CC_reactants", NULL, "", placeholder= "x1, x2"),
                  textInput("PI_CC_products", NULL, "", placeholder = "y1"),
                  textInput("PI_CC_modifiers", NULL, "", placeholder = "mod1")
                )
              )
            ),
            column(
              width = 9, 
              rHandsontableOutput("TO_CC_parameter_table")
            )
          ),
          hr(),
          fluidRow(
            column(
              width = 3,
              offset = 9,
              align = "right",
              actionButton(
                inputId = "bttn_store_custom_reaction",
                label = "Store"
              )
            )
          )
        )
      )
    )
)