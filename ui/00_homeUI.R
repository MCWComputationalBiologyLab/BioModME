Tab_home <-
  tabItem(
    tabName = "Tab_home",
    fluidRow(
      column(
        width = 4,
        align = "center",
        actionButton(inputId = "HOME_build_model",
                     label = "",
                     style = "display:block;
                              height: 200px;
                              width: 200px;
                              font-size: 22px;
                              color: #fff;
                              background-color: #337ab7;
                              background: url('CreateDatabase.png');
                              background-size: contain;
                              background-repeat: no-repeat;
                              background-position: center;
                              border-radius: 50%;
                              border: 3px solid #3c8dbc;")
      )
    )
  
)