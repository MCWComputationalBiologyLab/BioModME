#This tab corresponds to the "Create Database" Tab of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  July 28, 2020
#  Last Update: August 15, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------


TAB_CONTRIBUTIONS <- 
  tabItem(
    tabName = "TAB_CONTRIBUTIONS", 
    #h2("Contributions"),
    h2("More Information"),
    hr(),
    "To further information on our application/work please visit:",
    br(),
    br(),
    "Lab Homepage: ",
    tags$a(
      href = "https://mcw.marquette.edu/biomedical-engineering/computational-systems-biology-lab/biomodme.php", 
      target = "_blank", 
      "Visit Application Homepage at MCW/Marquette Website"
    ),
    br(),
    "GitHub: ",
    tags$a(
      href = "https://github.com/MCWComputationalBiologyLab/BioModME", 
      target = "_blank", 
      "Visit this Projects GitHub"
    ),
    br(),
    br(),
    h2("Authors/Contact"),
    hr(),
    "This application was made in conjuction with the Dash/Terhune labs at the 
    Medical College of Wisconsin.",
    br(),
    "Lead programmers in the development wereJustin Womack and Viren Shah.",
    br(),
    br(),
    "Ranjan Dash: rdash@mcw.edu",
    br(),
    "Scott Terhune: sterhune@mcw.edu",
    br(),
    "Justin Womack: jwomack@mcw.edu",
    br(),
    "Viren Shah: virshah@mcw.edu",
    br(), 
    br(),
    h2("Special Thanks to CSTI For Server Hosting"),
    hr(),
    fluidRow(
      column(
        width = 2,
        img(
          src = "images/ctsi.png",
          width = "95%",
          height = "95%"
        )
      ), 
      column(
        width = 10,
        p(" "),
        "We like with to thank the Clinical & Translational Science Institute 
        (CTSI) of Southeast Wisconsin for helping us host this application 
        online. A special thank you to Kent Brodie for helping build the Rshiny
        server and maintaining the application. Another special thank you to 
        George Kowalski for setting up the Jenkins bot allowing us an easy way
        to update changes to the application.
        "
       # p("This website was supported by the National Center for 
       #   Advancing Translational Sciences, National Institutes of 
       #   Health, Award Numbers UL1TR001436, TL1TR001437, 
       #   and KL2TR001438."
       # ),
       # p(
       #   "The content is solely the responsibility of the authors and 
       #   does not necessarily represent the official views of the NIH."
       # )
     ))
                              
) # end tabItem
