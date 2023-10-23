TAB_DOCUMENTATION <- tabItem(
  
  tabName = "TAB_DOCUMENTATION",
  
  h2("Documentation"),
  hr(),
  "Our current documentation is availble in an online format, word document, and
  pdf that can be seen/downloaded in the iFrame below.",
  br(),
  br(),
  "ReadTheDocs: ",
  tags$a(
    href = "https://biomodme.readthedocs.io/", 
    target = "_blank", 
    "Documentation webpage hosted at ReadTheDocs"
  ),
  br(),
  "Word Document: ",
  tags$a(
    href = "www/documentation/BioModME_Supplement_v1.1_0.docx", 
    download = "BioModME_Supplement_v1.1_0.docx", 
    tags$i(class = "fa fa-file-word-o", style = "margin-right: 5px;"), 
    "Download Documentation"
  ),
  br(),
  br(),
  tags$iframe(
    style = "height:90vh; width:100%; scrolling=yes",
    src = "./documentation/BioModME_Supplement_v1.1_0.pdf"
  ),
  "Additionally, please see the following links for more information or other
  downloading methods:",
  br(),
  
  


  
)