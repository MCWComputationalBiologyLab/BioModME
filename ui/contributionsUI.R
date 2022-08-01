#This tab corresponds to the "Create Database" Tab of the App
#-------------------------------------------------------------------------
#  Justin Womack
#  July 28, 2020
#  Last Update: August 15, 2020
#  ZSPH/MCW, Milwaukee, WI, USA
#-------------------------------------------------------------------------


TAB_Contributions <- tabItem(tabName = "TAB_Contributions", 
                             #h2("Contributions"),
                             
                             fluidRow(
                               column(
                                 width = 2,
                                 img(src="images/ctsi.png",
                                     height = "95%",
                                     width = "95%")
                               ),
                               column(
                                 
                                 width = 9,
                                 p(" "),
                                 p("This website was supported by the National Center for Advancing Translational Sciences, National Institutes of Health, Award Numbers UL1TR001436, TL1TR001437, and KL2TR001438."), 
                                 p("The content is solely the responsibility of the authors and does not necessarily represent the official views of the NIH.")
                               )
                             )

                             
                              
                              
) # end tabItem
