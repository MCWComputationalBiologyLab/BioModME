# BioSysMod

BioSysMod is an application that is meant to streamline building biological computational models. It is an all-in-one tool that allows the user to build a model, solve for its mathematical equations, and plot all relevant features. 

The application can be accessed online at  https://jwomack7512.shinyapps.io/Model_Builder/. 

## Launch BioSysMod directly from R 
**Step 1: Install R and RStudio**

Before running the app you will need to have R and RStudio installed (tested with R 4.1.1 and RStudio 1.4.1717).  
Please check CRAN (<a href="https://cran.r-project.org/" target="_blank">https://cran.r-project.org/</a>) for the installation of R.  
Please check <a href="https://www.rstudio.com/" target="_blank">https://www.rstudio.com/</a> for the installation of RStudio.  

**Step 2: Install the R Shiny package and other packages required by shinyCircos**

Start an R session using RStudio and run these lines:  
```
load.lib<-c("shinydashboard", "shinydashboardPlus", "shiny","ggplot2","gridExtra","shinythemes",
            "shinyWidgets","shinyjs","DT","tidyverse","dplyr","rhandsontable","data.table","ggpmisc",
            "plotly","colourpicker","shinyBS","shinyjqui", "bsplus", "deSolve", "shinyFiles", "ggplot2"
            ,"gridExtra", "shinythemes", "huxtable", "Deriv")
        
install.lib <- load.lib[!load.lib %in% installed.packages()]
for (lib in install.lib) install.packages(lib, dependencies = TRUE)
sapply(load.lib, require, character = TRUE)           
```

**Step 3: Start the app** 

Download the zip file of code for this application from Github. Start an R session using RStudio, navigate to the downloaded unziped folder and run these lines:  
```
shiny::runApp()  
```
If you open the "ui.R" or "server.R" scripts in Rstudio, you will notice a "Run App" button at the top right of the script screen that can also be used to run the application.
