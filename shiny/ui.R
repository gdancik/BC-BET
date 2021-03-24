library(shinyBS)
library(shinydashboard)
library(shinyjs)

source("ui-about.R")
source("ui-welcome.R")
source("ui-progress.R")

function(request) {

shinyUI(
  
  navbarPage(title = HTML('<a style = "color:white" href = "/">BC-BET</a>'),
             windowTitle = "Bladder Cancer Biomarker Evaluation Tool",
             id = "page", 
             header =  pleaseWait(),
               
    tabPanel('Home', 
             HTML("<link href='https://fonts.googleapis.com/css?family=Courgette' rel='stylesheet' type='text/css'>"),
             HTML("<script src='bcbet.js'></script>"),
             welcomePage),
    tabAbout,
    
    # activate shinyJS and include CSS
    useShinyjs(),
    includeCSS('www/ecsu.css')
      
  ) # end navbarPage
) # end shinyUI
  
}
               

