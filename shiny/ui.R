library(shinyBS)
library(shinydashboard)
#library(shinydashboardPlus)
library(shinyjs)

source("ui-about.R")
source("ui-welcome.R")
source("ui-progress.R")

jsCode <- "shinyjs.setReadOnly = function(id) {
  document.getElementById(id).setAttribute('readonly', 'readonly');
}"

function(request) {

shinyUI(
  
    navbarPage(title = HTML('<a style = "color:white" href = "/app/BCBET">BC-BET</a>'),
             windowTitle = "Bladder Cancer Biomarker Evaluation Tool",
             id = "page", 
             header =  pleaseWait(),
               
    tabPanel('Home',
             HTML("<link href='https://fonts.googleapis.com/css?family=Courgette' rel='stylesheet' type='text/css'>"),
             HTML("<script src='bcbet.js'></script>"),
             #br(),
             div(id = 'homepage',
              welcomePage,
              downloadLink('downloadAllResults', 'Download')
             )
             ),
    
    tabPanel('Datasets',
             tags$iframe(style="height:600px; width:100%", src="datasets.pdf")
             ),
    
    tabAbout,
    
    # activate shinyJS and include CSS
    useShinyjs(),
    shinyjs::extendShinyjs(text = jsCode, functions = c('setReadOnly')),
    includeCSS('www/ecsu.css')
      
  ) # end navbarPage
) # end shinyUI
  
}
               

