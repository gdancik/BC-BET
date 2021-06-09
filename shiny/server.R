# BC-BET

library(shiny)
library(ggplot2)
library(DT)

library(RMariaDB)

source("ui-tabResults.R")

shinyServer(function(input, output, session) {

  source("server-geneSearch.R", local = TRUE)
  source("server-bookmarking.R", local = TRUE)
  source("server-plots.R", local = TRUE)
  source("server-download.R", local = TRUE)
  
  REACTIVE_SEARCH <- reactiveValues(gene = NULL, 
                                    results_de = NULL,
                                    results_survival = NULL)
  
  shinyjs::disable('btnGeneSearch')
  shinyjs::runjs("$('#please-wait').addClass('hide');")
  
  shinyjs::hide('downloadAllResults')
  
  if (GLOBAL$TEST) {
    updateQueryString('?_inputs_&geneInput="HRAS"&page="Results"',
                      mode = 'push')
  }
  
})



