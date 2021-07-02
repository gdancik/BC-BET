# BC-BET

library(shiny)
library(ggplot2)
library(DT)

library(RMariaDB)

source("ui-tabResults.R")
source("ui-tabResultsMulti.R")

shinyServer(function(input, output, session) {

  source("server-geneSearch.R", local = TRUE)
  source("server-multiGeneSearch.R", local = TRUE)
  source('server-singleGeneResults.R', local = TRUE)
  source('server-multiGeneResults.R', local = TRUE)
  source("server-plots.R", local = TRUE)
  source("server-download.R", local = TRUE)
  
  REACTIVE_SEARCH <- reactiveValues(gene = NULL, 
                                    results_de = NULL,
                                    results_survival = NULL,
                                    parameters = NULL)
  
  shinyjs::disable('btnGeneSearch')
  shinyjs::runjs("$('#please-wait').addClass('hide');")
  
  shinyjs::hide('downloadAllResults')
  
  if (GLOBAL$TEST) {
    updateQueryString('?_inputs_&geneInput="HRAS"&page="Results"',
                      mode = 'push')
  }
  
  
  updateSelectizeInput(session, 'geneInput', choices = VALID_GENES, 
                       selected = 'FGFR3', server = TRUE,
                       options = list(maxOptions = 20)) 
  

  observe({
    shinyjs::onclick('link_parameters', shinyjs::toggle('div_parameters'))
  })  

  
  insertTab('page', tabResults, "Home", position = "after")
  hideTab('page', 'Results')
  
  insertTab('page', tabResultsMulti, "Home", select = TRUE, position = "after")
  hideTab('page', 'MultiResults')  
  
})



