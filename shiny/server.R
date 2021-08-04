# BC-BET

library(shiny)
library(ggplot2)
library(DT)
library(pheatmap)

source("ui-tabResults.R")
source("ui-tabResultsMulti.R")
source('mongo.R')

shinyServer(function(input, output, session) {

  VALID_GENES <- tryCatch({
    m <- mongo_connect('genes')
    m$find()$genes
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(VALID_GENES)) {
    shinyjs::html(id = 'homepage', html = '<h2> BC-BET is temporarily unavailable</h2><p>If this message persists, contact dancikg@easternct.edu</p>')
    shinyjs::runjs("$('#please-wait').addClass('hide');")
    return()
  }
  
  
  
  
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

})



