# BC-BET

library(shiny)
library(ggplot2)
library(DT)
library(pheatmap)

source("ui-tabResults.R")
source("ui-tabResultsMulti.R")
source('mongo.R')

shinyServer(function(input, output, session) {

  # include this here because shinyServer gets run on page refresh,
  # and these need to be reset

  ##################################################################
  # original approach with tabs -- but tab change does not always
  # happen on bioinformatics server
  ##################################################################
  
  # setGLOBAL('insertSingle', FALSE)
  # setGLOBAL('insertMulti', FALSE)
  # 
  # removeTab('page', 'MultiResults')
  # removeTab('page', 'Results')
  ##################################################################
 
  ################################################################## 
  # new approach, though we will show/hide tabs on start-up
  setGLOBAL('insertSingle', TRUE)
  setGLOBAL('insertMulti', TRUE)
  
  insertTab('page', tabResults, "Home", position = "after")
  hideTab('page', 'Results')

  insertTab('page', tabResultsMulti, "Home", select = TRUE, position = "after")
  hideTab('page', 'MultiResults')
  ##################################################################
  
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
                       selected = character(0), server = TRUE,
                       options = list(maxOptions = 20, placeholder = 'Enter any gene...')
                       ) 
  

  observe({
    shinyjs::onclick('link_parameters', shinyjs::toggle('div_parameters'))
  })  

})



