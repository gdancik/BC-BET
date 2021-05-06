# BC-BET

library(shiny)
library(ggplot2)

library(RMariaDB)

source("ui-tabResults.R")

shinyServer(function(input, output, session) {

  source("server-geneSearch.R", local = TRUE)
  source("server-bookmarking.R", local = TRUE)
  source("server-plots.R", local = TRUE)
  source("server-download.R", local = TRUE)
  
  shinyjs::disable('btnGeneSearch')
  shinyjs::runjs("$('#please-wait').addClass('hide');")
  
  if (GLOBAL$TEST) {
    updateQueryString('?_inputs_&geneInput="HRAS"&page="Results"',
                      mode = 'push')
  }
  
})
