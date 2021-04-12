# generate results for single 'gene'
getSingleGeneResults <- function(gene) {
  shinyjs::runjs("$('#please-wait').removeClass('hide');")
  
  
  myinput <- reactiveValuesToList(input)
  save(gene, myinput, file = 'a.RData')
  
  # query bcbet db and generate results (remove Sys.sleep when implemented)
  Sys.sleep(1)
  
  catn('PROCESSING GENE: ', gene)
  
  output$ResultsHeader <- renderUI({
    h3('Patient Analysis now for', gene)
  })
  
  output$plotSummary <- renderPlot({
    plot(1:10)
  })
  
  output$tableSummary <- renderTable(iris)
  
  shinyjs::runjs("$('#please-wait').addClass('hide');")
}

