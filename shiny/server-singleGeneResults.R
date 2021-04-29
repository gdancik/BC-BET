# generate results for single 'gene'
getSingleGeneResults <- function(gene) {
  shinyjs::runjs("$('#please-wait').removeClass('hide');")
  
  myinput <- reactiveValuesToList(input)
  
  # query bcbet db and generate results (remove Sys.sleep when implemented)
#  Sys.sleep(1)
  
  catn('PROCESSING GENE: ', gene)
  
  output$ResultsHeader <- renderUI({
    h4('Patient Analysis for', gene, style = 'margin:0px; color:darkred;')
  })
  
  output$plotSummary <- renderPlot({
    plot(1:10)
  })
  
  output$tableSummary <- renderTable(iris)
  
  shinyjs::runjs("$('#please-wait').addClass('hide');")
}

