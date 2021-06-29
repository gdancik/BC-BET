# on multi gene search
observeEvent(input$btnMultiGeneSearch,{

  cat("clicked btnMultiGeneSearch, genes = ", input$multiGeneInput, "\n")

  genes <- getGenesFromMultiGeneInput()

  n <- length(genes$ids)
  
  if (n == 0) {
    return()
  } else if (n > 500) {
    msg <- paste0('Your list contains ', n, ' valid genes.\n')
    msg <- paste0(msg, '\nPlease restrict your list to no more than 500 genes.')
    shinyjs::alert(msg)
    return()
  } 
  
  updateSelectizeInput(session, "geneInput", choices = geneIDs, 
                                            selected = "", server = TRUE)

}, ignoreInit = TRUE)



##########################################################
# Handle Multiple Gene Search
##########################################################

observeEvent(input$multiGeneInput, {
  delay(500, validateGenes())
  
}, ignoreInit = TRUE, ignoreNULL = TRUE)


# returns list containing
#   invalid gene symbols and valid ids, or NULL if nothing is entered
getGenesFromMultiGeneInput <- function() {
  
  genes <- strsplit(trimws(input$multiGeneInput), split = '\\s+')[[1]]
  
  if (length(genes) == 0) {
    return(NULL)
  }
  
  genes_upper <- toupper(genes)
  all_genes <- toupper(VALID_GENES)
  
  m <- match(genes_upper, all_genes)
  invalid <- genes[is.na(m)]
  m <- m[!is.na(m)]
  valid <- genes[m]
  
  list(valid = valid, invalid = invalid)
  
}

validateGenes <- reactive({
  
  genes <- getGenesFromMultiGeneInput() 
  
  if (is.null(genes)) {
    shinyjs::disable('btnMultiGeneSearch')
    shinyjs::hide('invalidGeneOutput')
    output$multiInvalidGeneMsg <- renderUI({})
    return(NULL)
  }
  
  if (length(genes$invalid) > 0) {
    catn('we have invalid genes...')
    shinyjs::show('invalidGeneOutput')
    updateTextAreaInput(session, 'invalidGeneOutput', value = paste(genes$invalid, collapse = '\n'))
    js$setReadOnly('invalidGeneOutput')
    output$multiInvalidGeneMsg <- renderUI({
      HTML("<span style = 'color:red'> Invalid genes detected</span>")
    })
  } else {
    cat('no invalid genes...\n')
    output$multiInvalidGeneMsg <- renderUI({})
    shinyjs::hide('invalidGeneOutput')
  }
  
 
  # if (length(genes$ids) == 0 || setequal(genes$ids, selected$geneID)) {
  #     shinyjs::disable('btnMultiGeneSearch')
  # } else {
  #     shinyjs::enable('btnMultiGeneSearch')
  # }
  
})


observeEvent(input$multiGeneFile, {
  catn('observeEvent multiGeneFile...')
  if (is.null(input$multiGeneFile)) {
    return()
  }
  genes <- read.csv(input$multiGeneFile$datapath, nrows = 1000, header = FALSE, as.is = 1)
  if (ncol(genes) > 1) {
    shinyjs::alert('Invalid format: gene file should have a single column')
    return()
  }
  updateTextAreaInput(session, 'multiGeneInput', value = paste0(genes$V1, collapse = "\n"))
  
}, ignoreInit = TRUE)

