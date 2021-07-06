
##########################################################
# Handle Multiple Gene Search
##########################################################

# on multi gene search
observeEvent(input$btnMultiGeneSearch,{

  cat("clicked btnMultiGeneSearch, genes = ", input$multiGeneInput, "\n")

  genes <- getGenesFromMultiGeneInput()

  n <- length(genes$valid)
  
  if (n == 0) {
    return()
  } else if (n > 500) {
    msg <- paste0('Your list contains ', n, ' valid genes.\n')
    msg <- paste0(msg, '\nPlease restrict your list to no more than 500 genes.')
    shinyjs::alert(msg)
    return()
  } 
  
  #save(genes, VALID_GENES, file = 'look.RData')
  m <- match(toupper(genes$valid), toupper(VALID_GENES))
  selected <- VALID_GENES[m]
  
  cat('you selected: ', selected, '\n')
  
  if (n == 1) {
    updateSelectizeInput(session, "geneInput", choices = VALID_GENES, 
                         selected = selected, server = TRUE)
  }
  
  # from server-geneSearch
  resetResultsPage(selected)
  
  
}, ignoreInit = TRUE)


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
  
  m <- genes_upper %in% all_genes
  
  invalid <- genes[!m]
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
    shinyjs::html('invalid_gene_label', 'Invalid genes')
    shinyjs::show('invalidGeneOutput')
    shinyjs::removeClass('invalid_gene_label', 'green')
    shinyjs::addClass('invalid_gene_label', 'red')
    updateTextAreaInput(session, 'invalidGeneOutput', 
                        value = paste(genes$invalid, collapse = '\n'))
    js$setReadOnly('invalidGeneOutput')
    output$multiInvalidGeneMsg <- renderUI({
      
    })
  } else if (length(genes$valid) > 0) {
    shinyjs::show('invalidGeneOutput')
    shinyjs::html('invalid_gene_label', 'All genes are valid')
    shinyjs::removeClass('invalid_gene_label', 'red')
    shinyjs::addClass('invalid_gene_label', 'green')
    
    updateTextAreaInput(session, 'invalidGeneOutput', 
                        value = '\n\nInvalid genes will be listed here')
    
    js$setReadOnly('invalidGeneOutput')
  } else {
    cat('no invalid genes...\n')
    output$multiInvalidGeneMsg <- renderUI({})
    shinyjs::hide('invalidGeneOutput')
  }
  
 
  if (length(genes$valid) > 0) {
    shinyjs::enable('btnMultiGeneSearch')
  } else {
    shinyjs::disable('btnMultiGeneSearch')
  }

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




observeEvent(input$loadExampleLink, {
  
  # KEGG Bladder Cancer Pathway Genes:
  # https://www.genome.jp/dbget-bin/www_bget?pathway:map05219
  # https://www.genome.jp/dbget-bin/get_linkdb?-t+orthology+path:map05219
  value <- c('TYMP\nMMP1\nMMP2\nMMP9\nCDK4\nHRAS\nEGF\nEGFR\nBRAF\nRAF1\nMAP2K1\nMAP2K2\nMAPK1\nMYC\nRPS6KA5\nTP53\nCCND1\nERBB2\nFGFR3\nVEGFA\nCDH1\nSRC\nRB1\nE2F3\nCDKN2A\nCDKN1A\nMDM2\nKRAS\nNRAS\nHBEGF\nDAPK1\nARAF\nE2F2\nRASSF1\nCXCL8\nTHBS1\nE2F1\nUPK3A')
  updateTextAreaInput(session, "multiGeneInput", value = value)
}) 