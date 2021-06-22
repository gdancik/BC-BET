##########################################################
# Handlers for gene selection drop downs and inputs
##########################################################

##########################################################
# Handle Single Gene Search
##########################################################

singleGeneSettings <- reactive({
  list(geneInput = input$geneInput,
       measure = input$measure,
       pvalue = input$pvalue,
       endpoint = input$endpoint,
       cutpoint = input$cutpoint,
       treated = input$treated)
})

# enable/disable btnGeneSearch when relevant inputs change
observe({
  catn('\nobserveEvent geneInput...')
  
  settings <- singleGeneSettings()
  if (is.null(settings$geneInput) || identical(settings$geneInput, '')) {
    shinyjs::disable('btnGeneSearch')
    return()
  }

  # check geneInput, which does not reside in REACTIVE_SEARCH$parameters  
  if (!identical(settings$geneInput, REACTIVE_SEARCH$gene)) {
    shinyjs::enable('btnGeneSearch')
    return()
  }
  
  settings$geneInput <- NULL # we no longer want to consider this
  
  for (s in names(settings)) {
    if (!identical(settings[[s]], REACTIVE_SEARCH$parameters[[s]])) {
      shinyjs::enable('btnGeneSearch')
      return()
    }
  }
  shinyjs::disable("btnGeneSearch")
})


# click button for single gene search
observeEvent(input$btnGeneSearch,{
  cat("\nclicked btnGeneSearch, geneInput = ", isolate(input$geneInput), "\n")
  
  # do nothing if no valid gene is selected
  if (is.null(input$geneInput) | input$geneInput == "") return()

  setGLOBAL('submitType', 'btn')
  
  # clear multiGeneInput
  updateTextAreaInput(session, "multiGeneInput", value = "")
  shinyjs::disable('btnGeneSearch')
  
  resetResultsPage()
  
}, ignoreInit = TRUE)


resetResultsPage <- function() {
  
  for (ch in REACTIVE_SEARCH$parameters) {
    REACTIVE_SEARCH$parameters[[ch]] <- NULL
  }
  
  #resetGLOBAL()
  #catn('inputs are: ', names(reactiveValuesToList(input)))
  
  catn("removing tab...")
  #removeTab('page', 'Results')
  catn('inserting tab...')
  
  shinyjs::runjs("$('#please-wait').removeClass('hide');")
  
  if (is.null(REACTIVE_SEARCH$gene)) {
    insertTab('page', tabResults, "Home", position = "after")
  }
  
  REACTIVE_SEARCH$gene <- input$geneInput
  REACTIVE_SEARCH$parameters <- list(measure = input$measure, 
                                     pvalue = input$pvalue, 
                                     endpoint = input$endpoint, 
                                     cutpoint = input$cutpoint,
                                     treated = input$treated)
  
  output$ResultsHeader <- renderUI({
    
    l <- REACTIVE_SEARCH$parameters
    
    h4('Patient Analysis for', REACTIVE_SEARCH$gene, 
       span('(', 
            paste0(names(l), ': ', l, collapse = ', '), 
            ')', style = 'font-size: 80%'),
       style = 'margin:0px; color:darkred;')
  })
  
  getSingleGeneResults()
  
  #showTab('page', 'Results')
    
  updateTabsetPanel(inputId = 'page', selected = 'Results')

  toggleSurvivalPlots(input$cutpoint)
  
  shinyjs::runjs("$('#please-wait').addClass('hide');")
    
}

toggleSurvivalPlots <- function(cutpoint) {
  f <- showTab
  if (cutpoint == 'continuous') {
    f <- hideTab
  }
  
  f('resultsPage', 'Survival')
  f('resultsPage', 'Survival (LG/NMI)')
  f('resultsPage', 'Survival (HG/MI)')
}

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
  
  tolower(genes)
  
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
    shinyjs::show('invalidGeneOutput')
    updateTextAreaInput(session, 'invalidGeneOutput', value = paste(genes$invalid, collapse = '\n'))
    js$setReadOnly('invalidGeneOutput')
    output$multiInvalidGeneMsg <- renderUI({
      HTML("<span style = 'color:red'> Invalid genes detected</span>")
    })
  } else {
    output$multiInvalidGeneMsg <- renderUI({})
    shinyjs::hide('invalidGeneOutput')
  }
  
 
  if (length(genes$ids) == 0 || setequal(genes$ids, selected$geneID)) {
      shinyjs::disable('btnMultiGeneSearch')
  } else {
      shinyjs::enable('btnMultiGeneSearch')
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
