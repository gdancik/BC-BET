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
  
  catn('insert = ', GLOBAL$insertSingle)
  
  settings <- singleGeneSettings()
  
  print(settings)
  
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

  catn('calling resetResultsPage...')
  resetResultsPage(input$geneInput)
  
}, ignoreInit = TRUE)


resetResultsPage <- function(selectedGene) {
  catn('in resetResultsPage...')
  setGLOBAL('submitType', 'btn')
  
  for (ch in REACTIVE_SEARCH$parameters) {
    REACTIVE_SEARCH$parameters[[ch]] <- NULL
  }
  
  catn('please wait...')
  shinyjs::runjs("$('#please-wait').removeClass('hide');")
  
  
  REACTIVE_SEARCH$gene <- selectedGene
  REACTIVE_SEARCH$parameters <- list(measure = input$measure, 
                                     pvalue = input$pvalue, 
                                     endpoint = input$endpoint, 
                                     cutpoint = input$cutpoint,
                                     treated = input$treated)
  
  geneHeader <- REACTIVE_SEARCH$gene
  lgh <- length(geneHeader)
  if (lgh > 1) {
    geneHeader <- paste0(paste0(lgh, ' genes: '), paste0(geneHeader[1:min(lgh,3)], collapse = ','))
    if (lgh > 3) {
      geneHeader <- paste0(geneHeader, ',...')
    }    
  }


  # generate page header
  l <- REACTIVE_SEARCH$parameters
  myheader <- h4('Biomarker evaluation of', geneHeader, 
     span('(', 
          paste0(names(l), ': ', l, collapse = ', '), 
          ')', style = 'font-size: 80%',
     '(', a(id = 'return_home', 'change'), ')'
     ), style = 'margin:0px; color:darkred;', )  
    
  if (length(selectedGene) == 1) {
      catn('selected single gene...')
      shinyjs::disable('btnGeneSearch')
    
      # adding/removing tabs causes issues with a blue line;
      # let's hide/show and insert tabs in server.R
      hideTab('page', 'MultiResults')
      
      if (!GLOBAL$insertSingle) {
        catn('inserting single results page')
        insertTab('page', tabResults, "Home", position = "after")
        setGLOBAL('insertSingle', TRUE)
      } else {
        catn('showing results page')
        showTab('page', 'Results') 
      }
      
      catn("getting single gene results...")
      getSingleGeneResults()
      catn('done')
      
      output$ResultsHeader <- renderUI({
        myheader
      })
      
      updateTextAreaInput(session, "multiGeneInput", value = "")
      catn('update tabset to Summary')
      toggleSurvivalPlots(input$cutpoint)
      updateTabsetPanel(session, 'page', selected = 'Results')
      updateTabsetPanel(inputId = 'resultsPage', selected = 'Summary')
  } else { # multiple genes selected
      hideTab('page', 'Results')
    
    if (!GLOBAL$insertMulti) {      
      insertTab('page', tabResultsMulti, "Home", select = TRUE, position = "after")
      setGLOBAL('insertMulti', TRUE)
    } else {
      showTab('page', 'MultiResults') 
    }
    
      output$MultiResultsHeader <- renderUI({
        myheader
      })
      
      catn('multi gene results!')
      getMultiGeneResults()
      updateTabsetPanel(session, 'page', selected = 'MultiResults')
  }
  
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

shinyjs::onclick('return_home', {
  updateNavbarPage(session, 'page', 'Home')
})
