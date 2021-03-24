##############################################################
# Handles bookmarking and GET requests involving
# 'gene' and 'page' parameters. 
#
# processQuery logic:
# 
# - The observer for 'btnGeneSearch' must set 'submitType'
#     to 'btn'; otherwise its value is 'qry'. This observer
#     also adds the results page, which triggers the 
#     'input$page' observer, which updates the query string, 
#     which triggers a call to 'processQuery()'
# 
# - Logic for handling gene change in processQuery():
#       - if 'submitType' is 'btn' or 'processing_qry', then
#             get results for the gene
#       - if 'submitType' is 'qry', (from qry change without the
#             button click), then resetResultsPage(), change
#            'submitType' to 'qry_processing', and call 
#             processQuery() again
#       - update 'geneInput' drop down if necessary
#
# - Observer for 'input$page' observer updates the query string
#     if appropriate; 
#
# Notes: updateTabsetPanel and updateSelectizeInput take
#        effect after all observers have finished; we
#        therefore cannot rely on updating the geneInput
#        and using the updated value in the same processQuery
#        call
##############################################################

source('server-singleGeneResults.R', local = TRUE)

##############################
# Some useful functions
##############################

# remove quotes from query string, e.g. "HRAS" becomes HRAS
getQueryStringNoQuotes <- function() {
  lapply(getQueryString(), function(x) {gsub('"', '', x)})
}

not_identical <- function(x,y) {
  !identical(x,y)
}

# returns TRUE if query should be updated (if there is a
# difference between relevant inputs and current query string)
update_bookmark <- function() {
  
  if (GLOBAL$submitType == 'processing_qry') {
    setGLOBAL('submitType', 'qry')
    return(FALSE)
  }
  
  s <- isolate(getQueryStringNoQuotes())
  r <- isolate(reactiveValuesToList(input))
  
  # check page parameter
  params <- c('page')
  for (p in params) {
    catn('\t', p, ':', 's=',s[[p]], ', r=', r[[p]])
    if (not_identical(s[[p]], r[[p]])) {
      return (TRUE)
    }
  }
  
  # check for change in gene
  if (not_identical(s$geneInput, GLOBAL$gene)) {
    return(TRUE)
  }
  
  return(FALSE)
  
}
 
###############################################################
# Handles bookmarking following changes to relevant inputs
###############################################################
observeEvent(input$page , {

  catn('respond to input$page...')
  
  if (update_bookmark()) {
    catn('\tupdate bookmark')
    session$doBookmark()
    return()
  }
  
  # We may end up here after processing a 
  # direct query string with a new 'geneInput'. 
  # In this case, the 'page' will be 'Results' 
  # yet the query parameter specifies a different page.
  # Update the page
  s <- getQueryStringNoQuotes() 
  r <- reactiveValuesToList(input)
  if (not_identical(s$page, r$page)) {
    updateTabsetPanel(session, 'page', s$page)
  }
  
}, ignoreInit = TRUE)

onBookmarked(function(url) {
  url <- gsub('.+\\?', '?', url)
  cat('onBookarked, url: ', url, '\n')
  updateQueryString(url, mode = 'push')
})

###############################################################
# Handles changes to the query string
###############################################################

# processes query
processQuery <- function() {
  s <- getQueryStringNoQuotes()
  u <- unlist(s)
  q <- paste0(names(u),'=',u, collapse = '&')
  
  catn("process query: ", q)
  
  r <- isolate(reactiveValuesToList(input))
  
  catn('genes (s, r, global) = ', s$geneInput, ',',
                                  r$geneInput, ',',
                                  GLOBAL$gene)
  
  # do we need to process a new gene?
  if (not_identical(s$geneInput, GLOBAL$gene)) {
   
    catn('submitType:', GLOBAL$submitType)
    
    if (GLOBAL$submitType %in% c('btn', 'processing_qry')) {
      #  user has clicked Evaluate 
      getSingleGeneResults(s$geneInput)
      setGLOBAL('gene', s$geneInput)
      if (GLOBAL$submitType == 'btn') {
        setGLOBAL('submitType', 'qry')
      }
    } else {
      # direct url entry
      catn('url has been changed manually')
      setGLOBAL('submitType', 'processing_qry')
      resetResultsPage()
      shinyjs::click('btnReprocessQuery')
      return()  
    }
    
    # update dropdown if necessary
    if (not_identical(s$geneInput, r$geneInput)) {
      catn('\tupdateselectizeinput')
      updateSelectizeInput(session, "geneInput", label = "Select a gene (begin typing for more)",
                           choices = sort(c("", "TP53", "HRAS", "CD24", "FGFR3")),
                           selected = s$geneInput)
    }
  }
    
  # update the page if necessary
  if (not_identical(s$page, r$page)) {
    catn('update tab')
    updateTabsetPanel(session, 'page', s$page)
  }
  
  catn('')
  
}

####################################################
# Observers that will process the query string
####################################################
queryStringReactive <- reactive(
  getQueryStringNoQuotes()
)

observeEvent(queryStringReactive(), {
  catn('queryStringReactive() has changed, calling processQuery()')
  processQuery()              
})

observeEvent(input$btnReprocessQuery,{
  catn('input$btnReprocessQuery, calling processQuery()')
  processQuery()
})



# bookmark only inputs based on 'x', along with 'page'
# dependendent on current input list
setBookmarkInclude <-function(x) {
  setBookmarkExclude(
    setdiff(  c(names(isolate(reactiveValuesToList(input))),
                "ResultsPage"),
            c('page', x)
    )
  )
}

setBookmarkInclude(NULL)

