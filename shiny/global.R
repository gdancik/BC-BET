# global variables and functions

GLOBAL <- list(#gene = NULL, geneResults = NULL,
  submitType = 'qry', 
  DEBUG = TRUE,
  TEST = FALSE,
  url_params = 'page',
  insertSingle = FALSE,
  insertMulti = FALSE)

resetGLOBAL <- function() {
  #setGLOBAL('gene', NULL)
  setGLOBAL('submitType', 'qry')
  setGLOBAL('url_params', 'page')
  #setGLOBAL('geneResults', NULL)
}

setGLOBAL <- function(name, x) {
  #valid <- c('gene','submitType', 'url_params', 'geneResults')
  valid <- c('submitType', 'url_params', 'insertSingle', 'insertMulti')
  if (!name %in% valid) {
    stop('invalid name argument: ', name)
  }
  GLOBAL[[name]] <<- x
}

addGLOBALurl <- function(...) {
  params <- c(...)
  GLOBAL$url_params <<- unique(c(GLOBAL$url_params, params))
}  

# remove all url_params except '...'; no params are added
removeGLOBALurlExcept <- function(...) {
  params <- c(...)
  GLOBAL$url_params <<- intersect(GLOBAL$url_params, params)
}
  
if (!GLOBAL$DEBUG) {
# comment out for debugging
  cat <- function(...){invisible()}
  print <- function(...){invisible()}
}

wait <- function() {
  cat("Press a key to continue...")
  scan(what = character(), n = 1)
}

catn <- function(...) {
  cat(..., '\n')
}

