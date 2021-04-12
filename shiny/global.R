# global variables and functions
GLOBAL <- list(gene = NULL, submitType = 'qry', 
               DEBUG = TRUE,
               TEST = TRUE)

setGLOBAL <- function(name, x) {
  if (!name %in% c('gene','submitType')) {
    stop('invalid name argument: ', name)
  }
  GLOBAL[[name]] <<- x
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


