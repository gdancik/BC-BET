#################################################
# Download GPL data from GEO and save
# ID and Symbol information

# Usage: RScript getPlatforms GPL96 GPL6102 ...
#   - (if no arguments are specified, all
#     platforms are updated)
#   - saves data to ../data/platforms/ relative
#       to the directory of this script
################################################

#' Sets the working directory to the execution directory of the 
#' current script (as determined from the RScript call or from RStudio)
#'
#' @return The working directory when the function is called

setwdToCurrentFileLocation <- function() {

  cwd <- getwd()
  
  # get command line args 
  args <- commandArgs()
  
  # parse command line and determine directory
  g <- grep('--file=', args)
  if (length(g) == 1) { 
      # running Rscript, get 'file' argument
      f <- args[g]
      d <- dirname(gsub('--file=','',f))
  } else if (args[1] == 'RStudio') {
      # running RStudio, get current file directly
      d <- dirname(rstudioapi::getSourceEditorContext()$path)
  } else {
    stop('Cannot determine location of current file')
  }
  
  d <- ifelse(d == '.', '', d)
  if (d != '') {
    print(paste('change directory to: ', d))
    setwd(d)
  }
  
  invisible(cwd)
        
}  

# get platform data

library(GEOquery)
library(dplyr)

wd <- setwdToCurrentFileLocation()

# get arguments; if no arguments are specified, get all platforms
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c('GPL96', 'GPL6102', 'GPL91', 'GPL570', 'GPL4060', 'GPL6947', 
            'GPL14951', 'GPL6480')
}

for (a in args) {
  
  cat('\n\ndownloading platform data:', a, '\n')
  pl <- Table(getGEO(a))

  if (a %in% c('GPL96', 'GPL570', 'GPL91')) {
    x <- dplyr::select(pl, 'ID', `Gene Symbol`) %>% 
      dplyr::filter (`Gene Symbol` != '')
  } else if (a %in% c('GPL6102', 'GPL6947')){
    x <- dplyr::select(pl, 'ID', 'Symbol') %>%
      dplyr::filter (Symbol != '')
  } else if (a == 'GPL6480') {
    x <- dplyr::select(pl, 'ID', 'GENE_SYMBOL') %>%
      dplyr::filter (GENE_SYMBOL != '')
  } else if (a == 'GPL4060') {
    # map gene symbols to GB ACC using org.Hs.eg.db
    library(org.Hs.eg.db)
    acc <- unique(pl$GB_ACC[pl$GB_ACC != ""])
    s <- AnnotationDbi::select(org.Hs.eg.db, keys = acc, 
                columns = c("SYMBOL"), keytype = "ACCNUM" )
    s <- s %>% filter(!is.na(SYMBOL))
    
    m <- match(s$ACCNUM, pl$GB_ACC)
  
    pl <- cbind(pl[m,], s)  
    x <- dplyr::filter(pl, SYMBOL!= '') %>% 
      dplyr::transmute(ID = ID, Symbol = SYMBOL)
      
  }else if (a %in% "GPL14951") {
    x <- dplyr::filter(pl, !is.na(Entrez_Gene_ID)) %>%
      dplyr::select('ID', 'Symbol')
  }else {
    stop('not implemented')
  }
  
  assign(a, x)
  file <-  paste0('../data/platforms/',a, '.RData')
  
  cat('saving:', file, '\n\n')
  save(list = a, file = file)
  
  }
  
setwd(wd) # restore working directory

