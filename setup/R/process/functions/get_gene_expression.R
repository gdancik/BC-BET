rm(list = ls())

#' Gets gene level expression matrix keeping probes with highest
#' mean expression
#' 
#' @param X Expression matrix
#' @param pl A matrix of platform data, where first column contains
#'           the probes and second column contains the genes
#' @return A gene-level expression matrix
#' @note Regex only captures 'words'; will not find genes such as
#' TRA@ or IGL@ or Ig alpha 1-[alpha]2m (these are not valid gene symbols)
get_expression <- function(X, pl) {

  # we only care about platform data for probes in X
  pl <- pl[pl[,1] %in% rownames(X),]
  
  # get vector of unique genes
  genes <- unique(unlist(strsplit(pl[,2], ' /// ')))

  n <- length(genes)
  
  allX <- vector(mode = "list",length = length(genes))

  for (i in seq_along(genes)) {

      g1 <- genes[i]
  
      if (i%%1000 == 0) {
        cat('get expression for gene', i, '/',n, '\n')
      }
  
      # need negative lookaheads because we don't want to match hyphens 
      # (e.g., grep('\\bDDR1\\b') will match 'DDR1-AS1')
      g <- grep(paste0('(?!-)\\b',g1,'\\b(?!-)'),  
                pl[,2], perl = TRUE)

      if (length(g) == 0) {
        cat("not found: ", g1, '...\n')
      }
      
      probes <- pl[g,1]
      
      m <- match(probes, rownames(X))      
      xx <- X[m,,drop = FALSE]
      
      w <- 1
    
      if (nrow(xx) > 1) {
        w <- which.max(rowMeans(xx))
        xx <- xx[w, , drop = FALSE]
      }
      
    if (nrow(xx) == 1) {
      rownames(xx) <- g1  
    }
      
    allX[[i]] <- xx

  }

  names(allX) <- genes
  allX <- do.call(rbind, allX)

  allX
}


