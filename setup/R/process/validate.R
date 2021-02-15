# Confirm that patient IDs match between expression and clinical
# data

# for now, only consider data sets with saved clinical data
files_clinical <- Sys.glob('../../data/clinical/*.RData')

if (length(files_clinical) == 0) {
  stop("no clinical files found")
}

# loads expression and clinical data, returns TRUE if
# IDs match, or FALSE otherwise
validateIDs <- function(f_clinical) {

  # extract data set name
  b <- basename(f_clinical)
  ds <- gsub('\\.RData', '', b)
  
  # create vector of expression files
  f_expr <- paste0('../../data/processed/', ds, '.RData')

  cat('Validating IDs for', ds, '...\n')
  
  # load data
  load(f_clinical)
  load(f_expr)
  
  # return TRUE/FALSE
  res <- all (  colnames(get(paste0(ds,'.expr'))) == 
           get(paste0(ds, '_clinical'))$id
  )
  
  setNames(res, ds)
  
}

s <- lapply(files_clinical, validateIDs)
s <- unlist(s)

if (any(!s)) {
  w <- which(s)
  stop('IDs do not match for: ',  paste0(names(s)[w], collapse = ', '))
}
