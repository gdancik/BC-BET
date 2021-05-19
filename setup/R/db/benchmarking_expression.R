# used to compare mongo vs. R for storage of expression data

source('functions.R')
#' Loads data from specified datasets
#'
#' @param ds The name of the datasets
#' @return A list containing expression matrix X and clinical table Y
#' 
get_data <- function(ds) {
  load(paste0('../../data/processed/', ds, '.RData'))
  load(cfile <- paste0('../../data/clinical/', ds, '.RData'))
  
  list(X = get(paste0(ds,'.expr')),
       Y = get(paste0(ds, '_clinical')))
}

#' Add expression data to MySQL 'expression' table
#'
#' @param con The database connection (using dbConnect)
#' @param ds The name of the dataset
#' @param X The expression matrix, with gene names in rows and
#           sample_ids as column names
#' 




plot_it <- function(mongo_time, R_time) {
  df <- data.frame(mongo = mongo_time, R = R_time) * 1e-6
  m <- melt(df)
  ggplot(m, aes(x = variable, y = value, fill = variable)) + 
    geom_boxplot() +
    theme_linedraw() + ylab('time (milliseconds)') +
    xlab('method') + theme(legend.position = 'none') +
    ggtitle('Which is the fastest way to retreive expression data?')
}


library(mongolite)

addMongoData('mskcc')
addMongoData('cnuh')

# bench marking
# Note: if disconnecting, set gc to FALSE; using gc takes a lot
# of time and makes this method slower
fmongo <- function() {
  for (ds in c('cnuh', 'mskcc')) {
    m <- mongo_connect(collection = paste0(ds, '_expr'))
    g <- sample(GENES,1)
    qry <- paste0('{"gene": "', g, '"}')
    a <- m$find(query = qry, fields = '{"_id":0, "expr":1}' )
    #cat(col, '-', g, ':', a$expr[[1]][1:3], '\n')
    m$disconnect(gc=FALSE)
    
    m <- mongo_connect(collection = paste0(ds, '_clinical'))
    a <- m$find()
    m$disconnect(gc = FALSE)
    
  }
}

fmongo2 <- function() {
  g <- sample(GENES,1)
  qry <- paste0('{"gene": "', g, '"}')
  m <- mongo_connect(collection = 'cnuh_expr')
  X <- m$find(query = qry, fields = '{"_id":0, "expr":1}')
  
  m <- mongo_connect(collection = 'cnuh_clinical')
  Y <- m$find()
  df <- data.frame(expr = X$expr[[1]], Y)
}



fR2 <- function() {
  D <- get_data('cnuh')
  g <- sample(GENES,1)
  m <- match(g, rownames(D$X))
  a <- NULL
  if (!is.na(m)) {
    a <- D$X[m,]
  }
  df <- data.frame(expr = a, D$Y)
}


D <- get_data('cnuh')
GENES <- rownames(D$X)

tmongo <- microbenchmark(fmongo2(), times = 20)
tR <- microbenchmark(fR2(), times = 20)

plot_it(tmongo, tR)


fR <- function() {
  for (ds in c('cnuh', 'mskcc')) {
    D <- get_data(ds)
    g <- sample(GENES,1)
    m <- match(g, rownames(D$X))
    a <- NULL
    if (!is.na(m)) {
      a <- D$X[m,]
    }
  }
}

micro1 <- microbenchmark(fmongo(), times = 20)
micro2 <- microbenchmark(fR(), times = 20)

plot_it(micro1, micro2)
