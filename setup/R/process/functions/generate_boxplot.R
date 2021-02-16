# generates boxplot from first 20 
# columns of X
generate_boxplot <- function(X, name) {
  file <- paste0('png/', name, '_', Sys.Date(), '.png')
  n <- min(20, ncol(X))
  png(file = file)
  boxplot(X[,1:n], main = name, ylab = 'log 2 expression')
  dev.off()
}

