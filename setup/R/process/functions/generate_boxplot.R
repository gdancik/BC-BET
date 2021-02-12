# generates boxplot from first 20 
# columns of X
generate_boxplot <- function(X, name) {
  file <- paste0('png/', name, '.png')
  png(file = file)
  boxplot(X[,1:20], main = name, ylab = 'log 2 expression')
  dev.off()
}

