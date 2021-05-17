# used to compare mongo vs. R for storage of DE results

library(mongolite)
library(RMariaDB)
library(microbenchmark)
library(reshape2)
library(ggplot2)

source('functions.R')

plot_it <- function(mongo_time, mysql_time) {
  df <- data.frame(mongo = mongo_time, mysql = mysql_time) * 1e-6
  m <- melt(df)
  ggplot(m, aes(x = variable, y = value, fill = variable)) + 
    geom_boxplot() +
    theme_linedraw() + ylab('time (milliseconds)') +
    xlab('method') + theme(legend.position = 'none') +
    ggtitle('Which is the fastest way to retreive results?')
}




# bench marking
# Note: if disconnecting from mongo, set gc to FALSE; using gc takes a lot
# of time and makes this method slower

fmongo <- function() {
  g <- sample(GENES,1)
  qry <- paste0('{"gene": "', g, '"}')
  
  collection <- sample(COLLECTIONS, 1)
  
  m <- mongo_connect(collection = collection)
  X <- m$find(query = qry, fields = '{"_id":0}')
  X
}

fmysql <- function() {
  g <- sample(GENES,1)
  con <- dbConnect(MariaDB(), group = "BCBET")
  v <- sample(COLLECTIONS, 1)
  dbGetQuery(con, paste0('SELECT * FROM ',v, ' where gene = \"',g,'"'))
}

D <- get_data('cnuh')
GENES <- rownames(D$X)
COLLECTIONS <- c('tumor', 'stage', 'grade', 'dss', 'ba')

tmysql <- microbenchmark(fmysql(), times = 150)
tmongo <- microbenchmark(fmongo(), times = 150)

plot_it(tmongo, tmysql)

