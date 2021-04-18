library(mongolite)

mongo_connect <- function(collection, user = "root", pass = "password",
                          host = "0.0.0.0:2000") {
  # Using plain-text
  URI = sprintf("mongodb://%s:%s@%s/", user, pass, host)
  mongo(url = URI, collection = collection, db = 'bcbet')
}

output$plotDataset <- renderPlot({
  
  # may need to alter this for multi-gene input. this also needs to be reactive
  gene <- input$geneInput
  
  
  qry <- paste0('{"gene": "', gene, '"}')
  m <- mongo_connect('mskcc_expr')
  x <- m$find(qry)
  
  m <- mongo_connect('mskcc_clinical')
  y <- m$find()
  
  s <- split(x$expr[[1]], y$tumor)
  boxplot(s, main = paste0('Tumor vs normal for ', gene))
  
})
