library(mongolite)
library(ggplot2)
library(tidyr)

source('graphPanelFunctions.R')

mongo_connect <- function(collection, user = "root", pass = "password",
                          host = "0.0.0.0:2000") {
  # Using plain-text
  URI = sprintf("mongodb://%s:%s@%s/", user, pass, host)
  mongo(url = URI, collection = collection, db = 'bcbet')
}


# generate plots of plotType (e.g., 'stage') for rendering
# in output$graphOutputId (e.g., output$tumor)
generatePlots <- function(plotType, graphOutputId) {
  shinyjs::runjs("$('#please-wait').removeClass('hide');")
  
  plot_prefix <- paste0('plot_', plotType)
  
  # may need to alter this for multi-gene input. 
  gene <- input$geneInput
  
  # get datasets (we may want to get this from mysql results)
  m <- mongo_connect('mskcc_clinical')
  command <- paste0('{"listCollections":1, ',
                    '"filter": {"name": {"$regex": "expr", "$options":""}},',
                    '"nameOnly": "true" }')
  
  collections <- m$run(command)
  
  if (collections$ok != 1) {
    message('Could not get collections')
    return()
  }
  
  datasets <- gsub('_.*$', '', collections$cursor$firstBatch$name)
  datasets <- sort(datasets)
  
  # Generate graph for each dataset
  
  qry <- paste0('{"gene": "', gene, '"}')
  
  count <- 1
  myplots <- vector(mode = 'list', 4)
  for (ds in datasets) {
    m <- mongo_connect(paste0(ds, '_expr'))
    x <- m$find(qry)
    m <- mongo_connect(paste0(ds, '_clinical'))
    y <- m$find()
    
    if (nrow(x) == 0 || is.null(y[[plotType]])) {
      next
    }
    
    cat('ds #', count, ' is ', ds, '\n')
    
    df <- data.frame(x = x$expr[[1]], y = y[[plotType]]) %>% drop_na()
    
    myplots[[count]] <- ggplot(df, aes(y,x,fill=y)) + geom_boxplot() + theme_linedraw() +
      labs(x = '', y = 'log2 expression') + guides(fill = 'none') + 
      ggtitle(ds)
    
    count <- count + 1
  }
  
  # create appropriate number of plot containers
  output[[graphOutputId]] <- renderUI({
    createPlotGrid(count-1, plot_prefix)
  })
  
  # note: for loop does not work! 
  # https://chasemc.github.io/post/the-subtleties-of-shiny-reactive-programming-lapply-and-for-loops/
  lapply(1:(count-1), function(i) {
    output[[paste0(plot_prefix,i)]] <- renderPlot({myplots[[i]]})
  })
  
  shinyjs::runjs("$('#please-wait').addClass('hide');")
 
}

# reactives for plot generation

tumorPlots <- reactive({
  generatePlots('tumor', 'graphOutputTumor')
})

gradePlots <- reactive({
  generatePlots('grade', 'graphOutputGrade')
})

stagePlots <- reactive({
  generatePlots('stage', 'graphOutputStage')
})


observeEvent(input$ResultsPage,{
  
  if (input$ResultsPage == 'Tumor') {
    tumorPlots()
    return()
  } else if (input$ResultsPage == 'Grade') {
    gradePlots()
    return()
  } else if (input$ResultsPage == 'Stage') {
    stagePlots()
    return()
  }
  
}, ignoreInit = TRUE)

