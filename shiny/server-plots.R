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

# returns a data frame combining expression with 
# clinical data specified by 'plotType', e.g., 'stage'.
get_mongo_df <- function(ds, gene_qry, clin_column = NULL) {
  m <- mongo_connect(paste0(ds, '_expr'))
  x <- m$find(gene_qry)
  m <- mongo_connect(paste0(ds, '_clinical'))
  y <- m$find()
  
  if (is.null(clin_column)) {
    df <- data.frame(id = y$id, x = x$expr[[1]], y[,-1])
    colnames(df)[2] <- x$gene
    return(df)
  }
  
  if (length(x$expr[[1]]) == 0 || is.null(y[[clin_column]])) {
    return(NULL)
  }
  
  data.frame(x = x$expr[[1]], y = y[[clin_column]]) %>% drop_na()
}

bcbet_boxplot <- function(df,ds, reverse = FALSE, upper = TRUE) {
  g1 <- ggplot(df, aes(y,x,fill=y)) + geom_boxplot() + theme_linedraw() +
    labs(x = '', y = 'log2 expression') + guides(fill = 'none') + 
    ggtitle(ds) + theme(text = element_text(size = 14))
  
  if (upper || reverse) {
    levels <- labels <- limits <- levels(factor(df$y))
  
    if (upper) {
      labels = toupper(labels)
    }  
    
    if (reverse) {
      limits <- limits[2:1]
      labels <- labels[2:1]
    }
    
    return (g1 + scale_x_discrete(limits = limits, labels = labels))
  }
  
  g1
  
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
  
  upper <- reverse <- FALSE
  if (plotType %in% c("stage", "grade")) {
    upper <- TRUE
    reverse <- TRUE
  }

  for (ds in datasets) {
    
    df <- get_mongo_df(ds, qry, plotType)
    
    if (is.null(df) || nrow(df) == 0) {
      next
    }
    
    # cat('ds #', count, ' is ', ds, ' (', plotType, ') with upper/reverse: ',
    # upper,'/',reverse,'\n')
    # 
  
    myplots[[count]] <- bcbet_boxplot(df, ds, upper = upper, reverse = reverse)
    
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



observeEvent(list(input$ResultsPage, input$plotsPage), {
  
  if (input$ResultsPage != "Plots") {
    return()
  }
  
  if (input$plotsPage == 'Tumor') {
    tumorPlots()
    return()
  } else if (input$plotsPage == 'Grade') {
    gradePlots()
    return()
  } else if (input$plotsPage == 'Stage') {
    stagePlots()
    return()
  }
  
}, ignoreInit = TRUE)

