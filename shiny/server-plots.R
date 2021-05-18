library(mongolite)
library(ggplot2)
library(tidyr)
library(survival)
library(GGally)

source('graphPanelFunctions.R')
source('mongo.R')


# returns a data frame combining expression with 
# clinical data specified by 'clin_column', e.g., 'stage'.
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
  
  if (length(x$expr[[1]]) == 0 || any(!clin_column%in%colnames(y))) {
    return(NULL)
  }
  
  if (length(clin_column) == 1) {
    df <- data.frame(x = x$expr[[1]], y = y[,clin_column])
  } else {
    df <- data.frame(x = x$expr[[1]],
                     y1 = y[,clin_column[1]],
                     y2 = y[,clin_column[2]])
  }

  df %>% drop_na()
    
}

bcbet_boxplot <- function(df,ds, measure, p, reverse = FALSE, upper = TRUE) {

  p <- round(p)
  if (p < 0.01) {
    p <- 'P < 0.01'
  } else {
    p <- paste0('P = ', p)
  }
  
  measure <- paste0('FC = ', round(measure, 2))
  
  
  title <- paste0(ds, '\n', measure, ' (', p, ')')
  
    g1 <- ggplot(df, aes(y,x,fill=y)) + geom_boxplot() + theme_linedraw() +
    labs(x = '', y = 'log2 expression') + guides(fill = 'none') + 
    ggtitle(title) + theme(text = element_text(size = 14))
  
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


bcbet_km <- function(df, ds) {
  
  cut = median(df$x)
  upper = "upper 50%"; lower = "lower 50%"
  
  ## split into high and low groups using appropriate cutoff ## 
  df$expression <- factor(df$x >= median(df$x), labels = c(lower,upper))
  
  ## plot graph ### ggplot2/GGally form
  km.group1 = survfit(Surv(y1, y2) ~ expression, data = df)
  col <- c('darkblue', 'darkred')
  
  ggsurv(km.group1, 
                    main = ds, 
                    surv.col = col, cens.col = col,
                    size.est = 1) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) + theme_classic() +
    labs(x = 'Time (months)', ylab = 'Survival probability')
    
}

# generate plots of plotType (e.g., 'stage') for rendering
# in output$graphOutputId (e.g., output$tumor)
generatePlots <- function(plotType, graphOutputId) {
 
  catn('in generatePlots...')
  
  if (plotType == 'survival') {
    showNotification('need to handle plotType survival in generatePlots; we need to get survival results from mongo')
    return(NULL)
  }
  
  shinyjs::runjs("$('#please-wait').removeClass('hide');")
  
  plot_prefix <- paste0('plot_', plotType)
  
  # may need to alter this for multi-gene input. 
  gene <- input$geneInput
  
  # # get datasets (we may want to get this from mysql results)
  # m <- mongo_connect('mskcc_clinical')
  # command <- paste0('{"listCollections":1, ',
  #                   '"filter": {"name": {"$regex": "expr", "$options":""}},',
  #                   '"nameOnly": "true" }')
  # 
  # collections <- m$run(command)
  # 
  # if (collections$ok != 1) {
  #   message('Could not get collections')
  #   return()
  # }
  # 
  # datasets <- gsub('_.*$', '', collections$cursor$firstBatch$name)
  # datasets <- sort(datasets)
  
  
  results <- REACTIVE_SEARCH$results[[plotType]]
  
  if (nrow(results) == 0) {
    return(NULL)
  }
  
 
  # Generate graph for each dataset
  
  qry <- paste0('{"gene": "', gene, '"}')
  
  count <- 1
  myplots <- vector(mode = 'list', length = 1)
  
  upper <- reverse <- FALSE
  if (plotType %in% c("stage", "grade")) {
    upper <- TRUE
    reverse <- TRUE
  }

  for (i in 1:nrow(results)) {
    
    columns <- plotType
    if (plotType == 'survival') {
      columns <- c('dss_time', 'dss_outcome')
    }
    
    ds <- results$dataset[i]
    measure <- results$fc[i]
    p <- results$pt[i]
    
    cat("getting data for: ", ds, "...\n")
    df <- get_mongo_df(ds, qry, columns)
    
    if (is.null(df) || nrow(df) == 0) {
      next
    }
    
    # cat('ds #', count, ' is ', ds, ' (', plotType, ') with upper/reverse: ',
    # upper,'/',reverse,'\n')
    # 
  
    cat('  plotType ', plotType, ' for ', ds, '...\n')
    
    if (plotType == 'survival') {
      cat('  assigning plot to myplots')
      myplots[[count]] <- bcbet_km(df)
    } else {
      myplots[[count]] <- bcbet_boxplot(df, ds, measure, p, upper = upper, reverse = reverse)
    }
    
    count <- count + 1
  }
  
  # create appropriate number of plot containers, including legend for
  # survival
  
  
  if (plotType == 'survival' && count > 0) {
    count <- count + 1

    myplot1 <- myplots[[1]] 
    mylegend <- cowplot::get_legend(myplot1)

    myplots <- lapply(myplots, function(x) x + theme(legend.position = 'none'))
    myplots[[count-1]] <- plot_grid(mylegend)
  }
  
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

survivalPlots <- reactive({
  generatePlots('survival', 'graphOutputSurvival')
})

observeEvent(list(input$resultsPage, input$plotsPage), {
  
  if (input$resultsPage != "Plots") {
    return()
  }
  
  
  catn('generating plots for: ', input$plotsPage)
  
  if (input$plotsPage == 'Tumor') {
    tumorPlots()
    return()
  } else if (input$plotsPage == 'Grade') {
    gradePlots()
    return()
  } else if (input$plotsPage == 'Stage') {
    stagePlots()
    return()
  } else if (input$plotsPage == 'Survival') {
    survivalPlots()
  }
  
}, ignoreInit = TRUE)

