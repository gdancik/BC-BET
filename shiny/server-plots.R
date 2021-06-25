library(mongolite)
library(ggplot2)
library(tidyr)
library(survival)
library(GGally)
library(cowplot)

source('graphPanelFunctions.R')
source('mongo.R')


# returns a data frame combining expression with 
# clinical data specified by 'clin_column', e.g., 'stage'.
#   - plotType only used for 'survival_lg_nmi' or 'survival_hg_mi'
#   - if treated is 'no', we filter df to remove treated

get_mongo_df <- function(ds, gene_qry, clin_column = NULL, plotType = NULL, treated = NULL) {
  
  HG_MI_COHORTS <- c('mda1', 'mda2')
  
  if (ds %in% HG_MI_COHORTS && !is.null(plotType) && plotType != 'survival_hg_mi') {
      return(NULL)
  }
  
  m <- mongo_connect(paste0(ds, '_expr'))
  x <- m$find(gene_qry)
  m <- mongo_connect(paste0(ds, '_clinical'))
  y <- m$find()
  
  # if no clin_column is specified, return all data
  if (is.null(clin_column)) {
    
    if (!is.null(plotType)) {
      stop('plotType must be NULL if clin_column is NULL')
    }
    df <- data.frame(id = y$id, x = x$expr[[1]], y[,-1])
    colnames(df)[2] <- x$gene
    return(df)
  } 
  
  if (length(x$expr[[1]]) == 0 || any(!clin_column%in%colnames(y))) {
    return(NULL)
  }
  
  keep <- NULL
  
  
  if (plotType == 'survival_lg_nmi') {
    keep <- y$grade %in% 'lg' & y$stage %in% 'nmi'
  } else if (plotType == 'survival_hg_mi') {
    
    if (ds %in% HG_MI_COHORTS) {
      keep <- NULL
    } else {
      keep <- y$grade %in% 'hg' & y$stage %in% 'mi'
    }
  }
  
  if (!is.null(keep) && sum(keep,na.rm=TRUE) < 10) {
    return(NULL)
  } 
  
  if (length(clin_column) == 1) {
    df <- data.frame(x = x$expr[[1]], y = y[,clin_column])
  } else {
    df <- data.frame(x = x$expr[[1]],
                     y1 = y[,clin_column[1]],
                     y2 = y[,clin_column[2]])
    
    if (!is.null(y$treated) && treated == 'no') {
      df <- df[y$treated == 0,]
    }
    
  }

  if (!is.null(keep)) {
    df <- df[keep,]
  }
  
  df %>% drop_na()
    
}

bcbet_boxplot <- function(df,ds, measure_name, measure, p, reverse = FALSE, upper = TRUE) {

  p <- round(p, 2)
  if (p < 0.01) {
    p <- 'P < 0.01'
  } else {
    p <- paste0('P = ', p)
  }
  
  if (measure_name == 'fc') {
    measure <- paste0('FC = ', round(measure, 2))
  } else {
    measure <- paste0('AUC = ', round(measure, 2))
  }
  
  
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

bcbet_km <- function(df, ds, hr, p, endpoint) {
  
  p <- round(p,3)
  if (p < 0.01) {
    p <- 'P < 0.01'
  } else {
    p <- paste0('P = ', p)
  }
  
  hr <- paste0('HR = ', round(hr, 2))
  title <- paste0(ds, '\n', hr, ' (', p, ')')

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
    labs(x = 'Time (months)', 
         y = paste0('Survival (', endpoint, ') probability')) + ggtitle(title)
    
}

# generate plots of plotType (e.g., 'stage') for rendering
# in output$graphOutputId (e.g., output$tumor)
generatePlots <- function(plotType, graphOutputId) {
 
  catn('in generatePlots...')
  
  # if (plotType == 'survival') {
  #   showNotification('need to handle plotType survival in generatePlots; we need to get survival results from mongo')
  #   return(NULL)
  # }
  
  shinyjs::runjs("$('#please-wait').removeClass('hide');")
  
  plot_prefix <- paste0('plot_', plotType)
  
  # may need to alter this for multi-gene input. 
  gene <- REACTIVE_SEARCH$gene
  
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
  
  if (plotType %in% c('survival', 'survival_lg_nmi', 'survival_hg_mi')) {
    results <- REACTIVE_SEARCH$results_survival[[plotType]]
    treated <- REACTIVE_SEARCH$parameters$treated
  } else {
    results <- REACTIVE_SEARCH$results_de[[plotType]]
    treated <- NULL
  }
  
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

  catn('generating plot for:')
  print(results)
  
  for (i in 1:nrow(results)) {
    
    columns <- plotType
    if (plotType %in% c('survival', 'survival_lg_nmi', 'survival_hg_mi')) {
      columns <- paste0(REACTIVE_SEARCH$parameters$endpoint, c('_time', '_outcome'))
      columns <- c(columns, 'treated')
      
      measure <- results$hr_med[i]
      p <- results$p_med[i]
      endpoint <- results$endpoint[i]
    } else {
      measure <- results[[REACTIVE_SEARCH$parameters$measure]][i]
      p <- results[[REACTIVE_SEARCH$parameters$pvalue]][i]
    }
    
    ds <- results$dataset[i]
    

    cat("getting data for: ", ds, "...\n")
    df <- get_mongo_df(ds, qry, columns, plotType, treated = treated)
    
    if (is.null(df) || nrow(df) == 0) {
      catn('skipping ', df, ' ...')
      next
    }
    
    # cat('ds #', count, ' is ', ds, ' (', plotType, ') with upper/reverse: ',
    # upper,'/',reverse,'\n')
    # 
  
    cat('  plotType ', plotType, ' for ', ds, '...\n')
    
    if (plotType %in% c('survival', 'survival_lg_nmi', 'survival_hg_mi')) {
      cat('  assigning plot to myplots')
      myplots[[count]] <- bcbet_km(df, ds, measure, p, REACTIVE_SEARCH$parameters$endpoint)
    } else {
      
      myplots[[count]] <- bcbet_boxplot(df, ds, REACTIVE_SEARCH$parameters$measure, measure, p, upper = upper, reverse = reverse)
    }
    
    count <- count + 1
  }
  
  # create appropriate number of plot containers, including legend for
  # survival
  
  if (plotType %in% c('survival', 'survival_lg_nmi', 'survival_hg_mi') && count > 0) {
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

survivalPlotsLGNMI <- reactive({
  catn('survivalPlotslGNMI reactive...')
  generatePlots('survival_lg_nmi', 'graphOutputSurvivalLGNMI')
})

survivalPlotsHGMI <- reactive({
  generatePlots('survival_hg_mi', 'graphOutputSurvivalHGMI')
})


observeEvent(list(input$resultsPage, input$plotsPage), {
  
  # if (input$resultsPage != "Plots") {
  #   return()
  # }
  # 
  
  catn('generating plots for: ', input$plotsPage)
  
  if (input$resultsPage == 'Tumor') {
    tumorPlots()
    return()
  } else if (input$resultsPage == 'Grade') {
    gradePlots()
    return()
  } else if (input$resultsPage == 'Stage') {
    stagePlots()
    return()
  } else if (input$resultsPage == 'Survival') {
    survivalPlots()
  } else if (input$resultsPage == 'Survival (LG/NMI)') {
    survivalPlotsLGNMI()
  } else if (input$resultsPage == 'Survival (HG/MI)') {
    survivalPlotsHGMI()
  }
  
}, ignoreInit = TRUE)

