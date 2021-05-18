library(ggplot2)
library(dplyr)
library(reshape)

source('mongo.R')

# must use 'a','b','c','d' with below FC and p-value order
# to ensure that categories/labels are plotted in the correct order
summarize_de <- function(x, fc_col = 'fc', p_col = 'pt', count = TRUE) {

  category <- vector('character', nrow(x))
  category[x[[fc_col]] > 1 & x[[p_col]] < 0.05] <- 'a'
  category[x[[fc_col]] > 1 & x[[p_col]] >= 0.05] <- 'b'
  category[x[[fc_col]] < 1 & x[[p_col]] >= 0.05] <- 'c'
  category[x[[fc_col]] < 1 & x[[p_col]] < 0.05] <- 'd'
  
  category <- factor(category, levels = c('a','b','c','d'))
  

  if (count) {
    return(table(count))
  }  
  
  data.frame(x, category = category)
  
  
}

# calculates the ylabel position given a count vector 'x',
# corresponding to 'a'-'d' categories;
# label is positioned in the center of the y-range
label_position <- function(x) {
  n <- length(x)
  d <- c(0, cumsum(x)[-n])
  res <- d + (cumsum(x) - d) /2
  res[x==0] <- NA
  res
}

format_column_names <- function(x) {
  mynames <- colnames(x) 
  mynames <- toupper(mynames)
  index <- which(nchar(mynames) > 3)
  for (i in index) {
    mynames[i] <- gtools::capwords(mynames[i], strict = TRUE)
  }
  
  mynames <- gsub('PT', 'P-value', mynames)
  mynames <- gsub('PW', 'P-value', mynames)
  
  colnames(x) <- mynames
  x
}

# generate results for single 'gene'
getSingleGeneResults <- reactive({
    
  gene <- REACTIVE_SEARCH$gene
  
  catn('PROCESSING GENE: ', gene)
  
  qry <- paste0('{"gene":"', gene, '"}')
  
  # get DE results
  types <- c('tumor', 'grade', 'stage')
  res <- sapply(types, function(x, qry) {
    m <- mongo_connect(x)
    m$find(qry, fields = '{"_id":0, "gene":1, "dataset":1, "fc":1, "pt":1}')
  }, qry = qry, USE.NAMES = TRUE, simplify = FALSE)
  
  res <- lapply(res, summarize_de, count = FALSE)
  
  REACTIVE_SEARCH$results <- res
  
  
})

render_de_table <- function(x, gene, var_type) {
  if (is.null(x)) {
    return(renderTable(NULL))
  }
  
  filename <- paste0('bcbet_', gene, '_', var_type)
  btnText <- paste('Download', var_type, 'table')
  
  colnames <- c('Dataset' = 'dataset', 'Gene' = 'gene', 'FC' = 'fc', 
                'P-value' = 'pt')
  
  renderDataTable({
    
    DT::datatable(x, colnames = colnames, rownames = FALSE,filter = 'none', selection = 'none',
                  extensions = 'Buttons',
                  options = list(paging = FALSE, searching = FALSE,
                                 columnDefs = list(list(visible=FALSE, targets=4),
                                                   list(className = 'dt-center', targets = '_all')),
                              dom = 'Bfrtip',
                                buttons = list(
                                  list(
                                    extend = 'csv', text = btnText, filename = filename
                                  ),
                                  list(
                                    extend = "collection",
                                    text = 'Download all results',
                                      action = DT::JS("function ( e, dt, node, config ) {
                                          alert( 'Button activated' );
                                      }")
                      ))
                  
                  
                  )) %>% 
      DT::formatStyle('category',target = 'row',
      backgroundColor = styleEqual(c('a','b','c','d'), 
                                   c('darkred', 'pink', 'lightblue', 'darkblue')
       ), #fontWeight = 'bold',
      color = styleEqual(c('a','b','c','d'),
                         c('white', 'black', 'black', 'white')
      )
    ) %>% formatRound(c('FC', 'P-value'),c(2,3))
    
  })
}

observe({
  l <- list(input$tabSummaryTable, REACTIVE_SEARCH$results, REACTIVE_SEARCH$gene)  
  if (any(sapply(l, is.null))) {
    return(NULL)
  }
  cat('clicked on: ', input$tabSummaryTable, '...\n')
  catn('gene = ', REACTIVE_SEARCH$gene)
  selected <- tolower(input$tabSummaryTable)
  output$tableSummary <- render_de_table(REACTIVE_SEARCH$results[[selected]],
                                         REACTIVE_SEARCH$gene, selected)
})

output$plotSummary <- renderPlot({
  
  if (is.null(REACTIVE_SEARCH$results)) {
    return(NULL)
  }
  
  labels <- labels <- c('FC > 1, P < 0.05', 'FC > 1, P >= 0.05', 'FC < 1, P >= 0.05', 'FC < 1, P > 0.05')
  
  df <- sapply(REACTIVE_SEARCH$results, function(x) table(x$category)) %>% data.frame()
  df$category <- rownames(df)
  
  # melt the data to create data frame of counts
  # arrange (order) by category (plotted with
  # 'a' on the top and 'd' on the bottom), and
  # add the label_position and number of datasets
  # for each variable
  m <- melt(df, id.vars = 'category') %>%
    arrange(rev(category)) %>%
    group_by(variable) %>%
    #mutate(label_y = cumsum(value), n = sum(value))
    mutate(label_y = label_position(value), n = sum(value))
  
  ggplot(m, aes(x = variable, y = value, fill = category)) +
    geom_col(position = 'fill', color = 'black') +
    geom_label(aes(y = label_y / n, label = value), vjust = .5,
               colour = "black", fill = 'white') +
    scale_fill_manual(values = c('darkred', 'pink', 'lightblue', 'darkblue'),
                      labels = labels) +
    theme_linedraw() + labs(x = '', y = 'proportion') + 
    theme(legend.title = element_blank()) +
    ggtitle('Summary of differential expression results')
  
})


