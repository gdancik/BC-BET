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
getSingleGeneResults <- function(gene) {
  shinyjs::runjs("$('#please-wait').removeClass('hide');")
  
  #myinput <- reactiveValuesToList(input)
  
  catn('PROCESSING GENE: ', gene)
  
  qry <- paste0('{"gene":"', gene, '"}')
  
  # get DE results
  types <- c('tumor', 'grade', 'stage')
  res <- sapply(types, function(x, qry) {
    m <- mongo_connect(x)
    m$find(qry, fields = '{"_id":0, "gene":1, "dataset":1, "fc":1, "pt":1}')
  }, qry = qry, USE.NAMES = TRUE, simplify = FALSE)
  
  res <- lapply(res, summarize_de, count = FALSE)
  
  setGLOBAL('geneResults', res)
  
  output$ResultsHeader <- renderUI({
    h4('Patient Analysis for', gene, style = 'margin:0px; color:darkred;')
  })
  
  
  output$plotSummary <- renderPlot({
    
    labels <- labels <- c('FC > 1, P < 0.05', 'FC > 1, P >= 0.05', 'FC < 1, P >= 0.05', 'FC < 1, P > 0.05')
    
    df <- sapply(res, function(x) table(x$category)) %>% data.frame()
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
  
  output$tableSummary <- render_de_table(GLOBAL$geneResults$tumor)
  
  shinyjs::runjs("$('#please-wait').addClass('hide');")
}


render_de_table <- function(x) {
  if (is.null(x)) {
    return(renderTable(NULL))
  }
  
  colnames <- c('Dataset' = 'dataset', 'Gene' = 'gene', 'FC' = 'fc', 
                'P-value' = 'pt')
  
  renderDataTable({
    
    DT::datatable(x, colnames = colnames, rownames = FALSE,filter = 'none', selection = 'none',
                  options = list(paging = FALSE, searching = FALSE,
                                 columnDefs = list(list(visible=FALSE, targets=4),
                                                   list(className = 'dt-center', targets = '_all'))
                                 )
                  ) %>% 
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

observeEvent(input$tabSummaryTable, {
  cat('clicked on: ', input$tabSummaryTable, '...\n')
  output$tableSummary <- render_de_table(GLOBAL$geneResults[[tolower(input$tabSummaryTable)]])
})





# 
# DT::datatable(x, rownames = FALSE,filter = 'none', selection = 'none',
#               options = list(paging = FALSE, searching = FALSE,
#                              columnDefs = list(list(visible=FALSE, targets=4))
#               )) %>%  
#   DT::formatStyle('Category', target = 'row',
#   backgroundColor = styleEqual(c('a','b','c','d'), 
#                                c('darkred', 'pink', 'lightblue', 'darkblue')
#   ), fontWeight = 'bold',
#   color = styleEqual(c('a','b','c','d'),
#                      c('white', 'black', 'black', 'white')
#   )) %>% formatRound(c('FC', 'P-value'),3)
