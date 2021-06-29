library(ggplot2)
library(dplyr)
library(reshape)
library(DT)
library(xlsx)

source('mongo.R')

# must use 'a','b','c','d' with below FC and p-value order
# to ensure that categories/labels are plotted in the correct order
summarize_de <- function(x, fc_col = 'fc', p_col = 'pt', count = TRUE) {

  cutoff <- 1
  if (fc_col == 'auc') {
    cutoff <- 0.5
  }
  
  category <- rep('e', nrow(x)) #vector('character', nrow(x))
  category[x[[fc_col]] > cutoff & x[[p_col]] < 0.05] <- 'a'
  category[x[[fc_col]] > cutoff & x[[p_col]] > 0.05] <- 'b'
  category[x[[fc_col]] < cutoff & x[[p_col]] > 0.05] <- 'c'
  category[x[[fc_col]] < cutoff & x[[p_col]] < 0.05] <- 'd'
  
  category <- factor(category, levels = c('a','b','c','d', 'e'))
  

  if (count) {
    return(table(count))
  }  
  
  data.frame(x, category = category)
  
  
}

# calculates the ylabel position given a count vector 'x',
# corresponding to 'a'-'e' categories;
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
  res1 <- sapply(types, function(x, qry, cols) {
    m <- mongo_connect(x)
    fields <- paste0('{"_id":0, "gene":1, "dataset":1, ',
                     paste0('"', cols, '":1', collapse = ', '),
              '}')
    m$find(qry, fields = fields)
  }, qry = qry, cols = c(REACTIVE_SEARCH$parameters$measure, REACTIVE_SEARCH$parameters$pvalue), USE.NAMES = TRUE, simplify = FALSE)
  
  res1 <- lapply(res1, arrange, dataset)
  res1 <- lapply(res1, summarize_de, fc_col = REACTIVE_SEARCH$parameters$measure, p_col = REACTIVE_SEARCH$parameters$pvalue, count = FALSE)
  
  # get survival results
  types <- c('survival', 'survival_lg_nmi', 'survival_hg_mi')
  
  # specify endpoint -- for 'ba', do not change query
  # qry <- paste0('{"gene":"', gene, '", "endpoint":"dss"}')
  
  cutpoint <- REACTIVE_SEARCH$parameters$cutpoint
  
  # m$find(' {"dataset": "dfci",  "treated": {"$ne": "include"}}  ')
  
  if (REACTIVE_SEARCH$parameters$treated == 'yes') {
    treated_qry <- '"treated": {"$ne": "remove"}'
  } else {
    treated_qry <- '"treated": {"$ne": "include"}'
  }
  
  res2 <- sapply(types, function(x, qry, cutpoint, treated_qry) {
    m <- mongo_connect(x)
    qry <- gsub("}", paste0(',', treated_qry, "}"), qry)
    fields = paste0('{"_id":0, "gene":1, "dataset":1, "endpoint":1,',
        paste0( c('\"hr_', '\"p_'), cutpoint, '\":1', collapse = ', '),
    '}')
    
    m$find(qry, fields = fields)
  }, qry = qry, cutpoint = cutpoint, treated_qry = treated_qry, USE.NAMES = TRUE, simplify = FALSE)
  
  res2 <- lapply(res2, arrange, dataset)
  
  # get specified endpoint, for now we assume 'ba' 
  get_endpoint <- function(x, endpt) {
    if (nrow(x) == 0) {
      return(x)
    }
    
    if (endpt != 'ba') {
      return(x %>% dplyr::filter(endpoint == endpt))
    }
    x  %>% arrange(endpoint) %>% group_by(dataset) %>% slice_head()
  }
  
  # if 'ba', then take first endpoint for each dataset (dss, os, rfs)

  res2 <- lapply(res2, get_endpoint, endpt = REACTIVE_SEARCH$parameters$endpoint)
  
  res2 <- lapply(res2, summarize_de, 
                 fc_col = paste0('hr_', cutpoint), 
                 p_col = paste0('p_', cutpoint),
                 count = FALSE)
  
  add_stats <- function(n, x, survival = NULL, treated_qry = NULL) {
    x <- x[[n]]
    if (nrow(x) == 0) {
      return(x)
    }
    
    m <- mongo_connect(paste0('stats_', n))

    
    if (is.null(treated_qry)) {
      stats <- m$find()  
    } else {
      stats <- m$find(query = paste0('{',treated_qry, '}'))
    }
    
    m <- match(x$dataset, stats$dataset)
    
    columns <- 3:2
    if (n == 'tumor') columns <- 2:3
    
    .after = 2
    
    if (!is.null(survival)) {
      return(tibble::add_column(x, n = stats[[survival]][m], .after = 3))
    }

    tibble::add_column(x, stats[m, columns, drop = FALSE], .after = 2)
  }
  
  # add stats for tumor, grade, stage
  n <- names(res1)
  res1 <- lapply(n, add_stats, x = res1)
  names(res1) <- n
    
  # add stats for survival
  n <- names(res2)
  res2 <- lapply(n, add_stats, x = res2, survival = REACTIVE_SEARCH$parameters$endpoint,
                 treated_qry = treated_qry)
  names(res2) <- n
  
  REACTIVE_SEARCH$results_de <- res1
  REACTIVE_SEARCH$results_survival <- res2
  
  
  
})

bcbet_column_names <- function() {
  return(c('Dataset' = 'dataset', 'Gene' = 'gene', 
           'FC' = 'fc', 'AUC' = 'auc', 
           'P-value' = 'pt', 
           'P-value' = 'p_med', 
           'P-value' = 'pw',
           'P-value' = 'p_continuous',
           'HR' = 'hr_med',
           'HR' = 'hr_continuous',
           'Endpoint' = 'endpoint', 
           'N' = 'n', 
           'N_tumor' = 'N_tumor', 'N_normal' = 'N_normal',
           'N_nmi' = 'N_nmi',  'N_mi' ='N_mi', 
           'N_lg' = 'N_lg', 'N_hg' = 'N_hg')
  )
}

render_de_table <- function(x, gene, var_type) {
  if (is.null(x)) {
    return(renderTable(NULL))
  }
  
  filename <- paste0('bcbet_', gene, '_', var_type)
  btnText <- paste('Download', var_type, 'table (csv)')
  
  colnames <- bcbet_column_names()
  colnames <- colnames[colnames%in% colnames(x)]
  
  hideCol <- 6
  print(x)
  

  # Note: let's round here because if formatRound is used, the NAs are replaced
  # with blanks
  
  #roundCols <- c('FC', 'AUC', 'HR', 'P-value')
  #roundCols <- roundCols[roundCols %in% names(colnames)]

  # round to 2 digits
  r1 <- intersect(c('fc', 'auc', 'hr_continuous', 'hr_med'), colnames(x))
  x[,r1] <- round(x[,r1],2)
  
  # round to 3 digits
  r2 <- intersect(c('pw', 'pt', 'p_continuous', 'p_med'), colnames(x))
  x[,r2] <- round(x[,r2],3)
  
  # catn('display iris now...\n')
  # return(renderDataTable(DT::datatable(iris)))
  
  x[is.na(x)] <- 'N/A'
  
  DT::renderDataTable({
    
    DT::datatable(x, colnames = colnames, rownames = FALSE,filter = 'none', selection = 'none',
                  extensions = 'Buttons',
                  options = list(paging = FALSE, searching = FALSE,
                                 columnDefs = list(list(visible=FALSE, targets=hideCol),
                                                   list(className = 'dt-center', targets = '_all')
                                              ),
                              dom = 'Bfrtip',
                                buttons = list(
                                  list(
                                    extend = "collection",
                                    text = 'Download all results (xlsx)',
                                      action = DT::JS("function ( e, dt, node, config ) {
                                          //alert( 'Button activated' );
                                          document.getElementById('downloadAllResults').click();
                                      }")
                      ),
                        list(
                          extend = 'csv', text = btnText, filename = filename,
                          exportOptions = list(columns = ":visible")
                        )
                      )
                  )) %>% DT::formatStyle('category',target = 'row',
      backgroundColor = DT::styleEqual(c('a','b','c','d'), 
                                   c('darkred', 'pink', 'lightblue', 'darkblue')
       ), #fontWeight = 'bold',
      color = DT::styleEqual(c('a','b','c','d'),
                         c('white', 'black', 'black', 'white')
      )
    ) #%>% DT::formatRound(roundCols, c(2,3))
    
  })
}

observe({
  l <- list(input$tabSummaryTable, REACTIVE_SEARCH$results_de, REACTIVE_SEARCH$gene)  
  if (any(sapply(l, is.null))) {
    return(NULL)
  }
  cat('clicked on: ', input$tabSummaryTable, '...\n')
  catn('gene = ', REACTIVE_SEARCH$gene)
  selected <- tolower(input$tabSummaryTable)
  
  if (selected %in% c('survival', 'survival_lg_nmi', 'survival_hg_mi')) {
    output$tableSummary <- render_de_table(REACTIVE_SEARCH$results_survival[[selected]],
                                           REACTIVE_SEARCH$gene, selected)
  } else {
    output$tableSummary <- render_de_table(REACTIVE_SEARCH$results_de[[selected]],
                                         REACTIVE_SEARCH$gene, selected)
  }
})


generate_summary_plot <- function(df, labels, title) {

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
  scale_fill_manual(values = c('darkred', 'pink', 'lightblue', 'darkblue', 'white'),
                    labels = labels) +
  theme_linedraw() + labs(x = '', y = 'proportion') + 
  theme(legend.title = element_blank()) +
  ggtitle(title)
}

output$plotSummary <- renderPlot({
  
  if (is.null(REACTIVE_SEARCH$results_de)) {
    return(NULL)
  }
  
  labels <- labels <- c('FC > 1, P < 0.05', 'FC > 1, P >= 0.05', 'FC < 1, P >= 0.05', 'FC < 1, P > 0.05', 'FC = 1')
  if (REACTIVE_SEARCH$parameters$measure == 'auc') {
    labels <- labels <- c('AUC > 0.50, P < 0.05', 'AUC > 0.50, P >= 0.05', 'AUC < 0.50, P >= 0.05', 'AUC < 0.50, P > 0.05', 'AUC = 0.50')
  }
  
  title <- 'Summary of differential expression results'
  df <- sapply(REACTIVE_SEARCH$results_de, function(x) table(x$category)) %>% data.frame()
  df$category <- rownames(df)
  g1 <- generate_summary_plot(df, labels, title)
  
  res <- REACTIVE_SEARCH$results_de
  
  labels <- gsub('FC|AUC','HR', labels)
  
  title <- 'Summary of survival analysis'
  df <- sapply(REACTIVE_SEARCH$results_survival, function(x) table(x$category)) %>% data.frame()
  df$category <- rownames(df)
  g2 <- generate_summary_plot(df, labels, title)
  
  plot_grid(g1,g2,nrow = 2)
  
  
})

write_sheet <- function(sheet, RES, filename) {
  x <- RES[[sheet]]
  x$category <- NULL
  
  # format columns
  columns <- bcbet_column_names()
  
  m <- match(colnames(x), columns)

  colnames(x)[!is.na(m)] <- names(columns)[m[!is.na(m)]]
  
  write.xlsx(x, sheetName = toupper(sheet), file = filename,
           row.names = FALSE, append = TRUE)
}

write_header_sheet <- function(filename, params) {
  
  p <- paste0(names(params), ': ', params)
  p <- paste0('- ', p, sep = '')
  p <- c('Statistical parameters:\n', p)
              
  header <- c("- TUMOR: FC > 1 means that expression is higher in tumors compared to normal samples",
  "- GRADE: FC > 1 means that expression is higher in high grade (hg) tumors compared to low grade (lg) tumors",
  "- STAGE: FC > 1 means that expression is higher in muscle invasive (mi) tumors compared to non-muscle invasive (nmi) tumors",
  "- SURVIVAL: HR > 1 means that high expression is associated with poor prognosis"
  ) 
  
  header <- c('Description:\n',header)
  header <- c(p, '',header)
  
  header <- gsub('pw', 'pw (p-values calculated using Wilcoxon test)',  header)
  header <- gsub('pt', 'pt (p-values calculated using t-test)',  header)
  
  header <- gsub('treated: yes', 'treated: yes (include patients treated with BCG/adjuvant chemo in survival analysis',  header)
  header <- gsub('treated: no', 'treated: no (remove patients treated with BCG/adjuvant chemo from survival analysis',  header)
  
  
  write.xlsx(header, sheetName = 'Settings', file = filename,
             row.names = FALSE, append = TRUE, col.names = FALSE)
  
}

write_all_results <- function(filename) {
  write_header_sheet(filename, REACTIVE_SEARCH$parameters)
  res1 <- isolate(REACTIVE_SEARCH$results_de)
  res2 <- isolate(REACTIVE_SEARCH$results_survival)
  sapply(names(res1), write_sheet, RES = res1, filename = filename)
  sapply(names(res2), write_sheet, RES = res2, filename = filename)
} 

resultsDownloadHandler <- downloadHandler(
  filename = function() {
    paste('bcbet_', REACTIVE_SEARCH$gene, '.xlsx', sep='')
  },
  content = function(con) {
    write_all_results(con)
    #write.csv(data, con)
  }
)

output$downloadAllResults <- resultsDownloadHandler
output$download_results <- resultsDownloadHandler


