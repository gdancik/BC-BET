library(pheatmap)

getMultiGeneResults <- reactive({
  
  genes <- REACTIVE_SEARCH$gene
  
  qry <- paste0('{"gene": { "$in" : [',
                paste0('"', genes, '"', collapse = ','),
          ']}}')
  
  catn('PROCESSING GENES: ', genes)
  
  # get DE results  
  res1 <- mongo_get_de_results(qry, c(REACTIVE_SEARCH$parameters$measure, REACTIVE_SEARCH$parameters$pvalue)) 
  
  
  # get survival results
  res2 <- mongo_get_survival_results(qry, REACTIVE_SEARCH$parameters$cutpoint,
                                     REACTIVE_SEARCH$parameters$endpoint,
                                     REACTIVE_SEARCH$parameters$treated) 
  
  
  # remove NULL results
  remove_null <- function(r) {
    for (n in names(r)) {
      if (nrow(r[[n]]) == 0) {
        r[[n]] <- NULL
      }
    }
    r
  }
  
  res1 <- remove_null(res1)
  res2 <- remove_null(res2)
  
  res1 <- lapply(res1, reformat_multigene_results)
  res2 <- lapply(res2, reformat_multigene_results)
  
  res1 <- lapply(res1, summarize_multi_results_de, REACTIVE_SEARCH$parameters$measure, REACTIVE_SEARCH$parameters$pvalue)
  res2 <- lapply(res2, summarize_multi_results_survival, REACTIVE_SEARCH$parameters$cutpoint)
    
  REACTIVE_SEARCH$results_de <- res1
  REACTIVE_SEARCH$results_survival <- res2
  
  
  displayMultiResultsTable('Tumor')
  
  output$heatmap <- renderPlot({
    
    #heatmap(as.matrix(iris[,1:3]))
    generate_heatmap(REACTIVE_SEARCH$results_de,  REACTIVE_SEARCH$results_survival)
  })
  
  save(res1, res2, file = 'multiResults.RData')
})


# takes a multigene results data frame with a dataset column
# and reformats with columns ds1-measure, ds1-pvalue, ds2-measure, etc
reformat_multigene_results <- function(res1) {
  
  res1 <- ungroup(res1)
  
  endpoints <- NULL
  if ('endpoint' %in% colnames(res1)) {
    endpoints <- select(res1 %>% ungroup(), dataset, endpoint) %>% unique()
    res1$endpoint <- NULL
  }
  
  s <- split(res1, res1$gene)
  ds <- lapply(s, `[[`, 'dataset') %>% unlist() %>% unique()
  
  columns <- expand.grid(ds, colnames(s[[1]])[3:ncol(s[[1]])],
                         stringsAsFactors = FALSE)
  columns <- apply(columns, 1, function(x) paste0(x,collapse = '_'))
  
  columns <- c('gene', sort(columns))
  tmp_df <- data.frame(matrix(ncol = length(columns)))
  colnames(tmp_df) <- columns
  
  df_all <- reformat_df_by_gene(s, tmp_df)
  
  if (!is.null(endpoints)) {
    for (i in 1:nrow(endpoints)) {
      g <- grep( paste0('^',endpoints$dataset[i], '_'), colnames(df_all))
      colnames(df_all)[g] <- paste0(colnames(df_all)[g], ' (', 
                                    endpoints$endpoint[i], ')')
    }
  }
  
  df_all
}

# reformats a multigene df where 'df' input contains a single gene
# template_df is empty df with all column names
reformat_df_by_gene1 <- function(df, template_df) {
  for (i in 1:nrow(df)) {
    x <- df[i,]
    mydataset <- x$dataset
    x$gene <- NULL
    x$dataset <- NULL
    colnames(x) <- paste0(mydataset, '_', colnames(x))
    m <- match(colnames(x), colnames(template_df))
    template_df[1,m] <- x
  }
  template_df$gene <- df$gene[1]
  template_df
}

# reformats multigene df by each gene 
reformat_df_by_gene <- function(s, template_df) {
  ll <- lapply(s,reformat_df_by_gene1, template_df = template_df)
  dd <- do.call('rbind', ll)
  dd
}

summarize_multi_results_de <- function(r1, measure, pvalue) {
  
  ds <- gsub('_.*$', '', colnames(r1)[-1]) %>% unique()
  
  m1 <- match(paste0(ds, '_', measure), colnames(r1))
  m2 <- match(paste0(ds, '_', pvalue), colnames(r1))
  
  measure_threshold <- 1
  if (measure == 'auc') measure_threshold <- 0.50
 
  calc_scores(r1, r1[,m1], r1[,m2], measure_threshold) 
  
}

summarize_multi_results_survival <- function(r1, cutpoint) {
  
  ds <- gsub('_.*$', '', colnames(r1)[-1]) %>% unique()
  columns <- gsub(' \\(.*', '', colnames(r1))
  
  m1 <- match(paste0(ds, '_hr_', cutpoint), columns)
  m2 <- match(paste0(ds, '_p_', cutpoint), columns)

  calc_scores(r1, r1[,m1], r1[,m2], 0.5) 
}

# calculate score based on vectors 'measures' and 'ps', and
# add to 'r1'
calc_scores <- function(r1, measures, ps, threshold) {
  
  # need to handle NA values
  measures[is.na(measures)] <- threshold
  ps[is.na(ps)] <- 1
  
  score <- rowSums( sign(measures - threshold) * as.integer(ps < 0.05))
  
  tibble::add_column(r1, score = score, .after = 'gene') %>%
    arrange(desc(score), gene)
}


# sample_score_distribution <- function() {
#   
#   create_score_df <- function(type, res) {
#     data.frame(type = type, score = res[[type]]$score)
#   }
#   
#   scores <- lapply(names(res1), create_score_df, res = res1)
#   scores <- do.call('rbind', scores)
#   
# }


summarize_score_table1 <- function(type, res) {
  
  r <- res[[type]]
  
  #filter(r, score != 0) %>% 
  r %>% arrange(desc(abs(score))) %>% select(gene,score) %>% #slice_head(n=10) %>%
    tibble::add_column(analysis = type, .after = 'gene')
}

summarize_score_table <- function(res1, res2) {
  r <- lapply(names(res1), summarize_score_table1, res1)
  r <- do.call('rbind', r)
  
  r2 <- lapply(names(res2), summarize_score_table1, res2)
  r2 <- do.call('rbind', r2)
  rr <- rbind(r,r2)
  
}

displayMultiResultsTable <- function(selected = NULL) {

  if (selected %in% c('Tumor', 'Stage', 'Grade')) {
    x <- REACTIVE_SEARCH$results_de[[tolower(selected)]]
  } else {
    x <- REACTIVE_SEARCH$results_survival[[tolower(selected)]]
  }

  if (is.null(x)) {
    return(DT::datatable(data.frame(results = integer()), colnames = NULL,
                         options = list(paging = FALSE)))
  }
    
  mycolumns <- colnames(x)
  
  g1 <- grep('_auc|_fc|_hr', mycolumns)
  g2 <- grep('_p', mycolumns)
  
  mytable <- DT::datatable(x, rownames = FALSE,filter = 'none', selection = 'none',
                           options = list(scrollX = TRUE, scrollY = '400px', paging = FALSE)
              ) %>% formatRound(mycolumns[g1]) %>% 
                    formatRound(mycolumns[g2], digits = 3)
  

  
  return(mytable)
  
  #output[[paste0('tableMultiSummary', selected)]] <- renderDataTable(mytable)  
  
}


observe({
  
  output$tableMultiSummary <- renderDataTable(displayMultiResultsTable(input$tabSummaryMultiTable))
})



observeEvent(input$score_expand, {
  catn('click')
  shinyjs::runjs('$("#score_div").toggleClass("hide");')
})


## heatmap functions ##
format_for_heatmap <- function(i, res) {
  x <- res[[i]]
  x <- x %>% select(gene, score) %>% arrange(desc(abs(score)))
  colnames(x)[2] <- names(res)[i]
  x
}

# maximum number of valid datasets in results
# 2 columns for measure/p-value and 2 for gene/score
num_ds_for_heatmap <- function(i, res) {
  m <- max(rowSums(!is.na(res[[i]])))
  m/2 - 1
}


generate_heatmap <- function(res1, res2) {
  # add tumor, grade, stage
  
  l1 <- lapply(1:length(res1), format_for_heatmap, res1)
  x <- l1[[1]]
  for (i in 2:length(l1)) {
    x <- full_join(x, l1[[i]])
  }

  # add survival, lg_nmi_survival, and hg_mi_survival
  l2 <- lapply(1:length(res2), format_for_heatmap, res2)
  for (i in 1:length(l2)) {
    x <- full_join(x, l2[[i]])
  }

  rownames(x) <- x$gene
  x <- x[,2:ncol(x)]

  if (nrow(x) > 100) {
    mm <- apply(abs(x), 1, max, na.rm=TRUE)
    x <- dplyr::arrange(x, desc(mm)) %>% slice_head(n=100)
  }
  
  ds_scale <- c(sapply(1:length(res1), num_ds_for_heatmap, res1),
              sapply(1:length(res2), num_ds_for_heatmap, res2))

  colors <- colorRampPalette(rev(c('darkred', 'pink', 'white', 'blue', 'darkblue')))(100)

  catn('generating heatmap now...\n')
  
   pheatmap(t(sweep(x,2, ds_scale, FUN = "/")), cluster_rows = FALSE, color = colors, 
           display_numbers = t(x), number_color = 'white')
   
  #x <- as.matrix(x)
  #heatmap(sweep(x,2, ds_scale, FUN = "/"), Colv = FALSE, col = colors)
           
  # heatmaply(sweep(x,2, ds_scale, FUN = "/"), colors = colors, 
  #           grid_gap = 1, cellnote = x)
  # 
}


observeEvent(input$tabSummaryMultiTable, {
  l <- list(input$tabSummaryMultiTable, REACTIVE_SEARCH$results_de, REACTIVE_SEARCH$gene)  
  
  if (any(sapply(l, is.null))) {
    return(NULL)
  }
  
  cat('clicked on: ', input$tabSummaryMultiTable, '...\n')
  catn('gene = ', REACTIVE_SEARCH$gene)
  
  selected <- input$tabSummaryMultiTable
  displayMultiResultsTable(selected)  
  
})


observe({

  output$score_description <- renderUI(
    HTML(
        fc_description(REACTIVE_SEARCH$parameters$measure, TRUE, TRUE), "</br>",
    )
  )
    
})
