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
  
  
  res1 <- lapply(res1, reformat_multigene_results)
  res2 <- lapply(res2, reformat_multigene_results)
  
  
  res1 <- lapply(res1, summarize_multi_results_de, REACTIVE_SEARCH$parameters$measure, REACTIVE_SEARCH$parameters$pvalue)
  res2 <- lapply(res2, summarize_multi_results_survival, REACTIVE_SEARCH$parameters$cutpoint)
    
  REACTIVE_SEARCH$results_de <- res1
  REACTIVE_SEARCH$results_survival <- res2
  
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