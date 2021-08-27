library(survival)
library(dplyr)

#' Carries out a two-sample t-test for vector 'x'
#'
#' @param x A vector of expression values
#' @param y A vector of pheno values corresponding to 'x'
#' @param groups A vector of group labels; fc is calculated as
#'        group2 - group1
#' @return A matrix containing the FC and p-value from the t-test
#'          
diff_expr_t_test1 <- function(x, y, groups) {
  
  w <- wilcox.test(x ~y)
  y2 <- factor(y, levels = groups, labels = 0:1)
  keep <- !is.na(y2)
  a <- auc(roc(x[keep],y2[keep]))
  
  res <- t.test(x ~ y) 
  groups <- paste('mean in group', groups)
  logfc <- res$estimate[groups[2]] - res$estimate[groups[1]]
  
  cbind(fc = 2**unname(logfc), pt = res$p.value, auc = a, pw = w$p.value)
}

#' Carries out a two-sample t-test for each row of 'X' 
#'
#' @param X A matrix of expression values
#' @param y A vector of pheno values corresponding to each row of 'X'
#' @param groups A vector of group labels; fc is calculated as
#'        group2 - group1
#' @return A matrix where each row contains the FC and p-value 
#'         corresponding to each row of X
#' 
diff_expr_t_test <- function(X, y, groups) {
  res <- apply(X, 1, diff_expr_t_test1, y, groups)
  res <- t(res)
  colnames(res) <- c('fc', 'pt', 'auc', 'pw')
  res
}

#' Survival analysis using coxph
#'
#' @param x A vector of expression values
#' @param times A vector of survival times
#' @param outcomes A vector of outcomes (0 = uncensored; 1 = censored)
#' @return A matrix containing the HR and p-values from the continuous model and 
#'         low/high expressors relative to median 
coxph_test1 <- function(x, times, outcomes) {
  keep <- !is.na(x) & !is.na(times) & !is.na(outcomes)
  x <- x[keep]; times <- times[keep]; outcomes <- outcomes[keep]
  
  mod_med <- coxph(Surv(times, outcomes) ~ x >= median(x))
  s_med <- summary(mod_med)
  
  mod_cont <- coxph(Surv(times, outcomes)~x)
  s_cont <- summary(mod_cont)
  
  cbind(hr_med = s_med$coefficients[2],
        p_med = s_med$logtest[3],
        hr_continuous = s_cont$coefficients[2], 
        p_continuous = s_cont$logtest[3])
}

#' Carries out a coxph survival analysis for each row of 'X' 
#'
#' @param X A matrix of expression values
#' @param times A vector of survival times
#' @param outcomes A vector of outcomes (0 = uncensored; 1 = censored)
#' @return A matrix where each row contains the HR and p-values 
#'         corresponding to each row of X
#' 
coxph_test <- function(X, times, outcomes) {
  res <- apply(X, 1, coxph_test1, times, outcomes)
  res <- t(res)
  colnames(res) <- c('hr_med', 'p_med', 'hr_continuous', 'p_continuous')
  res
}

#' Loads data from specified datasets
#'
#' @param ds The name of the datasets
#' @return A list containing expression matrix X and clinical table Y
#' 
get_data <- function(ds) {
  load(paste0('../../data/processed/', ds, '.RData'))
  load(paste0('../../data/clinical/', ds, '.RData'))
  
  list(X = get(paste0(ds,'.expr')),
       Y = get(paste0(ds, '_clinical')))
}

#' Checks whether processed and clinical data workspaces exist
#'
#' @param ds The name of the datasets
#' @return a logical vector containing TRUE if processed and clinical data exists
#' 

check_data <- function(ds) {
  f1 <- paste0('../../data/processed/', ds, '.RData')
  f2 <- paste0('../../data/clinical/', ds, '.RData')
  res <- file.exists(f1) & file.exists(f2)
  names(res) <- ds
  res
}


########################################################################
# mongo functions
########################################################################

mongo_connect <- function(collection, user = "root", pass = "password", 
                          host = "0.0.0.0:2000") {
  # Using plain-text
  URI = sprintf("mongodb://%s:%s@%s/", user, pass, host)
  mongo(url = URI, collection = collection, db = 'bcbet')
}

#' Adds survival stats to mongo 'collection' 
#'
#' @param ds The name of the datasets
#' @param Y The data frame containing clinical data
#' @collection The name of the collection (e.g., 'survival')
#' @subset A vector for subsetting samples, e.g., c('lg', 'nmi')
#' 

add_survival <- function(ds, Y, collection, subset = NULL) {
  
  # do this twice, second time, filter Y and change ds name for no_treated
  for (treated in c('include', 'remove')) {
  
    if (treated == 'remove') {
      if (is.null(Y$treated)) {
        next
      }
      
      Y <- dplyr::filter(Y, treated == 0)
      #ds <- paste0(ds, '_no_treated')
    }
    
    s <- sapply(c('dss', 'os', 'rfs', 'ba'), get_survival_n, Y = Y, subset = subset)
    s <- unlist(s)
    if (!is.null(s)) {
      
      if (!is.null(Y$treated)) {
        df <- data.frame(dataset = ds, treated = treated, t(s))
      } else {
        df <- data.frame(dataset = ds, t(s))
      }
      m <- mongo_connect(collection)
      
      # only delete dataset the first time
      if (treated == 'include') {
        qry <- paste0('{"dataset":"',ds, '"}')
        m$remove(qry)
      }
      m$insert(df)
    }
    
  }
  
  
}

# calculate number of samples for survival analysis
get_survival_n <- function(column, Y, subset = NULL) {
  keep <- paste0(column, c('_time', '_outcome'))
  if ( sum(keep %in% colnames(Y)) != 2) {
    return(NULL)
  } else if (!is.null(subset)) {
    if (!all(c('grade', 'stage') %in% colnames(Y))) {
      return(NULL)
    }
    Y <- dplyr::filter(Y, grade %in% subset[1], stage %in% subset[2])
  }
  
  n <- min(apply(Y[,keep], 2, function(x) sum(!is.na(x))))
  #names(n) <- column
  n
}

#' Gets sample size stats and inserts to mongo collection
#'
#' @param ds The name of the datasets
#' @param Y The data frame containing clinical data
#' 
get_stats <- function(ds, Y, hg_mi_cohorts) {
  
  count_stats <- function(x) {
    t <- table(x)
    names(t) <- paste0('N.', names(t))
    t
  }
  
  keep <- c('tumor', 'grade', 'stage') 
  keep <- intersect(keep, colnames(Y))
  
  stats <- lapply(Y[keep], count_stats)
  stats <- lapply(stats, function(x,ds) data.frame(dataset = ds, data.frame(rbind(x))), ds = ds)
  
  if (length(stats) > 0) {
    collections <- paste0('stats_', names(stats))
    for (i in 1:length(collections)) {
      m <- mongo_connect(collections[i])
      qry <- paste0('{"dataset":"',ds, '"}')
      m$remove(qry)
      rownames(stats[[i]]) <- NULL
      m$insert(stats[[i]])
    }
  }
    
  # stats <- plyr::alply(Y[keep], 2, count_stats)
  # stats <- unlist(stats)
  # if (!is.null(stats)) {
  #   names(stats) <- gsub('\\d+\\.', '', names(stats))
  # }
  
  if (ds %in% hg_mi_cohorts) {
    add_survival(ds, Y, 'stats_survival_hg_mi')
  } else {
    add_survival(ds, Y, 'stats_survival')
    add_survival(ds, Y, 'stats_survival_lg_nmi', c('lg', 'nmi'))
    add_survival(ds, Y, 'stats_survival_hg_mi', c('hg', 'mi'))
  }
  
  # if (!is.null(s) && !is.null(stats)) {
  #   return(data.frame(dataset = ds, t(stats), t(s)))
  # } else if (!is.null(s)) {
  #   return(data.frame(dataset = ds, t(s)))
  # }

  # data.frame(dataset = ds, t(stats))

}

# reads expression (X) and clinical (Y) data and adds to mongo db
addMongoData <- function(ds, D = NULL) {

  if (is.null(D)) D <- get_data(ds)

  m <- mongo_connect(paste0(ds, '_expr'))  
  m$drop()

  x <- D$X
  
  df <- apply(x, 1,
              function(x) {
                paste0('[',paste0(x, collapse = ','),']')
              }
  )
  df <- paste(paste0('"expr"'), df, sep = ':')
  df <- gsub('NA', '\"NA\"', df)
  x <- paste0('{', '"gene": "',rownames(x), '",',df, '}')
  m$insert(x)
  m$index(add = 'gene')
  m$disconnect(gc = FALSE)

  m <- mongo_connect(paste0(ds, '_clinical'))
  m$drop()
  m$insert(D$Y)
  m$disconnect(gc = FALSE)
  
}

mongo_add_genes <- function() {
  
  m <- mongo_connect('stage')
  cols <- m$run('{"listCollections":1, "nameOnly":"true"}')
  
  if (length(names(cols$cursor)) != 3) {
    stop('check mongo_add_genes -- do we have multiple batches?')
  }
  
  res <- cols$cursor$firstBatch
  res <- res %>% dplyr::filter(grepl('expr',name))
  
  ALL.GENES <- vector(mode = 'list', length = nrow(res))
  
  for (i in 1:nrow(res)) {
    ds <- res$name[i]
    m <- mongo_connect(ds)
    ALL.GENES[[i]] <- m$find(fields = '{"_id":0, "gene":1}')
  }
  
  genes <- do.call('rbind', ALL.GENES)
  hgnc_genes <- readRDS('../../data/genes/genes.rda')
  
  final_genes <- unique(genes$gene) %>% intersect(hgnc_genes)
  
  m <- mongo_connect('genes')
  m$drop()
  m$insert(data.frame(genes = sort(final_genes)))
  m$disconnect(gc = FALSE)
}
