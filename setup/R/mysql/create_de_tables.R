library(RMariaDB)

# Connect to employee database using a configuration file,
# (see tables 4.1 and 4.2 at 
#    https://dev.mysql.com/doc/refman/8.0/en/option-files.html)
con <- dbConnect(MariaDB(), group = "BCBET")

#' Carries out a two-sample t-test for vector 'x'
#'
#' @param x A vector of expression values
#' @param y A vector of pheno values corresponding to 'x'
#' @param groups A vector of group labels; fc is calculated as
#'        group2 - group1
#' @return A matrix containing the FC and p-value from the t-test
#'          
diff_expr_t_test1 <- function(x, y, groups) {
  res <- t.test(x ~ y) 
  groups <- paste('mean in group', groups)
  logfc <- res$estimate[groups[2]] - res$estimate[groups[1]]
  cbind(fc = 2**unname(logfc), pvalue = res$p.value)
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
  colnames(res) <- c('fc', 'pvalue')
  res
}

#' Loads data from specified datasets
#'
#' @param ds The name of the datasets
#' @return A list containing expression matrix X and clinical table Y
#' 
get_data <- function(ds) {
  load(paste0('../../data/processed/', ds, '.RData'))
  load(cfile <- paste0('../../data/clinical/', ds, '.RData'))
  
  list(X = get(paste0(ds,'.expr')),
       Y = get(paste0(ds, '_clinical')))
  
}

# we will carry out DE based on each column of 'variables'
variables <- data.frame(tumor = c('normal', 'tumor'),
                        grade = c('lg', 'hg'),
                        stage = c('nmi', 'mi')
)

#############################################################################
# An example using 500 genes from a single dataset -- delete this when
# finished
#############################################################################
# DS <- get_data('mskcc')
# X <- DS$X
# Y <- DS$Y
# v <- 'stage'
# 
#
create_table <- function(con, table_name, remove = TRUE) {

  if (remove) {
    dbRemoveTable(con,table_name, fail_if_missing = FALSE)
  }


  dbCreateTable(con, table_name, row.names = NULL, temporary = FALSE,
              fields = c(gene = 'varchar(40)',
                         dataset = 'varchar(40)',
                         fc = 'double', pvalue = 'double') 
              )
  alt <- "ALTER TABLE"
  tabName <- table_name
  addInd <- "ADD INDEX `ind` (`gene`);"
  
  dbExecute(con,paste(alt,tabName,addInd, sep=" ", collapse=NULL) )
  
}


create_table(con, 'tumor')
create_table(con, 'grade')
create_table(con, 'stage')
# 
# #results <- dbGetQuery(con, 'SELECT * from stage LIMIT 10 ')
# #View(results)





#############################################################################
# Loop through all datasets and all variables
#############################################################################

datasets <- c('mskcc', 'auh2', 'mda2','blaveri','cnuh','mda1','stransky1','stransky2','uva')
#dataset2 <- c('auh1')
# iterate over all datasets
for (ds in datasets) {

  cat('loading data from: ', ds, '\n')
  DS <- get_data(ds)
  X <- DS$X
  Y <- DS$Y

  # for each phenotype (e.g., tumor, grade, stage)
  for (v in colnames(variables)) {
  
    if (!is.null(Y[[v]])) {
      cat('DE analysis for ', ds, ':', v, '\n')
      res <- diff_expr_t_test(X, Y[[v]], variables[[v]])
      
      # add data to the appropriate table (e.g.,
      # stage data should be added to 'stage' table)
      
      dataset <- rep(ds,nrow(res))
      gene <- row.names(res)
      
      res.df <- data.frame(gene,dataset,res)
      dbAppendTable(con,v, res.df,row.names = NULL)
  
      
    }
  }
  
  cat('\n')

}
;jkdbDisconnect(con)
