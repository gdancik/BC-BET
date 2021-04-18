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

#' creates a table in the database, if the table does not exist
#'
#' @param con The connection to the database
#' @table_name The name of the table
#' @drop if TRUE, the table is first dropped 
#' @return a logical vector containing TRUE if processed and clinical data exists
#'
create_table <- function(con, table_name, drop = TRUE) {
  
  if (drop) {
    dbRemoveTable(con,table_name, fail_if_missing = FALSE)
  }
  if (dbExistsTable(con, table_name)) {
    return()
  }
  dbCreateTable(con, table_name, row.names = NULL, temporary = FALSE,
                fields = c(gene = 'varchar(40)',
                           dataset = 'varchar(40)',
                           fc = 'double', pt = 'double',
                           auc = 'double', pw = 'double') 
  )
  alt <- "ALTER TABLE"
  tabName <- table_name
  addInd <- "ADD INDEX `ind` (`gene`);"
  
  dbExecute(con,paste(alt,tabName,addInd, sep=" ", collapse=NULL) )
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
  x <- paste0('{', '"gene": "',rownames(x), '",',df, '}')
  m$insert(x)
  m$index(add = 'gene')
  m$disconnect()

  m <- mongo_connect(paste0(ds, '_clinical'))
  m$insert(D$Y)
  m$disconnect()
}
