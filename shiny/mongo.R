library(mongolite)


if (is.null(getOption('mongo.host'))) {

       mm <- Sys.getenv('MONGO_HOST')
       if (mm == '') {
           mm <- "0.0.0.0:2000"
       }
       options(mongo.host = mm)
       rm(mm)
}

mongo_connect <- function(collection, user = "root", pass = "password",
                          host = getOption('mongo.host')) {
  # Using plain-text
  URI = sprintf("mongodb://%s:%s@%s/", user, pass, host)
  mongo(url = URI, collection = collection, db = 'bcbet')
}


mongo_get_de_results <- function(qry, cols) {
  # get DE results
  types <- c('tumor', 'grade', 'stage')
  res1 <- sapply(types, function(x, qry, cols) {
    m <- mongo_connect(x)
    fields <- paste0('{"_id":0, "gene":1, "dataset":1, ',
                     paste0('"', cols, '":1', collapse = ', '),
                     '}')
    m$find(qry, fields = fields)
  }, qry = qry, cols = cols, USE.NAMES = TRUE, simplify = FALSE)
  
  for (n in names(res1)) {
    if (nrow(res1[[n]]) == 0) {
      res1[[n]] <- NULL
    }
  }
  res1
}

mongo_get_survival_results <- function(qry, cutpoint, endpoint, treated) {
  
  # get survival results
  types <- c('survival', 'survival_lg_nmi', 'survival_hg_mi')

  if (treated == 'yes') {
    treated_qry <- '"treated": {"$ne": "remove"}'
  } else {
    treated_qry <- '"treated": {"$ne": "include"}'
  }

  res2 <- sapply(types, function(x, qry, cutpoint, treated_qry) {
    m <- mongo_connect(x)
    qry <- gsub("}$", paste0(',', treated_qry, "}"), qry)
    fields = paste0('{"_id":0, "gene":1, "dataset":1, "endpoint":1,',
                  paste0( c('\"hr_', '\"p_'), cutpoint, '\":1', collapse = ', '),
             '}')
  
    m$find(qry, fields = fields)
  }, qry = qry, cutpoint = cutpoint, treated_qry = treated_qry, USE.NAMES = TRUE, simplify = FALSE)

  
  for (n in names(res2)) {
    if (nrow(res2[[n]]) == 0) {
      res2[[n]] <- NULL
    }
  }

  res2 <- lapply(res2, arrange, dataset)
  res2 <- lapply(res2, get_endpoint, endpt = endpoint)
  res2
}

# helper function for mongo_get_survival_results
get_endpoint <- function(x, endpt) {
  if (nrow(x) == 0) {
    return(x)
  }
  
  if (endpt != 'ba') {
    return(x %>% dplyr::filter(endpoint == endpt))
  }
  x  %>% arrange(endpoint) %>% group_by(gene,dataset) %>% slice_head()
}




