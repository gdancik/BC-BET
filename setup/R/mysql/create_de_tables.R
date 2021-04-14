library(RMariaDB)
suppressPackageStartupMessages(library(AUC))

source('functions.R')

library(argparse)
parser <- ArgumentParser()

parser$add_argument("--datasets", default="all",
                    help="comma-separated list of datasets to process [default %(default)s]")
parser$add_argument("--variables", default="all",
                   help="comma-separated list of variables to process [default %(default)s]")
parser$add_argument("--drop", default="no",
                    help="drop tables specified by --variables? (yes/no) [default %(default)s]")
parser$add_argument("--replace", default = "no",
                    help = paste0('replace dataset/variable combinations already in db (yes/no)',
                                  'if "yes", we first delete rows from db [default %(default)s]'))

args <- parser$parse_args()

#########################################################
# process arguments and check that arguments are valid
#########################################################

# check yes/no options
for (p in c('drop', 'replace')) {
  if (!args[[p]] %in% c('yes','no')) {
    stop('argument must be yes/no: ', p)
  }
}

if (args$drop == 'yes') {
  args$replace = 'no'
}

# process --datasets
datasets <- NULL
if (args$datasets == 'all') {
  datasets <- Sys.glob('../../data/clinical/*.RData')
  datasets <- gsub('\\.RData','',basename(datasets))
} else {
  datasets <- strsplit(args$datasets, ',')[[1]]
}

valid <- check_data(datasets)
if (!all(valid)) {
  stop('invalid dataset(s) detected: ', 
       paste(names(valid)[which(!valid)], collapse = ','), '\n')
}

# process --variables
# we will carry out DE based on each column of 'variables'
variables <- data.frame(tumor = c('normal', 'tumor'),
                        grade = c('lg', 'hg'),
                        stage = c('nmi', 'mi')
)

if (args$variables == 'all') {
  args$variables <- c('tumor', 'grade', 'stage')
} else {
  variables <- dplyr::select(variables, args$variables)
}



#############################################################################
# An example using 500 genes from a single dataset -- delete this when
# finished
#############################################################################
# DS <- get_data('mskcc')
# X <- DS$X
# Y <- DS$Y
# v <- 'stage'
# 
# res <- diff_expr_t_test(X[1:100,],Y[[v]],variables[[v]])   
# 
# #results <- dbGetQuery(con, 'SELECT * from stage LIMIT 10 ')
# #View(results)


#############################################################################
# Connect to Database 
#############################################################################

# Connect todatabase using a configuration file,
# (see tables 4.1 and 4.2 at 
#    https://dev.mysql.com/doc/refman/8.0/en/option-files.html)
con <- dbConnect(MariaDB(), group = "BCBET")


#############################################################################
# Create tables if they do not exist
#############################################################################

drop <- args$drop == "yes"
for (v in names(variables)) {
  create_table(con, v, drop = drop)
}
#############################################################################
# Loop through all datasets and all variables
#############################################################################

for (ds in datasets) {

  cat('loading data from: ', ds, '\n')
  DS <- get_data(ds)
  X <- DS$X
  Y <- DS$Y

  # for each phenotype (e.g., tumor, grade, stage)
  for (v in colnames(variables)) {
  
    # if we are replacing the data, then we need to delete it
    if (args$replace == "yes") {
      statement <- paste0('DELETE FROM ', v, ' where dataset = "', ds, '"')
      dbExecute(con, statement)
    } else { # otherwise skip if data exists
     qry <- paste0('select * from ', v, ' where dataset="',ds,'" limit 1')
     res <- dbGetQuery(con, qry)
     if (nrow(res) > 0) {
       cat('db already contains ', ds, '(', v, ') -- skipping\n' )
       next
     }
    }
    
    if (!is.null(Y[[v]])) {
      cat('DE analysis for ', ds, ':', v, '\n')
      res <- diff_expr_t_test(X, Y[[v]], variables[[v]])
      
      dataset <- rep(ds,nrow(res))
      gene <- row.names(res)
      
      res.df <- data.frame(gene,dataset,res)
      dbAppendTable(con,v, res.df,row.names = NULL)
  
    }
  }
  
  cat('\n')

}
dbDisconnect(con)
