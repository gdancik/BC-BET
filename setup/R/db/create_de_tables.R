library(RMariaDB)
library(mongolite)

suppressPackageStartupMessages(library(AUC))

####################################################################
# Add data to mongo (expression/clinical) and mysql (DE results)
# databases; designed to work with lamp-rm 
#   (https://github.com/gdancik/lamp-rm)
####################################################################

source('functions.R')

library(argparse)
parser <- ArgumentParser(description = paste0("Create databases with expression/clinical data (mongo) and DE results (mysql). ",
                                              "NOTE: writing to mongo will delete corresponding mysql data")
)

parser$add_argument("--datasets", default="all",
                    help="comma-separated list of datasets to process [default %(default)s]")
parser$add_argument("--mongo", default = "no",
                    help = "create mongo db (yes/no) [default %(default)s]")
parser$add_argument("--var", default="all",
                   help="comma-separated list of clinical variables to process; must be \"all\" if mongo is \"yes\" [default %(default)s]")
parser$add_argument("--drop", default="no",
                    help="first drop tables specified by --var? (yes/no) [default %(default)s]")
parser$add_argument("--replace", default = "no",
                    help = paste0('replace dataset/variable combinations already in db (yes/no)',
                                  'if "yes", we first delete rows from db [default %(default)s]'))

args <- parser$parse_args()


#########################################################
# process arguments and check that arguments are valid
#########################################################

# check yes/no options
for (p in c('drop', 'replace', 'mongo')) {
  if (!args[[p]] %in% c('yes','no')) {
    stop('argument must be yes/no: ', p)
  }
}

if (args$mongo == "yes" && args$var != "all") {
  stop("var must be 'all' when mongo is 'yes'")
}

# set up drop/replace:
# - if we are dropping DE tables, don't replace the data
# - unless we are using mongo, then we do need to replace
if (args$drop == 'yes') {
  args$replace = 'no'
} else if (args$mongo == 'yes') {
  args$replace = 'yes'
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

if (args$var == 'all') {
  args$var <- c('tumor', 'grade', 'stage')
} else {
  variables <- dplyr::select(variables, args$var)
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

cat('creating mysql tables if needed...')
drop <- args$drop == "yes"
for (v in names(variables)) {
  create_table(con, v, drop = drop)
}
cat('done\n')

#############################################################################
# Loop through all datasets and all variables
#############################################################################

for (ds in datasets) {

  cat('loading data from: ', ds, '\n')
  DS <- get_data(ds)
  
  if (args$mongo == "yes") {
    cat('  adding ', ds, ' data to mongo...\n')
    addMongoData(ds, DS)
  }
  
  X <- DS$X
  Y <- DS$Y

  # for each phenotype (e.g., tumor, grade, stage)
  for (v in colnames(variables)) {
  
    # if we are replacing the data, then we need to delete it
    if (args$replace == "yes") {
      cat('  deleting ', ds, ' from mysql table ', v, '...\n')
      statement <- paste0('DELETE FROM ', v, ' where dataset = "', ds, '"')
      dbExecute(con, statement)
    } else { # otherwise skip if data exists
     qry <- paste0('select * from ', v, ' where dataset="',ds,'" limit 1')
     res <- dbGetQuery(con, qry)
     if (nrow(res) > 0) {
       cat('  db already contains ', ds, '(', v, ') -- skipping\n' )
       next
     }
    }
    
    if (!is.null(Y[[v]])) {
      cat('  DE analysis for ', ds, ':', v, '\n')
      res <- diff_expr_t_test(X, Y[[v]], variables[[v]])
      
      dataset <- rep(ds,nrow(res))
      gene <- row.names(res)
      
      res.df <- data.frame(gene,dataset,res)
      cat('  adding results to mysql ...\n')
      dbAppendTable(con,v, res.df,row.names = NULL)
    }
  }
  
  cat('\n')

}
dbDisconnect(con)
