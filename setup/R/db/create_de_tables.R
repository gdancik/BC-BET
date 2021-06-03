library(mongolite)
suppressPackageStartupMessages(library(AUC))

####################################################################
# Add data to mongo (expression, clinical, and DE/surv results)
# databases; designed to work with lamp-rm 
#   (https://github.com/gdancik/lamp-rm)
####################################################################

# Note: this needs to be consistent with /shiny/
HG_MI_COHORTS <- c('mda1','mda2')

source('functions.R')

library(argparse)
parser <- ArgumentParser(description = paste0("Creates BC-BET mongo database. ",
                                              "NOTE: writing expression data will delete corresponding result data and update the stats")
)

parser$add_argument("--datasets", default="all",
                    help="comma-separated list of datasets to process [default %(default)s]")
parser$add_argument("--expression", default = "no",
                    help = "add expression data (yes/no) [default %(default)s]")
parser$add_argument("--var", default="all",
                   help="comma-separated list of clinical variables to process, or \"none\"; must be \"all\" if expression is \"yes\" [default %(default)s]")
parser$add_argument("--survival", default = "yes",
                    help = "carry out survival analysis (yes/no) [default %(default)s]")
parser$add_argument("--drop", default="no",
                    help="(NOT IMPLEMENTED) first drop tables specified by --var or --surv (yes/no) [default %(default)s]")
parser$add_argument("--replace", default = "no",
                    help = paste0('replace dataset/variable combinations already in db (yes/no)',
                                  'if "yes", we first delete rows from db [default %(default)s]'))

args <- parser$parse_args()


#########################################################
# process arguments and check that arguments are valid
#########################################################

# check yes/no options
for (p in c('drop', 'replace', 'expression', 'survival')) {
  if (!args[[p]] %in% c('yes','no')) {
    stop('argument must be yes/no: ', p)
  }
}

# if (args$var == 'none' && args$survival == 'no') {
#   stop('var cannot be none when survival is no')
# }

# if (args$expression == "yes" && args$var != "all") {
#   stop("var must be 'all' when expression is 'yes'")
# }

# set up drop/replace:
# - if we are dropping DE tables, don't replace the data
# - unless we are using mongo, then we do need to replace
if (args$drop == 'yes') {
  args$replace = 'no'
} else if (args$expression == 'yes') {
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

# process -- de_variables
# we will carry out DE based on each column of 'de_variables'
de_variables <- data.frame(tumor = c('normal', 'tumor'),
                        grade = c('lg', 'hg'),
                        stage = c('nmi', 'mi')
)

if (args$var == 'all') {
  args$var <- c('tumor', 'grade', 'stage')
} else if (args$var == 'none') {
  de_variables <- vector(length=0)
} else {
  cols <- strsplit(args$var, ',')[[1]]
  if (any(!cols%in%colnames(de_variables))) {
    stop('Invalid var=', args$var)
  }
  
  de_variables <- dplyr::select(de_variables, 
                                cols)
}

# get survival column names, e.g.,
# dss => dss_time and dss_outcome
survival_columns <- function(type) {
  list(times = paste0(type, '_time'),
       outcomes = paste0(type, '_outcome'))
}

#############################################################################
# An example using 500 genes from a single dataset -- delete this when
# finished
############################################################################
# DS <- get_data('mskcc')
# X <- DS$X
# Y <- DS$Y
# v <- 'stage'
# 
# res_de <- diff_expr_t_test(X[1:100,],Y[[v]],de_variables[[v]])
# 
# # add to mongo db
# genes <- row.names(res_de)
# rownames(res_de) <- NULL
# res_de <- data.frame(gene = genes,dataset=ds,res_de)
# 
# m <- mongo_connect(v)
# m$insert(data.frame(res_de))
# m$disconnect()


# 
# sv <- 'dss' 

# v2 <- survival_columns(sv)
# res_km <- coxph_test(X[1:100,], Y[[v2$times]], Y[[v2$outcomes]])


survival_variables <- NULL
if (args$surv == "yes") {
  survival_variables <- c('dss', 'os', 'rfs')
}


# replace data from dataset 'ds' in table 'v' if replace is 'yes';
#   otherwise return 'skip' if data already exists
#   otherwise return 'add'
#   we optionally include "endpoint":endpoint in the query
replace_or_skip <- function(con, replace, ds, v, endpoint = NULL) {
  
  qry <- paste0('{"dataset":"', ds, '"}')
  if (!is.null(endpoint)) {
    qry <- paste0('{"dataset":"', ds, '", "endpoint":"', endpoint, '"}')
  }
  
  # if we are replacing the data, then we need to delete it
  if (replace == "yes") {
    cat('  deleting ', ds, ' from mongo table ', v, ' with endpoint ', endpoint, '...\n')
    m <- mongo_connect(v)
    m$remove(qry)
    return('replace')
  } else { # otherwise skip if data exists
    m <- mongo_connect(v)
    res <- m$count(qry)
    if (res > 0) {
      cat('  db already contains ', ds, '(', v, endpoint, ') -- skipping\n' )
      return('skip')
    }
  }
  return('add')
}


#############################################################################
# Loop through all datasets and all variables
#############################################################################

for (ds in datasets) {

  cat('loading data from: ', ds, '\n')
  DS <- get_data(ds)
  
  if (args$expression == "yes") {
    cat('  adding ', ds, ' data to mongo...\n')
    addMongoData(ds, DS)
  }
  
  X <- DS$X
  Y <- DS$Y

  # for each phenotype (e.g., tumor, grade, stage)
  for (v in colnames(de_variables)) {
  
    if (replace_or_skip(con, args$replace, ds, v) == 'skip') {
      next
    }
      
    if (!is.null(Y[[v]])) {
      cat('  DE analysis for ', ds, ':', v, '\n')
      res <- diff_expr_t_test(X, Y[[v]], de_variables[[v]])
      
      gene <- row.names(res)
      res.df <- data.frame(gene,dataset=ds,res)
      rownames(res.df) <- NULL
      cat('   adding results to mongo ...\n')
      m <- mongo_connect(v)
      m$insert(data.frame(res.df))
      m$disconnect()
    }
    
  } # end loop for de_variables
  
  # handle survival if necessary
  if (args$surv == 'no') {
    next
  }
  
  for (survival_table in c('survival', 'survival_lg_nmi', 'survival_hg_mi')) {

    ba_added <- FALSE
    for (v in survival_variables) {
      if (replace_or_skip(con, args$replace, ds, survival_table, v) == 'skip') {
        next
      }

      # skip HG_MI_COHORTS if we are looking at survival or lg_nmi
      if (ds %in% HG_MI_COHORTS && survival_table %in% c('survival', 'survival_lg_nmi')) {
        next
      }
      
      vs <- survival_columns(v)
      if (is.null(Y[[vs$times]])) {
        next
      }
      
      cat('  survival analysis for ', ds, ':', v, ':', survival_table, '\n')

      if (survival_table == 'survival') {      
        keep <- 1:nrow(Y)
      } else if (survival_table == 'survival_lg_nmi') {
        keep <- Y$grade == 'lg' & Y$stage == 'nmi'
      } else if (survival_table == 'survival_hg_mi') {
        if (ds %in% HG_MI_COHORTS) {
          keep <- 1:nrow(Y)
        } else {
          keep <- Y$grade == 'hg' & Y$stage == 'mi'
        }
      } else {
        stop('invalid survival_table: ', survival_table)
      }
      
      t <- table(Y[[vs$outcome]][keep])
      
      # currently skip if n < 10 or more than 90% of patients are censured
      if (sum(t) < 10 || proportions(t)['0']>0.90) {
        cat('  insufficient samples for ', ds, ' (', survival_table, ') -- skipping\n')
        next
      }

      res <- coxph_test(X[,keep], Y[[vs$times]][keep], Y[[vs$outcomes]][keep])
      gene <- row.names(res)
      res.df <- data.frame(gene, dataset = ds, endpoint = v, res)
      rownames(res.df) <- NULL
      cat('  adding results to mongo ...\n')
      m <- mongo_connect(survival_table)
      m$insert(data.frame(res.df))
      m$disconnect(gc = FALSE)
      
      # if (!ba_added) {
      #   res.df <- data.frame(gene, dataset = ds, endpoint = 'ba', res)
      #   rownames(res.df) <- NULL
      #   cat('  adding ba results to mongo ...\n')
      #   m <- mongo_connect(survival_table)
      #   m$insert(data.frame(res.df))
      #   m$disconnect(gc = FALSE)
      #   ba_added <- TRUE
      # }
      
    }
    
    cat('\n')
  }

}

# add mongo indices
collections <- colnames(de_variables)
if (args$expression == "yes") {
  collections <- c(collections, survival_variables, 'ba')
}

for (v in collections) {
  m <- mongo_connect(v)
  if (m$count() > 0) {
    m$index('{"gene": 1}')
  }
}
