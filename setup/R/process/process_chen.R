# process chen (GSE77952) data set 

library(GEOquery)

cat('processing chen...\n\n')

rm(list = ls())
source('functions/setup_functions.R')

# specify dataset name and set file names appropriately
ds_name <- 'chen'
file_expr <- paste0('../../data/processed/', ds_name, '.RData')
file_clinical <- paste0('../../data/clinical/', ds_name, '.RData')

# load expression and platform data
chen <- getGEO('GSE77952', getGPL = PROCESS_EXPRESSION)
chen.expr <- exprs(chen[[1]])

if (PROCESS_EXPRESSION) {

  load("../../data/platforms/GPL6480.RData")

  # generate boxplot
  generate_boxplot(chen.expr, 'chen', TRUE)

  # get gene-level expression values
  chen.expr <- get_expression(chen.expr, GPL6480)

}


### extract clinical data -- limit to 
chen.p <- pData(chen[[1]])

stage <- gsub('_.*', '', chen.p$title)
stage <- gsub('BC$', '', stage)
stage <- tolower(stage)


if (PROCESS_EXPRESSION) {
  save(chen.expr, file = file_expr)
}

##################################################################
# create clinical data table
chen_clinical <- create_clinical_table(id = colnames(chen.expr),  
                                       stage = stage)
save(chen_clinical, file = file_clinical)
