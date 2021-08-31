# process elamin_blood data 

library(GEOquery)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2) #otherwise we have an error in Github workflow

cat('processing elamin_blood...\n\n')

rm(list = ls())
source('functions/setup_functions.R')

# specify dataset name and set file names appropriately
ds_name <- 'elamin_blood'
file_expr <- paste0('../../data/processed/', ds_name, '.RData')
file_clinical <- paste0('../../data/clinical/', ds_name, '.RData')

# load expression and platform data

elamin <- getGEO('GSE138118', getGPL = PROCESS_EXPRESSION)
elamin_blood.expr <- exprs(elamin[[1]])

if (PROCESS_EXPRESSION) {

  load("../../data/platforms/GPL17692.RData")
  
  # generate boxplot
  generate_boxplot(elamin_blood.expr, 'elamin_blood')

  # get gene-level expression values
  elamin_blood.expr <- get_expression(elamin_blood.expr, GPL17692)
}

### extract clinical data -- limit to 
elamin.p <- pData(elamin[[1]])
tumor <- gsub(' Female| Male', '', elamin.p$source_name_ch1)
tumor <- gsub('Health volunteer', 'normal', tumor)
tumor <- gsub('UCB positive', 'tumor', tumor)


# not enough samples for stage/grade

# library(dplyr)
# sapply(strsplit(elamin.p$characteristics_ch1, ':'), `[`, 2) %>% table()
# sapply(strsplit(elamin.p$characteristics_ch1.1, ':'), `[`, 2) %>% table()

# create clinical data table
elamin_blood_clinical <- create_clinical_table(id = rownames(elamin.p), 
                                               tumor = tumor)

# save expression and clinical data
if (PROCESS_EXPRESSION) {
  save(elamin_blood.expr, file = file_expr)
}

save(elamin_blood_clinical, file = file_clinical)
