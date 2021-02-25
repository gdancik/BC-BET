# process AUH-2 data 

cat('processing auh2...\n\n')

rm(list = ls())
source('functions/setup_functions.R')

# specify dataset name and set file names appropriately
ds_name <- 'auh2'
file_expr <- paste0('../../data/processed/', ds_name, '.RData')
file_clinical <- paste0('../../data/clinical/', ds_name, '.RData')

# load expression and platform data
load("../../data/original/GSE5479.impute.RData")
load("../../data/platforms/GPL4060.RData")

auh2.expr <- GSE5479.expr

# generate boxplot
generate_boxplot(auh2.expr, 'AUH-2')

# get gene-level expression values
auh2.expr <- get_expression(auh2.expr, GPL4060)

### extract clinical data
GSE5479.stage <- factor(GSE5479.stage, labels = c('nmi', 'mi'))

GSE5479.grade <- factor(GSE5479.grade, labels = c('lg', 'hg'))

# create clinical data table
auh2_clinical <- create_clinical_table(id = paste0('S', 1:ncol(auh2.expr)),
                             grade = GSE5479.grade, 
                             stage = GSE5479.stage)

# set names (matching not done in this pipeline)
colnames(auh2.expr) <- auh2_clinical$id

# save expression and clinical data
save(auh2.expr, file = file_expr)
save(auh2_clinical, file = file_clinical)
