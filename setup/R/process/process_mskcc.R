# process MSKCC (SC) data 

cat('processing mskcc...\n\n')

rm(list = ls())
source('functions/setup_functions.R')

# specify dataset name and set file names appropriately
ds_name <- 'mskcc'
file_expr <- paste0('../../data/processed/', ds_name, '.RData')
file_clinical <- paste0('../../data/clinical/', ds_name, '.RData')

# load expression and platform data
load("../../data/original/Sanchez.RData")
load("../../data/platforms/GPL96.RData")

# we need to take the log for this one
SC.expr <- log2(SC.expr)
SC.expr <- as.matrix(SC.expr)

# generate boxplot
generate_boxplot(SC.expr, 'MSKCC')

# get gene-level expression values
mskcc.expr <- get_expression(SC.expr, GPL96)

### extract clinical data
SC.stage = rep(NA,length(SC.stage.T))
SC.stage[SC.stage.T == 0] <- 'nmi'
SC.stage[SC.stage.T == 1] <- 'nmi'
SC.stage[SC.stage.T > 1] <- 'mi'

SC.tumor <- as.double(SC.p$TYPE != 0)
SC.tumor <- factor(SC.tumor, labels = c('normal', 'tumor'))

SC.grade <- factor(SC.grade, labels = c('lg', 'hg'))

# create clinical data table
mskcc_clinical <- create_clinical_table(id = colnames(SC.expr), tumor = SC.tumor, 
                             grade = SC.grade, stage = SC.stage,
                             dss_time = SC.OS.time,
                             dss_outcome = SC.OS.outcome)

# save expression and clinical data
save(mskcc.expr, file = file_expr)
save(mskcc_clinical, file = file_clinical)
