# process MSKCC (SC) data 

cat('processing mskcc...\n\n')

rm(list = ls())
source('functions/get_gene_expression.R')
source('functions/generate_boxplot.R')

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

rm(SC.expr)

# save data
save(list = ls(), file = '../../data/processed/mskcc.RData')

