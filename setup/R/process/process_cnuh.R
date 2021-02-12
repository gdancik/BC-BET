# process cnuh  

rm(list = ls())
source('functions/get_gene_expression.R')
source('functions/generate_boxplot.R')

# load expression and platform data
load("../../data/original/GSE13507.RData")
load("../../data/platforms/GPL6102.RData")

# generate boxplot
generate_boxplot(GSE13507.expr, 'CNUH')

# get gene-level expression values
GSE13507.expr <- get_expression(GSE13507.expr, GPL6102)

# save data
save(list = ls(), file = '../../data/processed/cnuh.RData')
