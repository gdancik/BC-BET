# process mda2 data (GSE48075)

cat('processing mda2...\n\n')

rm(list = ls())
source('functions/get_gene_expression.R')
source('functions/generate_boxplot.R')

# specify dataset name and set file names appropriately
ds_name <- 'mda2'
file_expr <- paste0('../../data/processed/', ds_name, '.RData')
file_clinical <- paste0('../../data/clinical/', ds_name, '.RData')

# load expression data
library(GEOquery)
GSE48075 = getGEO('GSE48075')
mda2.expr <- exprs(GSE48075[[1]])
GSE48075.p <- pData(GSE48075[[1]])

# load platform data
load("../../data/platforms/GPL6947.RData")

# generate boxplot
generate_boxplot(mda2.expr, 'MDA-2')

# get gene-level expression values
mda2.expr <- get_expression(mda2.expr, GPL6947)

## get clinical data
# stage info is in both ch1 and ch1.4

tmp <- rep(NA, ncol(mda2.expr))
g1 <- grep("cstage", GSE48075.p$characteristics_ch1)
g2 <- grep("cstage", GSE48075.p$characteristics_ch1.4)
if (length(intersect(g1,g2)) > 0) {
  stop ("at least one sample has multiple cstages")
}

tmp[g1] <-  as.character(GSE48075.p$characteristics_ch1[g1])
tmp[g2] <-  as.character(GSE48075.p$characteristics_ch1.4[g2])

stage <- rep(NA, length(tmp)) 
stage[grep("Ta",tmp)] <- 'nmi'
stage[grep("T1",tmp)] <- 'nmi'
stage[grep("T2",tmp)] <- 'mi'
stage[grep("T3",tmp)] <- 'mi'
stage[grep("T4",tmp)] <- 'mi'


#####################################
# is survival time for OS or DSS?
#####################################

# time = as.character(GSE48075.p$characteristics_ch1.8)
# time = as.double(gsub("survival \\(mo\\): ","", time))
# 
# tmp = as.character(GSE48075.p$characteristics_ch1.6)
# outcome = rep(NA, length(tmp))
# outcome[tmp == "os censor: uncensored"] = 1
# outcome[tmp == "os censor: censored"] = 0
# 
# GSE48075.time = time
# GSE48075.outcome = outcome

# create clinical data table
mda2_clinical <- data.frame(id = colnames(mda2.expr), 
                            stage = stage)

# save expression and clinical data
save(mda2.expr, file = file_expr)
save(mda2_clinical, file = file_clinical)
