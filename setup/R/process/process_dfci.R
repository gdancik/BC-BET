# process dfci data 

library(GEOquery)

cat('processing dfci...\n\n')

rm(list = ls())
source('functions/setup_functions.R')


# specify dataset name and set file names appropriately
ds_name <- 'dfci'
file_expr <- paste0('../../data/processed/', ds_name, '.RData')
file_clinical <- paste0('../../data/clinical/', ds_name, '.RData')

# load expression and platform data

dfci <- getGEO('GSE31684')
#View(dfci)

dfci.expr <- exprs(dfci[[1]])

load("../../data/platforms/GPL570.RData")

# generate boxplot
generate_boxplot(dfci.expr, 'dfci', FALSE)

keep <- apply(dfci.expr, 1, sd) > 0.01
dfci.expr <- dfci.expr[keep,]

# get gene-level expression values
dfci.expr <- get_expression(dfci.expr, GPL570)



#################################################

### extract clinical data -- limit to 
GSE31684.p <- pData(dfci[[1]])

#View(GSE31684.p)

#################################################

# get stages #

dfci.stage = rep(NA, nrow(GSE31684.p))
dfci.stage[GSE31684.p$characteristics_ch1.4 == "rc_stage: pTa"] <- "nmi"
dfci.stage[GSE31684.p$characteristics_ch1.4 == "rc_stage: pT1"] <- "nmi"
dfci.stage[GSE31684.p$characteristics_ch1.4 == "rc_stage: pT2"] <- "mi"
dfci.stage[GSE31684.p$characteristics_ch1.4 == "rc_stage: pT3"] <- "mi"
dfci.stage[GSE31684.p$characteristics_ch1.4 == "rc_stage: pT4"] <- "mi"

# get grade
dfci.grade = rep(NA, nrow(GSE31684.p))
dfci.grade[GSE31684.p$characteristics_ch1.7 == "rc grade: Low"] <- "lg"
dfci.grade[GSE31684.p$characteristics_ch1.7 == "rc grade: High"] <- "hg"

# create clinical data table
dfci_clinical <- create_clinical_table(id = rownames(GSE31684.p), grade = dfci.grade, stage = dfci.stage)

#View(dfci_clinical)

# save expression and clinical data
save(dfci.expr, file = file_expr)
save(dfci_clinical, file = file_clinical)

