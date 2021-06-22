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

if (PROCESS_EXPRESSION) {
  dfci.expr <- exprs(dfci[[1]])

  load("../../data/platforms/GPL570.RData")

  # generate boxplot
  generate_boxplot(dfci.expr, 'dfci', FALSE)

  keep <- apply(dfci.expr, 1, sd) > 0.01
  dfci.expr <- dfci.expr[keep,]

  # get gene-level expression values
  dfci.expr <- get_expression(dfci.expr, GPL570)
  save(dfci.expr, file = file_expr)
  
}

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

time <- outcome <- rep(NA, nrow(GSE31684.p))

for (col in paste0('characteristics_ch1.', 17:20)) {
  g <- grep('recurrence free', GSE31684.p[[col]])
  time[g] <- GSE31684.p[[col]][g]
  
  g <- grep('recurrence/dod', GSE31684.p[[col]])
  outcome[g] <- GSE31684.p[[col]][g]
  
}

myoutcome <- rep(NA, length(outcome))
myoutcome[outcome%in%"recurrence/dod: No"] <- 0
myoutcome[outcome%in%"recurrence/dod: Yes"] <- 1
mytime <- as.double(gsub('recurrence free survival months (distant and local): ', '', time, fixed = TRUE))


omit <- GSE31684.p$characteristics_ch1.14 == 'prerc_chemo: Yes'

mytime[omit] <- NA
myoutcome[omit] <- NA

treated <- GSE31684.p$characteristics_ch1.15 == 'post rc_chemo: Yes'

# create clinical data table
dfci_clinical <- create_clinical_table(id = rownames(GSE31684.p), grade = dfci.grade, 
                                       stage = dfci.stage, 
                                       treated = treated,
                                       rfs_time = mytime,
                                       rfs_outcome = myoutcome)

#View(dfci_clinical)

save(dfci_clinical, file = file_clinical)
