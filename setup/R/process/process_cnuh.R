# process cnuh  

rm(list = ls())
source('functions/setup_functions.R')

# specify dataset name and set file names appropriately
ds_name <- 'cnuh'
file_expr <- paste0('../../data/processed/', ds_name, '.RData')
file_clinical <- paste0('../../data/clinical/', ds_name, '.RData')

# load expression and platform data
load("../../data/original/GSE13507.RData")

if (PROCESS_EXPRESSION) {
  load("../../data/platforms/GPL6102.RData")
  
  # generate boxplot
  generate_boxplot(GSE13507.expr, 'CNUH')
  
  # get gene-level expression values
  cnuh.expr <- get_expression(GSE13507.expr, GPL6102)
  save(cnuh.expr, file = file_expr)
}

# get clinical information for tumor

tumor = rep(NA,nrow(GSE13507.p))
tumor[grep("normal", GSE13507.p$characteristics_ch1)] = 'normal'
tumor[grep("bladder cancer", GSE13507.p$characteristics_ch1)] = 'tumor'

# clinical information for grade 
grade <- factor(GSE13507.grade, labels = c("hg", "lg"))

# clinical information for stage
stage <- rep(NA, length(grade))
stage[grep("^a$|1", GSE13507.stage.T)] <- "nmi"
stage[grep("[2-4]", GSE13507.stage.T)] <- "mi"

treated = as.integer(GSE13507.chemo%in%1 | GSE13507.intravesical %in% 1)

cnuh_clinical <- create_clinical_table(id = rownames(GSE13507.p),
                      tumor = tumor, grade = grade,
                      stage = stage, treated = treated,
                      dss_time = GSE13507.DSS.time,
                      dss_outcome = GSE13507.DSS.outcome,
                      os_time = GSE13507.OS.time,
                      os_outcome = GSE13507.OS.outcome)

# save data
save(cnuh_clinical, file = file_clinical)
