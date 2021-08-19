# process lindgren1 data 
# this uses previously processed data 'GSE19915_MergedData_GeneExpression.txt.gz'
# from https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE19915
# clinical data is from: https://www.ebi.ac.uk/arrayexpress/experiments/E-GEOD-19915/files/

# Note: platform data is NOT updated

cat('processing lindgren1...\n\n')

rm(list = ls())
source('functions/setup_functions.R')

# load expression and platform data
load("../../data/original/GSE19915.merged.RData")

# specify dataset name and set file names appropriately
ds_name <- 'lindgren1'
file_expr <- paste0('../../data/processed/', ds_name, '.RData')
file_clinical <- paste0('../../data/clinical/', ds_name, '.RData')

# load expression and platform data
lindgren1.expr <- GSE19915.expr

if (PROCESS_EXPRESSION) {

  #################################################

  # generate boxplot
  generate_boxplot(lindgren1.expr, 'lindgren1', TRUE)

}

df <- data.frame(GSE19915.samples)

if (PROCESS_EXPRESSION) {
  save(lindgren1.expr, file = file_expr)
}


# tumor / normal
lindgren1.tumor = rep(NA,nrow(df))
lindgren1.tumor[df$histology == 'Normal bladder'] <- "normal"
lindgren1.tumor[df$histology == 'Urothelial carcinoma'] <- "tumor"

# nmi / mi
lindgren1.stage <- rep(NA, nrow(df))
g1 <- grep('T[a1]', df$stage)
g2 <- grep('T[234]', df$stage)
lindgren1.stage[g1] <- 'nmi'
lindgren1.stage[g2] <- 'mi'

# lg/hg
lindgren1.grade <- rep(NA, nrow(df))
g1 <- grep('G[12]', df$grade)
g2 <- grep('G[34]', df$grade)
lindgren1.grade[g1] <- 'lg'
lindgren1.grade[g2] <- 'hg'

# survival
Lindgren.time = as.integer(df$follow.up.time)
Lindgren.outcome = rep(NA,nrow(df))
Lindgren.outcome[df$dead.of.disease == "YES"] = 1
Lindgren.outcome[df$dead.of.disease == "NO"] = 0

# create clinical data table
lindgren1_clinical <- create_clinical_table(id = colnames(lindgren1.expr), 
                                           tumor = lindgren1.tumor, 
                                           grade = lindgren1.grade, 
                                           stage = lindgren1.stage,
                                           dss_time = Lindgren.time,
                                           dss_outcome = Lindgren.outcome)

save(lindgren1_clinical, file = file_clinical)
