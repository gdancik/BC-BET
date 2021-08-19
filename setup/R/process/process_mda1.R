# process MDA1 data 

library(GEOquery)

cat('processing mda1...\n\n')

rm(list = ls())
source('functions/setup_functions.R')

# specify dataset name and set file names appropriately
ds_name <- 'mda1'
file_expr <- paste0('../../data/processed/', ds_name, '.RData')
file_clinical <- paste0('../../data/clinical/', ds_name, '.RData')

# get expression and pheno data
GSE48276 <- getGEO("GSE48276", getGPL = PROCESS_EXPRESSION)
p <- pData(GSE48276[[1]])

if (PROCESS_EXPRESSION) {
  
  X <- exprs(GSE48276[[1]])
  
  # generate boxplot
  generate_boxplot(X, 'MDA1')
  
  load('../../data/platforms/GPL14951.RData')
  
  # get gene-level expression values
  mda1.expr <- get_expression(X, GPL14951)
}

keep = p$characteristics_ch1.4 %in% "nac: no" &
       p$characteristics_ch1.3 %in% "cystectomy histology: urothelial"

p <- p[keep,]

if (PROCESS_EXPRESSION) {
  mda1.expr <- mda1.expr[,keep]
  save(mda1.expr, file = file_expr)
}

stage = as.character(p$characteristics_ch1.2)
stage = gsub("pstage: ", "", stage)
stage = substr(stage, 1, 3)
tmp = rep(NA, length(stage))
tmp[stage %in% "pT1"] = 1 
tmp[stage %in% "pT2"] = 2 
tmp[stage %in% "pT3"] = 2 
tmp[stage %in% "pT4"] = 2 
stage = tmp 

keep = stage!= 1
stage = stage[keep]

# extract survival data
os.outcome <- rep(NA, nrow(p))
os.outcome[grep('os censor: uncensored', p$characteristics_ch1.8)] <- 0
os.outcome[grep('os censor: censored', p$characteristics_ch1.8)] <- 1
os.time <- as.double(gsub("survival \\(mo\\): ","", p$characteristics_ch1.10))

# create clinical data table
mda1_clinical <- create_clinical_table(id = rownames(p),
                            os_time = os.time, os_outcome = os.outcome)
 
save(mda1_clinical, file = file_clinical)
