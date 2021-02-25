# process UVA data 

library(GEOquery)

cat('processing uva...\n\n')

rm(list = ls())
source('functions/setup_functions.R')

# specify dataset name and set file names appropriately
ds_name <- 'uva'
file_expr <- paste0('../../data/processed/', ds_name, '.RData')
file_clinical <- paste0('../../data/clinical/', ds_name, '.RData')

# load expression and platform data

UVA <- getGEO('GSE37317')

uva.expr <- exprs(UVA[[1]])

load("../../data/platforms/GPL96.RData")

# generate boxplot
generate_boxplot(uva.expr, 'UVA')

# get gene-level expression values
uva.expr <- get_expression(uva.expr, GPL96)

### extract clinical data -- limit to 
GSE37317.p <- pData(UVA[[1]])

keep <- GSE37317.p$characteristics_ch1.2 == "histology: urothelial carcinoma"
uva.expr = uva.expr[,keep]
GSE37317.p = GSE37317.p[keep,]

# get stages #
uva.stage = rep(NA, nrow(GSE37317.p))
uva.stage[GSE37317.p$characteristics_ch1.1 == "muscle invasion: non-muscle invasive"] <- "nmi"
uva.stage[GSE37317.p$characteristics_ch1.1 == "muscle invasion: muscle invasive"] <- "mi"

# create clinical data table
uva_clinical <- create_clinical_table(id = colnames(uva.expr), stage = uva.stage)

# save expression and clinical data
save(uva.expr, file = file_expr)
save(uva_clinical, file = file_clinical)
