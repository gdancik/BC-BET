# process lindgren1 data and lindgren2 data

library(GEOquery)

########################################################################
# lindgren 1
########################################################################

# this uses previously processed data 'GSE19915_MergedData_GeneExpression.txt.gz'
# from https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE19915
# clinical data is from: https://www.ebi.ac.uk/arrayexpress/experiments/E-GEOD-19915/files/

# Note: platform data is NOT updated

cat('processing lindgren1...\n\n')

rm(list = ls())
source('functions/setup_functions.R')

# load expression and platform data
load("../../data/original/GSE19915.merged.RData")

# load expression and platform data
lindgren1.expr <- GSE19915.expr

if (PROCESS_EXPRESSION) {
  # generate boxplot
  generate_boxplot(lindgren1.expr, 'lindgren1', TRUE)
}

df <- data.frame(GSE19915.samples)

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


########################################################################
# lindgren 2
########################################################################

# process lindgren2 data (GSE32548); some samples overlap with
# lindgren1 and we take information from there


cat('processing lindgren2...\n\n')

# load expression and platform data
dd <- getGEO(GEO = "GSE32548")

lindgren2.expr <- exprs(dd[[1]])

if (PROCESS_EXPRESSION) {
  # generate boxplot
  generate_boxplot(lindgren2.expr, 'lindgren2', TRUE)
  
  # get gene-level expression values
  load('../../data/platforms/GPL6947.RData')
  lindgren2.expr <- get_expression(lindgren2.expr, GPL6947)
}

GSE32548.p = pData(dd[[1]])

if (!all(colnames(lindgren2.expr) == rownames(GSE32548.p)))
  stop("patients not in correct order (p and expr)")


tmp = rep(NA, nrow(GSE32548.p))
tmp[grep("Ta", GSE32548.p$characteristics_ch1)] = "nmi"
tmp[grep("T1", GSE32548.p$characteristics_ch1)] = "nmi"
tmp[grep("T2", GSE32548.p$characteristics_ch1)] = "mi"
GSE32548.stage = tmp


tmp = rep(NA, nrow(GSE32548.p))
tmp[grep("G1", GSE32548.p$characteristics_ch1.1)] = "lg"
tmp[grep("G2", GSE32548.p$characteristics_ch1.1)] = "lg"
tmp[grep("G3", GSE32548.p$characteristics_ch1.1)] = "hg"
GSE32548.grade = tmp

######################################################################
# Some samples overlap, get survival information from lindgren1
######################################################################

l1.names = lindgren1_clinical$id

l2.names = as.character(GSE32548.p$source_name_ch1)
l2.names = gsub("Bladder tumor ", "", l2.names)

GSE32548.time = rep(NA, length(l2.names))
GSE32548.outcome = rep(NA, length(l2.names))
GSE32548.cystectomy = rep(NA, length(l2.names))

common = intersect(l1.names, l2.names)

m1 = match(common, l1.names)
m2 = match(common, l2.names)

GSE32548.time[m2] = lindgren1_clinical$dss_time[m1]
GSE32548.outcome[m2] = lindgren1_clinical$dss_outcome[m1]

# Note: all survival information is from lindgren1; 
# we are no longer looking at lg,nmi and hg,mi subsets, partly because
# of inconsistent cystectomy values, but we could look at lindgren1 for 
# cystectomy info if we need it


# create clinical data table
lindgren2_clinical <- create_clinical_table(id = l2.names, 
                                            grade = GSE32548.grade, 
                                            stage = GSE32548.stage,
                                            dss_time = GSE32548.time,
                                            dss_outcome = GSE32548.outcome)

colnames(lindgren2.expr) <- lindgren2_clinical$id

ds_name <- 'lindgren2'
file2_expr <- paste0('../../data/processed/', ds_name, '.RData')
file2_clinical <- paste0('../../data/clinical/', ds_name, '.RData')
rm(ds_name)

#############################################################
# create 4 datasets
#############################################################

library(dplyr)

# lindgren1a - lindgren1, all genes, tumor vs normal only
ds_name <- 'lindgren1a'
file1a_expr <- paste0('../../data/processed/', ds_name, '.RData')
file1a_clinical <- paste0('../../data/clinical/', ds_name, '.RData')
rm(ds_name)

lindgren1a_clinical <- select(lindgren1_clinical, id, tumor)
lindgren1a.expr <- lindgren1.expr

# lindgren1b - lindgren1, no normal samples, genes unique to lindgren1, 
ds_name <- 'lindgren1b'
file1b_expr <- paste0('../../data/processed/', ds_name, '.RData')
file1b_clinical <- paste0('../../data/clinical/', ds_name, '.RData')
rm(ds_name)

lindgren1b_clinical <- lindgren1_clinical %>% filter(tumor == 'tumor') %>% 
                      select(- one_of('tumor'))

keep_genes <- setdiff(rownames(lindgren1.expr), rownames(lindgren2.expr))
m_genes <- match(keep_genes, rownames(lindgren1.expr))
m_samples <- match(lindgren1b_clinical$id, colnames(lindgren1.expr))
lindgren1b.expr <- lindgren1.expr[m_genes,m_samples]


# lindgren1c - lindgren1, no normal samples, genes not unique to lindgren1, 
ds_name <- 'lindgren1c'
file1c_expr <- paste0('../../data/processed/', ds_name, '.RData')
file1c_clinical <- paste0('../../data/clinical/', ds_name, '.RData')
rm(ds_name)

lindgren1c_clinical <- lindgren1b_clinical
lindgren1c.expr <- lindgren1.expr[, m_samples]

common_samples <- intersect(colnames(lindgren1c.expr), lindgren2_clinical$id)
m_remove <- match(common_samples, colnames(lindgren1c.expr))

keep_genes <- intersect(rownames(lindgren1c.expr), rownames(lindgren2.expr))
m_genes <- match(keep_genes, rownames(lindgren1c.expr))
lindgren1c.expr[m_genes, m_remove] <- NA

 if (PROCESS_EXPRESSION) {
   save(lindgren1a.expr, file = file1a_expr)
   save(lindgren1b.expr, file = file1b_expr)
   save(lindgren1c.expr, file = file1c_expr)
   save(lindgren2.expr, file = file2_expr)
 }
 
 save(lindgren1a_clinical, file = file1a_clinical)
 save(lindgren1b_clinical, file = file1b_clinical)
 save(lindgren1c_clinical, file = file1c_clinical)
 save(lindgren2_clinical, file = file2_clinical)

