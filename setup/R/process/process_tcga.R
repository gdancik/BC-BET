library(dplyr)
library(ggplot2)
library(UCSCXenaTools) # needed to retreive data
library(edgeR) # needed for processing, such as TMM

rm(list = ls())
source('functions/setup_functions.R')

# specify dataset name and set file names appropriately
ds_name <- 'tcga'
file_expr <- paste0('../../data/processed/', ds_name, '.RData')
file_clinical <- paste0('../../data/clinical/', ds_name, '.RData')

data(XenaData)

# limit to desired cohort
blca <- XenaData %>% filter(XenaCohorts == 'GDC TCGA Bladder Cancer (BLCA)')

# Get the phenotype / clinical data
cli_query = blca %>%
  filter(Label == "Phenotype") %>%  # select clinical dataset
  XenaGenerate() %>%  # generate a XenaHub object
  XenaQuery() %>%     # generate the query
  XenaDownload()      # download the data

# prepare (load) the data into R
blca_pheno <- XenaPrepare(cli_query)

# Get the RNA-seq data, including the "probe map"
cli_query <- blca %>% filter(Label == 'HTSeq - Counts') %>%
  XenaGenerate() %>%  # generate a XenaHub object
  XenaQuery() %>%
  XenaDownload(download_probeMap = TRUE)

# prepare (load) the data into R
blca_counts <- XenaPrepare(cli_query)



# Get survival data
cli_query = blca %>%
  filter(Label == "survival data") %>%  # select clinical dataset
  XenaGenerate() %>%  # generate a XenaHub object
  XenaQuery() %>%     # generate the query
  XenaDownload()      # download the data

# prepare (load) the data into R
blca_survival <- XenaPrepare(cli_query)


########################################################
# use X (count data), Y (clinical) data, and probeMap
########################################################
X <- data.frame(blca_counts$TCGA.BLCA.htseq_counts.tsv.gz)
rownames(X) <- X$Ensembl_ID
X <- X[,-1]  # remove the probe name column

# probeMap = probe names
probeMap <- blca_counts$gencode.v22.annotation.gene.probeMap

# Y = pheno data
Y <- blca_pheno

# 'change '.' to '-' so sample ID format is consistent
colnames(X) <- gsub('\\.', '-', colnames(X))


##################################################
# Filter samples -- remove samples that are
#     FFPE
#     Papillary
#     T1 or Tx
##################################################

is_normal <- function(x) {
  gsub('....-..-....-', '', x) == '11A'
}

no_keep <- Y$diagnosis_subtype %in% 'Papillary' |
           Y$clinical_T %in% c('T1', 'TX') |
           Y$is_ffpe.samples

Y <- Y[!no_keep | is_normal(Y$submitter_id.samples) ,]

# keep only clinical data with expression
keep <- Y$submitter_id.samples%in% colnames(X)
Y <- Y[keep,]

# check for unwanted duplicate samples (01A and 01B)

# get sample type / vial
s <- strsplit(Y$submitter_id.samples, '-')
s <- sapply(s, `[`, 4)

# remove sample type / vial
samples <- gsub('-..[A-Z]$', '', Y$submitter_id.samples)

# split samples by type/vial
s <- split(samples, s)

stopifnot( length(intersect(s$`01A`, s$`01B`)) == 0) 
   

# We still need to match the expression data with the clinical data
# Let's do that by first finding the samples that are common
# between the expression and clinical data. We can use 
# intersect(a,b) to return a vector containing the elements common
# to vectors 'a' and 'b'

common_samples <- intersect(colnames(X), Y$submitter_id.samples)

# we then use match(x, t) to get a vector of indices. The value
# x[i] is the index of 't' containing the i^th value of 'x'

mx <- match(common_samples, colnames(X))
my <- match(common_samples, Y$submitter_id.samples)

X <- X[,mx]
Y <- Y[my,]

# Make sure that the samples match -- if they don't, this will produce an error
stopifnot(all(colnames(X) == Y$submitter_id.samples))





#######################################################
# Process the expression data
#######################################################

###################################################
# Data is log2(counts + 1), so we need to get
# back to the scale of counts
####################################################
X <- round(2**X - 1)

# first create a Digital Gene Expression (DGE) list object,
# which contains counts and library size information
dge <- DGEList(counts=X)

# remove genes with low counts, since these should not
# be considered in our downstream analysis. The default
# min.count = 10 will be used and we require this min
# count in at least min.prop = 10% of samples
keep <- filterByExpr(dge,min.prop = .10 )
dge <- dge[keep,,keep.lib.sizes=FALSE]

nrow(dge) # how many probes are left?

# apply TMM normalization, which computes the normalization 
# factors. The actual normalization is done in a later step
dge <- calcNormFactors(dge, method = "TMM")

# Calculate the log CPM values, using the normalization factors;
# 3 counts are added to each observation to prevent log 0 values
logCPM <- cpm(dge, log = TRUE, prior.count = 3)


#################################################################
## process clinical data
#################################################################

tumor <- rep(1, ncol(X))
tumor[grep('11A$', colnames(X))] <- 0

grade <- ifelse(Y$neoplasm_histologic_grade == "High Grade", 'hg', 'lg')

ms <- match(colnames(X), blca_survival$sample)
os_outcome <- blca_survival$OS[ms]
os_time <- blca_survival$OS.time[ms]


no <- Y$additional_pharmaceutical_therapy %in% "YES" |
      Y$additional_radiation_therapy %in% "YES" |
      Y$additional_surgery_locoregional_procedure %in% "YES"

os_time[no] <- os_outcome[no] <- NA

tcga_clinical <- create_clinical_table(id = colnames(X),
                                       tumor = tumor, grade = grade,
                                       os_time = os_time,
                                       os_outcome = os_outcome)

save(tcga_clinical, file = file_clinical)

if (PROCESS_EXPRESSION) {
  tcga_expr <- get_expression(logCPM, as.data.frame(probeMap), sep = FALSE)
  save(tcga_expr, file = file_expr)
}


