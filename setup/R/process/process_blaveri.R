# process Blaveri data 

library(impute)

cat('processing blaveri ...\n\n')

rm(list = ls())
source('functions/setup_functions.R')

# specify dataset name and set file names appropriately
ds_name <- 'blaveri'
file_expr <- paste0('../../data/processed/', ds_name, '.RData')
file_clinical <- paste0('../../data/clinical/', ds_name, '.RData')

# load expression and platform data
load("../../data/original/Blaveri.RData")
blaveri.expr <- Blaveri.expr

if (PROCESS_EXPRESSION) {
  NA.cutoff <- 0.20
  cat("imputing missing data with NA.cutoff of", NA.cutoff, "...\n")
  prop.na <-function (x) { return(sum(is.na(x)) / length(x)) }
  x.na <- apply(Blaveri.expr,1, prop.na)
  Blaveri.expr <- Blaveri.expr[x.na <= NA.cutoff,]
  
  tmp <- impute.knn(Blaveri.expr)
  blaveri.expr <- tmp$data
  rm(Blaveri.expr)

  # generate boxplot
  generate_boxplot(blaveri.expr, 'Blaveri')
}

# remove squamous samples
g <- grep('tcc', Blaveri.p$histology)
blaveri.expr <- blaveri.expr[,g]

if (PROCESS_EXPRESSION) {
  save(blaveri.expr, file = file_expr)
}

Blaveri.p <- Blaveri.p[g,]

# extract stage and grade
Blaveri.stage = rep(NA,length(Blaveri.p$stage))
Blaveri.stage[grep("T[a1]", Blaveri.p$stage)] = "nmi"
Blaveri.stage[grep("T[2-4]", Blaveri.p$stage)] = "mi"

Blaveri.grade = rep(NA,length(Blaveri.p$grade))
Blaveri.grade[grep("LG",Blaveri.p$grade)] = 'lg'
Blaveri.grade[grep("HG",Blaveri.p$grade)] = 'hg'

# create clinical data table
blaveri_clinical <- create_clinical_table(id = colnames(blaveri.expr), 
                               grade = Blaveri.grade,
                               stage = Blaveri.stage,
                               os_time = Blaveri.p$Survival..mos.,
                               os_outcome = as.integer(Blaveri.p$Survival_Status)-1)

save(blaveri_clinical, file = file_clinical)
