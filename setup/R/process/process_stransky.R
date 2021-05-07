# process stransky1 and stransky2 data 

cat('processing stransky1 and stransky2 ...\n\n')

rm(list = ls())
source('functions/setup_functions.R')

# specify dataset name and set file names appropriately
ds1_name <- 'stransky1'
file1_expr <- paste0('../../data/processed/', ds1_name, '.RData')
file1_clinical <- paste0('../../data/clinical/', ds1_name, '.RData')

ds2_name <- 'stransky2'
file2_expr <- paste0('../../data/processed/', ds2_name, '.RData')
file2_clinical <- paste0('../../data/clinical/', ds2_name, '.RData')

# load expression and platform data
load("../../data/original/E.TABM.147.combine.reps.RData")
load("../../data/platforms/GPL91.RData")

# extract clinical information for stage
stage = rep(NA,length(E.TABM.147.p$Characteristics..DiseaseStaging.))
stage[grep("T[a1]",E.TABM.147.p$Characteristics..DiseaseStaging.)] <- 'nmi'
stage[grep("T[2-4]",E.TABM.147.p$Characteristics..DiseaseStaging.)] <- 'mi'

# extract clinical information for grade
tmp = E.TABM.147.p$Characteristics..TumorGrading.
grade = rep(NA, length(tmp))
grade[tmp == "G1" | tmp == "G1-G2"] <- "lg"
grade[tmp == "G2" | tmp == "G3"] <- "hg"

# extract clinical information for tumor
tumor = as.character(E.TABM.147.p$Characteristics..DiseaseState.)
tumor[tumor == "bladder carcinoma"] = "tumor"

# create clinical data table
stransky_clinical <- create_clinical_table(id = paste0("P", E.TABM.147.p$Source.Name), 
                                tumor = tumor, grade = grade, 
                                stage = stage)

if (PROCESS_EXPRESSION) {                    
  # Dataset contains two platforms
  #   samples 1 - 31 are on U95Av (GPL91)
  #   samples 32 - 62 are on U95Av2 (GPL8300)

  # However, both platforms are identical in this dataset, so
  # we will just use GPL91

  # get gene-level expression values
  stransky.expr <- get_expression(E.TABM.147.expr, GPL91)

}

# split into 2 datasets
i1 <- 1:31
i2 <- 32:62

if (PROCESS_EXPRESSION) {
  stransky1.expr <- stransky.expr[,i1]
  stransky2.expr <- stransky.expr[,i2]
  
  # generate boxplots
  generate_boxplot(stransky1.expr, 'stransky1')
  generate_boxplot(stransky2.expr, 'stransky2')

  save(stransky1.expr, file = file1_expr)
  save(stransky2.expr, file = file2_expr)
  
}

stransky1_clinical <- stransky_clinical[i1,]
stransky2_clinical <- stransky_clinical[i2,]
stransky2_clinical$tumor <- NULL # remove since all are tumors

save(stransky1_clinical, file = file1_clinical)
save(stransky2_clinical, file = file2_clinical)
