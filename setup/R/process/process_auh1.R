# process auh1 data 

library(GEOquery)

cat('processing auh1...\n\n')

rm(list = ls())
source('functions/setup_functions.R')

# specify dataset name and set file names appropriately
ds_name <- 'auh1'
file_expr <- paste0('../../data/processed/', ds_name, '.RData')
file_clinical <- paste0('../../data/clinical/', ds_name, '.RData')

# load expression and platform data

auh1 <- getGEO('GSE3167')
auh1.expr <- exprs(auh1[[1]])

if (PROCESS_EXPRESSION) {

  load("../../data/platforms/GPL96.RData")

  #################################################

  #TODO - take log2 of auh1 and save in auh1

  # generate boxplot
  generate_boxplot(auh1.expr, 'auh1', FALSE)

  auh1.expr <- log2(auh1.expr)
  auh1.expr <- as.matrix(auh1.expr)

  generate_boxplot(auh1.expr, 'auh1', FALSE)

  #View(auh1.expr)

  # get gene-level expression values
  auh1.expr <- get_expression(auh1.expr, GPL96)
  #View(auh1.expr)

}

#################################################

### extract clinical data -- limit to 
GSE3167.p <- pData(auh1[[1]])

#View(GSE3167.p)


######################################################

keep <- (GSE3167.p$source_name_ch1 == "Normal bladder biopsy" | GSE3167.p$source_name_ch1 == "Bladder tumor tissue" | GSE3167.p$source_name_ch1=="Bladder timor tissue")

auh1.expr = auh1.expr[,keep]

if (PROCESS_EXPRESSION) {
  save(auh1.expr, file = file_expr)
}

GSE3167.p = GSE3167.p[keep,]

#View(GSE3167.p)

auh1.tumor = rep(NA,nrow(GSE3167.p))
auh1.tumor[GSE3167.p$source_name_ch1 == "Normal bladder biopsy"] <- "normal"
auh1.tumor[GSE3167.p$source_name_ch1 == "Bladder tumor tissue"] <- "tumor"
auh1.tumor[GSE3167.p$source_name_ch1 == "Bladder timor tissue"] <- "tumor"
#View(auh1.tumor)

#########################################################################

# get stages #

auh1.stage = rep(NA, nrow(GSE3167.p))

attribute <-GSE3167.p$characteristics_ch1

stageTa <- grepl("T[a1]",attribute)
stageT2 <- grepl("T[234]", attribute)
#stageNormal <- grepl("Normal", attribute)


auh1.stage[stageTa == TRUE] <- "nmi"
#auh1.stage[stageNormal == TRUE] <- "normal"
auh1.stage[stageT2 == TRUE] <- "mi"


##########################################################
# get grade

auh1.grade = rep(NA,nrow(GSE3167.p))

gradeLow <- grepl("gr[12]",attribute)
gradeHi<- grepl("gr[34]", attribute)                  

auh1.grade[gradeLow == TRUE] <- "lg"
auh1.grade[gradeHi == TRUE] <- "hg"

#View(auh1.stage)
#View(auh1.grade)

##################################################################

# create clinical data table
auh1_clinical <- create_clinical_table(id = colnames(auh1.expr), tumor = auh1.tumor, grade = auh1.grade, stage = auh1.stage)
#View(auh1_clinical)

save(auh1_clinical, file = file_clinical)
