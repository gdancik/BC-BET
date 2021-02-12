

### creates a submatrix of X from rownames (probes) and col.keep ###
### all NAs in col.keep are converted to TRUE  
sub.matrix.by.probes <- function(X, probes, col.keep = 1:ncol(X)) {
  m = match(probes, rownames(X))
  m = m[!is.na(m)]
  if (length(m) == 0) return(NULL) 
  if (is.logical(col.keep)) col.keep[is.na(col.keep)] = TRUE 
  new.X = X[m,col.keep]
  if (length(m) == 1) new.X = as.matrix(t(new.X)) 
  rownames(new.X) = rownames(X)[m] 
  return(new.X)
}

#####################################################
## adds gene names to probe names of X, given
## the gene.probes matrix
#####################################################

set.gene.names <- function(X,gene.probes) {
  m = match(rownames(X), gene.probes[,2])
  mm = gene.probes[m,]
  if (is.null(nrow(mm))) mm = as.matrix(t(mm))
  n = paste(mm[,1], " (", mm[,2], ")", sep = "")
  rownames(X) = n
  return(X)
}


############################################################
## The Dataset Object
############################################################
create.dataset <- function(X = NULL,stage = NULL, grade = NULL, dss.time = NULL,	dss.outcome = NULL, progression = NULL, tumor = NULL) {
ans = list(X = X, stage = stage, grade = grade, dss.time = dss.time, dss.outcome = dss.outcome, progression = progression, tumor = tumor)
return(ans)
}

##################################################################
### evaluate dataset object, when 'X' is replaced with  
### a score
### Note: ROCR and survival libraries and appropriate functions
### should be loaded
##################################################################
evaluate.ds <- function(DS, what) {
  if (what == "dss") {
    ans = plot.km(DS$dss.time, DS$dss.outcome, DS$X, no.plot = TRUE)
    return(format.km(ans))
  }
  m = match(what, names(DS))
  if (is.na(m)) stop (what, " not found...\n")
  ans = plot.auc (DS$X, DS[[m]], alternative = "two.sided", no.plot = TRUE)
  return(format.auc(ans))
}

evaluate.ds.all <- function(DS.LIST) {
  ans.stage = lapply(DS.LIST, evaluate.ds, what = "stage")
  ans.grade = lapply(DS.LIST, evaluate.ds, what = "grade")
  ans.tumor = lapply(DS.LIST, evaluate.ds, what = "tumor")
  ans.progression = lapply(DS.LIST, evaluate.ds, what = "progression")
  ans.km = lapply(DS.LIST, evaluate.ds, what = "dss")
  ans = rbind(tumor = ans.tumor, grade = ans.grade, stage = ans.stage, progression = ans.progression, dss = ans.km)
  return(ans)
}

###############################################################
## keeps subsets of patients, such as MI and NMI tumors      ##
###############################################################
ds.subset <- function(DS, subset) {
   f <- function(x, index) {
 	if (is.null(nrow(x))) return(x[index])
        return(x[,index])
   }
   DS = lapply(DS, f, subset)
   return(DS)
}

ds.subset.mi <- function(DS) {
   subset = DS$stage == max(DS$stage, na.rm=T)
   subset[is.na(subset)] = FALSE   
   DS = ds.subset(DS, subset)
   return(DS)
}

ds.subset.nmi <- function(DS) {
   subset = DS$stage == min(DS$stage, na.rm=T)
   subset[is.na(subset)] = FALSE   
   DS = ds.subset(DS, subset)
   return(DS)
}

