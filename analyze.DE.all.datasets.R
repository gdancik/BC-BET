##############################################################################
##### finds DE genes in Lindgren, MSKCC, CNUH, GSE1367, E-TABM-147(1-2)   ####
##### Blaveri for grade, stage, normal vs tumor, and DSS (or OS)          ####
#####
##### Currently omitted: Laval and Germany
##### Modified 5/3/2015 for BC.BET format
##############################################################################

source("/mnt/bioinformatics/R/functions/xlsx.R")

# KEEP.TREATED - FALSE to remove chemo/intravesicular treated prior to survival
#		 in CNUH and DFCI 
# RM.DUPS - removes dups from GSE5287 if GSE3167 is used (for stage,grade)
#         - removes dups from Lindgren if GSE32548 (Lindgren2) is used 
#		(for stage, grade, outcome)

IMPUTE = TRUE
COMBINE.REPS = TRUE ## for GSE5479

if (length(ls(pat = "RM.DUPS"))==0) RM.DUPS = TRUE 
if (length(ls(pat = "KEEP.TREATED"))==0) KEEP.TREATED = TRUE 
if (length(ls(pat = "SURGERY.TYPE"))==0) SURGERY.TYPE = "ALL"
if (length(ls(pat = "PRINT.P.CUTOFF"))==0) PRINT.P.CUTOFF = .05
if (length(ls(pat = "LOAD.PLATFORM"))==0) LOAD.PLATFORM = TRUE
if (length(ls(pat = "GENE.LIST"))==0) stop("MUST SPECIFY GENE LIST")
if (length(ls(pat = "COMBINE.PROBES"))==0) COMBINE.PROBES = TRUE
if (length(ls(pat = "XLSX.FILE"))==0) XLSX.FILE = "RESULTS.xlsx" 
if (length(ls(pat = "KM.MEDIAN"))==0) KM.MEDIAN = FALSE 
if (length(ls(pat = "ENDPOINT")) == 0) ENDPOINT = "BA"
if (length(ls(pat = "M.METHOD")) == 0) M.METHOD = "fc"
if (length(ls(pat = "P.METHOD")) == 0) P.METHOD = "w"
if (length(ls(pat = "ADD.SCORE")) == 0) ADD.SCORES = FALSE
if (length(ls(pat = "BC.BET")) == 0) BC.BET = FALSE

if (BC.BET & PRINT.P.CUTOFF != 1) {
  stop ("PRINT.P.CUTOFF MUST BE 1 for BC.BET")
  ## otherwise errors in to.web.compare function (parsing p-values) 
}

if (length(GENE.LIST) > 1 & BC.BET) {
   stop ("GENE.LIST MUST BE 1 with BC.BET")
}


### output parameters ###
cat("=========================================\n")
cat("INPUT PARAMETERS: \n")
cat("GENE.LIST: ", GENE.LIST, "\n")
cat("COMBINE.PROBES: ", COMBINE.PROBES, "\n")
cat("SURGERY.TYPE: ", SURGERY.TYPE, "\n")
cat("PRINT.P.CUTOFF: ", PRINT.P.CUTOFF, "\n")
cat("KEEP.TREATED: ", KEEP.TREATED, "\n")
cat("RM.DUPS: ", RM.DUPS, "\n")
cat("XLSX.FILE: ", XLSX.FILE, "\n")
cat("=========================================\n")
#system("sleep 1")

format.n <- function(n) {
   if (is.null(n)) return(n)
   paste("(", paste(n, collapse = "/"), ")", sep = "")
}

format.w.table <- function(w, p.cut, pow, digits.1 = 2, digits.2 = 3) {
  if (is.null(w)) return(w)
  if (all(is.na(w[,2]))) return (NULL)
  if (!is.null(pow)) w[,1] = pow**w[,1]
  keep = 1:nrow(w)
  keep = !is.na(w[,2]) & w[,2] <= p.cut
 
  lt = w[,2] < 0.001
  w2 = round(w[,2], digits.2)
  w2[lt] = "<0.001"

  #ans = paste(round(w[,1],digits.1), " (", round(w[,2],digits.2), ")", sep = "")
  ans = paste(round(w[,1],digits.1), " (", w2, ")", sep = "")
  ans[!keep] = "-"
  names(ans) = rownames(w)
  return(ans)
}

#count.values <- function(x,v) {
#  ans = rep(NA, length(v))
#  for (i in 1:length(v)) {
#	ans[i] = sum(x == v[i],na.rm=TRUE)
#  }
#  return(ans)
#}


analyze.genes <-function(X, tumor, grade, stage, time, outcome, endpoint, survival.type = "", survival.subset = rep(TRUE,length(time)), 
              survival.hg.mi.subset = rep(TRUE, length(time))) {

  w.tumor = NULL
  w.stage = NULL
  w.grade = NULL
  km.all = NULL
  km.hg.mi = NULL
  km.lg.nmi = NULL
  if (is.null(endpoint)) endpoint = ""
  n.tumor = n.stage = n.grade = 0

  if (!is.null(tumor)) n.tumor = table(tumor)
  if (!is.null(stage)) n.stage = table(stage)
  if (!is.null(grade)) n.grade = table(grade)
  
  n.dss = n.dss.hg.mi = n.dss.lg.nmi = 0 
  if (!is.null(outcome) & !is.null(time)) { 
   	n.dss = sum(!is.na(outcome) & !is.na(time) & survival.subset)
  	if (!is.null(stage) & !is.null(grade)) {
    	  subset.hg.mi = grade == max(grade,na.rm=T) & stage == max(stage,na.rm=T) & survival.subset & survival.hg.mi.subset

	  subset.lg.nmi = grade == min(grade,na.rm=T) & stage == min(stage,na.rm=T) & survival.subset 
    	  n.dss.hg.mi = sum(!is.na(outcome[subset.hg.mi]) & !is.na(time[subset.hg.mi]))
    	  n.dss.lg.nmi = sum(!is.na(outcome[subset.lg.nmi]) & !is.na(time[subset.lg.nmi]))
	}
  }

  if (!is.null(X)) {
    if (!is.null(tumor)) w.tumor = wilcox.quick(X, tumor, NULL, NULL, m = M.METHOD, p = P.METHOD)
    if (!is.null(stage)) w.stage = wilcox.quick(X, stage, NULL, NULL, m = M.METHOD, p = P.METHOD)
    if (!is.null(grade)) w.grade = wilcox.quick(X, grade, NULL, NULL, m = M.METHOD, p = P.METHOD)

    if (is.null(stage) || is.null(grade)) {
	if (!is.null(outcome) & survival.type == "hg.mi") {
	    km.hg.mi = coxph.by.gene(time, outcome, X, hr = TRUE, median.cut = KM.MEDIAN, subset = survival.subset)
	    n.dss.hg.mi = n.dss; n.dss = 0 
	} else if (!is.null(outcome)) {
		stop ("only hg.mi survival.type implemented in analyze.genes") 
	}
    } else if (!is.null(outcome) & survival.type!="hg.mi") {
	  km.all = coxph.by.gene(time, outcome, X, hr = TRUE, median.cut = KM.MEDIAN, subset = survival.subset)
	  t = table(outcome[subset.hg.mi])
    	  if (length(t) > 1 & min(t) > 1) {
        	km.hg.mi = coxph.by.gene(time, outcome, X, hr = TRUE, subset = subset.hg.mi, median.cut = KM.MEDIAN)
    	  }
	  t = table(outcome[subset.lg.nmi])
	  if (length(t) > 1 & min(t) > 1) {
	      km.lg.nmi = coxph.by.gene(time, outcome, X, hr = TRUE, subset = subset.lg.nmi, median.cut = KM.MEDIAN)
    	  }
  	}
  }
  ANS = NULL
  ANS$n.tumor = n.tumor
  ANS$n.stage = n.stage
  ANS$n.grade = n.grade
  ANS$n.dss = n.dss
  ANS$n.dss.hg.mi = n.dss.hg.mi
  ANS$n.dss.lg.nmi = n.dss.lg.nmi
  ANS$w.tumor = w.tumor
  ANS$w.grade = w.grade
  ANS$w.stage = w.stage
  ANS$km.all = km.all
  ANS$km.hg.mi = km.hg.mi
  ANS$km.lg.nmi = km.lg.nmi
  ANS$endpoint = endpoint 

  return(ANS)
}

#################################################
## Lindgren-2 (GSE32548) 
#################################################
loadLindgren2()
time = GSE32548.time; outcome = GSE32548.outcome
endpoint = "OS"
if (ENDPOINT %in% c("DSS", "RFS")) {
   time = NULL; outcome = NULL; endpoint = NULL
}
LINDGREN2 = analyze.genes(GSE32548.expr, NULL, GSE32548.grade, GSE32548.stage, time, outcome, endpoint, survival.hg.mi.subset = GSE32548.cystectomy%in% 1)


#################################################
## Lindgren
#################################################
loadLindgren()

if (RM.DUPS & !is.null(nrow(GSE32548.expr)) & !is.null(Lindgren.expr)) {
	l2.names = as.character(GSE32548.p$source_name_ch1)
	l2.names = gsub("Bladder tumor ", "", l2.names)
	common = intersect(colnames(Lindgren.expr), l2.names)
        m = match(colnames(Lindgren.expr), common)
	Lindgren.stage[m] = NA
	Lindgren.grade[m] = NA
	Lindgren.time[m] = NA
	Lindgren.outcome[m] = NA
}



time = Lindgren.time; outcome = Lindgren.outcome
endpoint = "OS"
if (ENDPOINT %in% c("DSS", "RFS")) {
   time = NULL; outcome = NULL; endpoint = NULL
}

LINDGREN = analyze.genes(Lindgren.expr, Lindgren.tumor, Lindgren.grade, Lindgren.stage, time, outcome, endpoint, survival.hg.mi.subset = GSE19915.samples[,8] %in% "YES")

#################################################
## MSKCC (SC) 
#################################################

loadSC()
time = SC.OS.time; outcome = SC.OS.outcome; endpoint = "DSS"
if (ENDPOINT %in% c("RFS", "OS")) {
  time = NULL; outcome = NULL
  endpoint = ENDPOINT
}
SC = analyze.genes(SC.expr, SC.tumor, SC.grade, SC.stage, time, outcome, endpoint)


loadGSE31684()
time = GSE31684.RFS.time; outcome = GSE31684.RFS.outcome; endpoint = "RFS"
if (!KEEP.TREATED) {
   treated = GSE31684.chemo.postrc %in% 1 
   time[treated] = NA
   outcome[treated] = NA
}

if (ENDPOINT %in% c("OS", "DSS")) {
  time = NULL; outcome = NULL
  endpoint = ENDPOINT
}
DFCI = analyze.genes(GSE31684.expr, NULL, GSE31684.grade, GSE31684.stage, time, outcome, endpoint, survival.subset = GSE31684.chemo.prerc%in%0)

#################################################
## E-TABM-147 
#################################################
loadETABM()
E1 = analyze.genes(E.TABM.147.1.expr, E.TABM.147.1.tumor, E.TABM.147.1.grade,
	           E.TABM.147.1.stage, NULL, NULL, NULL)

## all E2 profiles from tumors
E2 = analyze.genes(E.TABM.147.2.expr, NULL, E.TABM.147.2.grade,
	           E.TABM.147.2.stage, NULL, NULL, NULL)

#################################################
## GSE3167 
#################################################
loadGSE3167()
GSE3167 = analyze.genes(GSE3167.expr, GSE3167.tumor, GSE3167.grade, GSE3167.stage, NULL, NULL, NULL)


#################################################
## GSE5479 
#################################################
loadGSE5479()

if (RM.DUPS & !is.null(nrow(GSE3167.expr)) & !is.null(GSE5479.expr)) {

	l1.names = as.character(GSE3167.p$title)
	l1.names = gsub("Normal bladder ", "", l1.names)
	l2.names = GSE5479.CLINICAL$sample.name
	l2.names = sapply(strsplit(l2.names, " "), function(x)x[[1]])

	common = intersect(l1.names, l2.names)
        m = match(l2.names, common)
	GSE5479.stage[m] = NA
	GSE5479.grade[m] = NA
}
GSE5479 = analyze.genes(GSE5479.expr, NULL, GSE5479.grade, GSE5479.stage, NULL, NULL, NULL)


#################################################
## Korea (CNUH) 
#################################################
loadKorea()
time = GSE13507.DSS.time; outcome = GSE13507.DSS.outcome; endpoint = "DSS"
if (!KEEP.TREATED) {
   treated = GSE13507.chemo%in%1 | GSE13507.intravesical %in% 1 
   time[treated] = NA
   outcome[treated] = NA
}

if (ENDPOINT %in% "OS") {
  time = GSE13507.OS.time
  outcome = GSE13507.OS.outcome
  endpoint = ENDPOINT
} else if (ENDPOINT %in% "RFS") {
  time = NULL; outcome = NULL; endpoint = NULL
}

CNUH = analyze.genes(GSE13507.expr, Korea.tumor, Korea.grade, Korea.stage,
                     time, outcome, endpoint)
#################################################
## Blaveri  
#################################################
loadBlaveri()
time = Blaveri.time; outcome = Blaveri.outcome; endpoint = "OS"
if (ENDPOINT %in% c("RFS", "DSS")) {
  time = NULL; outcome = NULL; endpoint = ENDPOINT
}

subset = (Blaveri.stage%in%1 & Blaveri.p$surgery.Type %in% "TURBT") |
         (Blaveri.stage%in%2 & Blaveri.p$surgery.Type %in% "Cystectomy")

BLAVERI = analyze.genes(Blaveri.expr, NULL, Blaveri.grade, Blaveri.stage, time, outcome, endpoint, survival.subset = subset)

#################################################
## GSE37317 (UVA) 
#################################################
loadGSE37317()
UVA = analyze.genes(GSE37317.expr, NULL, NULL, GSE37317.stage, NULL, NULL, NULL)

#################################################
## GSE48277 (MDA-1) 
#################################################
loadGSE48277()
time = GSE48277.time; outcome = GSE48277.outcome; endpoint = "OS"
if (ENDPOINT %in% c("RFS", "DSS")) {
  time = NULL; outcome = NULL; endpoint = NULL
}

MDA.1 = analyze.genes(GSE48277.expr, NULL, NULL, NULL, time, outcome, endpoint, survival.type = "hg.mi")


#################################################
## GSE48075 (MDA-2) 
#################################################
loadGSE48075()
time = GSE48075.time; outcome = GSE48075.outcome; endpoint = "OS"
if (ENDPOINT %in% c("RFS", "DSS")) {
  time = NULL; outcome = NULL; endpoint = NULL
}

MDA.2 = analyze.genes(GSE48075.expr, NULL, NULL, GSE48075.stage, time, outcome, endpoint, survival.type = "hg.mi")


### formatting results and output ###

format.all.n <-function(X) {
   X$n.tumor = format.n(X$n.tumor)
   X$n.grade = format.n(X$n.grade)
   X$n.stage = format.n(X$n.stage)
   X$n.dss = format.n(X$n.dss)
   X$n.dss.hg.mi = format.n(X$n.dss.hg.mi)
   X$n.dss.lg.nmi = format.n(X$n.dss.lg.nmi)
   return(X)
}


if (M.METHOD == "fc") {
  pow = 2
} else if (M.METHOD == "auc") {
  pow = NULL
} else {
  stop ("Invalid M.METHOD; must be fc or auc")
}

format.all.w.table <-function (X, p.cut, pow) {
   X$w.tumor = format.w.table(X$w.tumor, p.cut,pow)
   X$w.grade = format.w.table(X$w.grade, p.cut,pow)
   X$w.stage = format.w.table(X$w.stage, p.cut,pow)
   X$km.all = format.w.table(X$km.all, p.cut, NULL)
   X$km.hg.mi = format.w.table(X$km.hg.mi, p.cut, NULL)
   X$km.lg.nmi = format.w.table(X$km.lg.nmi, p.cut, NULL)
   return(X)
}


format.all <-function(X,p.cut, pow) {
     X = format.all.n(X)
     X = format.all.w.table(X, p.cut, pow)
     return(X)
}


cbind.match <- function(rownames, l) {
  if (is.null(rownames)) rownames = sort(unique(unlist(lapply(l, names))))
  ans = NULL
  for (i in 1:length(l)) {
    tmp = rep("", length(rownames))
    m = match(names(l[[i]]), rownames)
    tmp[m] = l[[i]]
    ans = cbind(ans,tmp)
  }
  rownames(ans) = rownames
  colnames(ans) = names(l) 
  
#  keep = lapply(l, is.null)
#  keep = !unlist(keep)
#  ans = ans[,keep]
  return(ans)
}

ALL = list("AUH-1" = GSE3167, "AUH-2" = GSE5479, BLAVERI = BLAVERI, CNUH = CNUH,  DFCI = DFCI, LINDGREN = LINDGREN, "LINDGREN-2" = LINDGREN2, "MDA-1" = MDA.1, "MDA-2" = MDA.2, MSKCC = SC, "Stransky-1" = E1, "Stransky-2" = E2, UVA = UVA )


ALL.ORIG = ALL
ALL = lapply(ALL, format.all, p.cut = PRINT.P.CUTOFF, pow = pow)

gene.names = GENE.LIST
if (ADD.GENE.NAMES) {
  gene.names = lapply(ALL, function(x)  
	unique(names(c(x$tumor, x$w.grade, x$w.stage, x$km.all, x$km.hg.mi, x$km.lg.nmi)))) 
  gene.names = unique(unlist(gene.names))
}

TUMOR = lapply(ALL, function(x) x$w.tumor)
TUMOR = cbind.match(gene.names, TUMOR)

GRADE = lapply(ALL, function(x) x$w.grade)
GRADE = cbind.match(gene.names, GRADE)

STAGE = lapply(ALL, function(x) x$w.stage)
STAGE = cbind.match(gene.names, STAGE)

add.endpoint <- function(X, E.list) {
  endpoints = sapply(E.list, function(x)x$endpoint);
  n = paste(colnames(X), endpoints, sep = "-")
  colnames(X) = n
  X
}

DSS = lapply(ALL, function(x) x$km.all)
DSS = cbind.match(gene.names, DSS)

DSS.HG.MI = lapply(ALL, function(x) x$km.hg.mi)
DSS.HG.MI = cbind.match(gene.names, DSS.HG.MI)

DSS.LG.NMI = lapply(ALL, function(x) x$km.lg.nmi)
DSS.LG.NMI = cbind.match(gene.names, DSS.LG.NMI)

TUMOR = rbind(unlist(lapply(ALL, function(x) x$n.tumor)), TUMOR)
GRADE = rbind(unlist(lapply(ALL, function(x) x$n.grade)), GRADE)
STAGE = rbind(unlist(lapply(ALL, function(x) x$n.stage)), STAGE)
DSS = rbind(unlist(lapply(ALL, function(x) x$n.dss)), DSS)
DSS.HG.MI = rbind(unlist(lapply(ALL, function(x) x$n.dss.hg.mi)), DSS.HG.MI)
DSS.LG.NMI = rbind(unlist(lapply(ALL, function(x) x$n.dss.lg.nmi)), DSS.LG.NMI)

DSS = add.endpoint(DSS, ALL)
DSS.HG.MI = add.endpoint(DSS.HG.MI, ALL)
DSS.LG.NMI = add.endpoint(DSS.LG.NMI, ALL)


rm.blanks <-function(x) {
  if (length(x) == 0) return(x)
  if (nrow(x) == 2) {
	no.keep = x[-1,] == ""
  } else {
    no.keep = apply(x[-1,],2,function(x) all(x == ""))
  }
  x[,!no.keep, drop = FALSE]
}

TUMOR = rm.blanks(TUMOR)
GRADE = rm.blanks(GRADE)
STAGE = rm.blanks(STAGE)
DSS = rm.blanks(DSS)
DSS.HG.MI = rm.blanks(DSS.HG.MI)
DSS.LG.NMI = rm.blanks(DSS.LG.NMI)

if (ADD.SCORES) {

  to.double <- function(x) {
    if (length(x) <= 1) return(NA)
    as.double(x[1])
  }

  count.x <- function(x) {
    num = sapply(strsplit(x, "\\("), to.double)
    score = sum(sign(num-1), na.rm=TRUE)
    n = sum(x!="")
    cbind(score = score, prop = score/n, n = n)
  }

  score.tumor = apply(TUMOR[-1,], 1, count.x)
  score.grade = apply(GRADE[-1,], 1, count.x)
  score.stage = apply(STAGE[-1,], 1, count.x)
  score.dss = apply(DSS[-1,], 1, count.x)
  score.dss.lg.nmi = apply(DSS.LG.NMI[-1,], 1, count.x)
  score.dss.hg.mi = apply(DSS.HG.MI[-1,], 1, count.x)

  add.scores <- function(x,score, rm.none = TRUE) {
     score = cbind(NA, score)
     score = t(score)
     colnames(score) = c("score.count", "score.prop", "score.n")
     x = cbind(score, x)

     i = 1:3
     colnames(x)[-i] = paste(colnames(x)[-i], as.character(unlist(x[1,-i])))
     x = x[-1,]
     if (rm.none) {
	keep = as.double(x[,3]) != 0
	keep[is.na(keep)] = TRUE
        x = x[keep,]
     }
     o = order(abs(as.double(x[,1])), abs(as.double(x[,2])), decreasing = TRUE)

     return(x[o,])
  }

  TUMOR = add.scores(TUMOR, score.tumor)
  GRADE = add.scores(GRADE, score.grade)
  STAGE = add.scores(STAGE, score.stage)
  DSS = add.scores(DSS, score.dss)
  DSS.LG.NMI = add.scores(DSS.LG.NMI, score.dss.lg.nmi)
  DSS.HG.MI = add.scores(DSS.HG.MI, score.dss.hg.mi)

}

if (BC.BET) {

  to.web.compare <- function(X, header, colnames = NULL) {
    if (is.null(colnames)) {
	colnames = c("N", "HR", "P-value")
    }
    X[1,] = gsub("\\(","",X[1,])
    X[1,] = gsub("\\)","",X[1,])
    N = strsplit(X[1,], "/")
    N = strsplit(X[1,], "/")
    N1 = sapply(N, function(x)x[1])
    N2 = sapply(N, function(x)x[2])
    X[2,] = gsub("\\(","",X[2,])
    X[2,] = gsub("\\)","",X[2,])
    RES = strsplit(X[2,], " ")
    FC = sapply(RES, function(x)x[1])
    P = sapply(RES, function(x)x[2])

    if (length(N1) == 1)  names(N1) = colnames(X)

    if (length(colnames) == 4) {
       ANS = cbind(N1,N2,FC,P)
    } else {
       ANS = cbind(N1,FC,P)
    }

    HEADER = cbind("", "", "")
    if (length(colnames) == 4) HEADER = cbind(HEADER, "")
    HEADER = rbind(HEADER, HEADER)
    if (length(header) == 3) HEADER = rbind(HEADER, "")

    rownames(HEADER) = header

    ANS = rbind(HEADER, "", " " = colnames, ANS)
    colnames(ANS) = NULL 
    return(ANS)
  }


  h1 = "# FC > 1 means that expression is higher in"
  C3 = "FC"
  if (M.METHOD!="fc") {
    C3 = "AUC"
    h1 = "# AUC > 0.5 means that expression is higher in"
  }

  tumor.header = paste(h1, "tumors compared to normal samples")
  tumor.header = c(tumor.header, "# N.normal = number of normal samples","# N.tumor = number of tumor samples")
  tumor.cols = c("N.normal", "N.tumor", C3, "P-value")
  TUMOR.WEB = to.web.compare(TUMOR, tumor.header, tumor.cols)

  grade.header = paste(h1, "HG compared to LG tumors")
  grade.header = c(grade.header, "# N.LG = number of low-grade samples","# N.HG = number of high-grade samples")
  grade.cols = c("N.LG", "N.HG", C3, "P-value")
  GRADE.WEB = to.web.compare(GRADE, grade.header, grade.cols)

  stage.header = paste(h1, "MI compared to NMI tumors")
  stage.header = c(stage.header, "# N.NMI = number of non-muscle invasive samples","# N.MI = number of muscle invasive samples")
  stage.cols = c("N.NMI", "N.MI", C3, "P-value")
  STAGE.WEB = to.web.compare(STAGE, stage.header, stage.cols)

  dss.header = "# HR > 1 means that high expression is associated with poor prognosis"
  dss.header = c(dss.header, "# N = number of samples")

  DSS.WEB = to.web.compare(DSS, dss.header)

  DSS.LG.NMI.WEB = "not enough samples for analysis"
  if (!is.null(DSS.LG.NMI) & ncol(DSS.LG.NMI) > 0) {
    DSS.LG.NMI.WEB = to.web.compare(DSS.LG.NMI, dss.header)
  }
  DSS.HG.MI.WEB = "not enough samples for analysis"
  if (!is.null(DSS.HG.MI) & ncol(DSS.HG.MI) > 0) {
     DSS.HG.MI.WEB = to.web.compare(DSS.HG.MI, dss.header)
  }

  write.xlsx.sheets(list(TUMOR = TUMOR.WEB, GRADE = GRADE.WEB, STAGE = STAGE.WEB, SURVIVAL = DSS.WEB,
 SURVIVAL.LG.NMI = DSS.LG.NMI.WEB, SURVIVAL.HG.MI = DSS.HG.MI.WEB), file = XLSX.FILE)

} else {


line1 = "The TUMOR, GRADE, AND STAGE tables are in the form FC (p-value), where FC > 1 means expression is higher in tumor (compared to normal), higher in high grade (compared to low grade), and higher in T2-T4 (compared to Ta-T1)." 

line1B = "The SURVIVAL tables are in the form HR (p-value), where HR > 1 means that high expression is associated with poor outcomes."

if (M.METHOD == "auc") {
  line1 = "The tables are in the form AUC (p-value), where AUC > 0.50 means expression is higher in tumor (compared to normal), higher in high grade (compared to low grade), and higher in T2-T4 (compared to Ta-T1)." 
} 

line3 = "A '-' indicates the gene is not significant (at P < 0.05 by default) while a blank means the gene was not profiled in that dataset."

if (PRINT.P.CUTOFF >=1) {
  line3 = "A blank means that the gene was not profiled in that dataset."
} 

desc =  c(line1, line1B, line3, "", 
 
"The numbers below each dataset in the TUMOR, STAGE, and GRADE sheets indicate the number of samples with each phenotype in the form (normal, tumor), (lg, hg), or (nmi, mi). ", "For example, in the TUMOR sheet the (38/91) under MSKCC indicates 38 normal samples and 91 tumors." , "In the SURVIVAL sheets, the numbers below each dataset corresponds to the number of patients analyzed.")

write.xlsx.sheets(list(DESCRIPTION = matrix(desc), TUMOR = TUMOR, GRADE = GRADE, STAGE = STAGE, SURVIVAL = DSS,
 SURVIVAL.LG.NMI = DSS.LG.NMI, SURVIVAL.HG.MI = DSS.HG.MI), file = XLSX.FILE)
}

cat("results written to files....\n")
cat("\txlsx.file: ", XLSX.FILE, "\n")
cat("----------------------------------------------------------------------------------\n")


#########################################################

count.dss <- function(L) {
  to.int <-function(x) {
     x  = gsub("\\(","", x)
     x = gsub("\\)","", x)
     as.integer(x)
  }
  f <-function(x) {
	max(to.int(x$n.dss), to.int(x$n.dss.lg.nmi), to.int(x$n.dss.hg.mi))
  }
  sum(unlist(lapply(L, f)) )

}

