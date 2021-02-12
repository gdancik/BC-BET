#!/usr/bin/Rscript --vanilla --slave
#sink("/dev/null/")  ## this doesn't work from cgi-script

# Sample call:
# Rscript /usr/lib/cgi-bin/R/WEB.patients.R HRAS HRAS-1415460073 no median BA fc w yes  
# m = 1

HIDE.OUTPUT = TRUE 

if (HIDE.OUTPUT) {
   sink("/var/www/html/BCBET2/img/__out.txt")
}

#ll = c("/home/dancikg/R/i686-pc-linux-gnu-library/3.0", 
#        "/usr/lib/R/library")
#.libPaths(ll)  

print("loading xlsx")

library(xlsx)
rJava::.jpackage('xlsxjars')
rJava::.jpackage('xlsx')

print("done")

source("/mnt/bioinformatics/R/functions/MicroArrayAnalysis/MicroArrayAnalysis.R")
aa = commandArgs()
m = match("--args",aa) 
commands = commandArgs()[-(1:m)]
GENE.LIST = commands[1]
XLSX.FILE = paste("/var/www/html/BCBET2/xlsx/", commands[2], ".xlsx", sep = "") 
probes = commands[3]
cut = commands[4]
ENDPOINT = commands[5]
M.METHOD = commands[6]
P.METHOD = commands[7]
KEEP.TREATED = commands[8]== "yes"
PRINT.P.CUTOFF = 1
BC.BET = TRUE

print("hello")

KM.MEDIAN = TRUE
if (cut != "median") { 
   KM.MEDIAN = FALSE 
}

if (!is.null(probes)) {
  if (probes == "yes") {
	COMBINE.PROBES = FALSE
	ADD.GENE.NAMES = TRUE
  }
}

cat("calling analyze...\n")
source("/mnt/bioinformatics/R/functions/analyze.DE.all.datasets.R")
cat("done...\n")

#######################################################
## count results
#######################################################

rbind.it <- function(x) {
  ans = NULL
  for (i in 1:length(x)) {
	if (!is.null(x[[i]])) ans = rbind(ans,x[[i]])
  }
  return(ans[!is.na(ans[,2]),])
}


count.res <- function(x, cut) {
  if (is.null(x)) return(c(0,0,0,0))
  if (is.null(nrow(x))) x= t(x)
  a = sum(x[,1] > cut & x[,2] <=  0.05)
  b = sum(x[,1] < cut & x[,2] <=  0.05)
  c = sum(x[,1] > cut & x[,2] >  0.05)
  d = sum(x[,1] < cut & x[,2] >  0.05)
  return (c(a,b,c,d)) 

}


TUMOR = lapply(ALL.ORIG, function(x) x$w.tumor)
GRADE = lapply(ALL.ORIG, function(x) x$w.grade)
STAGE = lapply(ALL.ORIG, function(x) x$w.stage)
DSS = lapply(ALL.ORIG, function(x) x$km.all)
DSS.HG.MI = lapply(ALL.ORIG, function(x) x$km.hg.mi)
DSS.LG.NMI = lapply(ALL.ORIG, function(x) x$km.lg.nmi)

TUMOR = rbind.it(TUMOR)
GRADE = rbind.it(GRADE)
STAGE = rbind.it(STAGE)
DSS = rbind.it(DSS)
DSS.HG.MI = rbind.it(DSS.HG.MI)
DSS.LG.NMI = rbind.it(DSS.LG.NMI)

td.class.TUMOR = TUMOR
td.class.GRADE = GRADE
td.class.STAGE = STAGE

td.class.DSS = DSS
td.class.DSS.HG.MI = DSS.HG.MI
td.class.DSS.LG.NMI = DSS.LG.NMI


cut = 0
if (M.METHOD != "fc") cut = 0.50
tumor.count = count.res(TUMOR, cut)
grade.count = count.res(GRADE, cut)
stage.count = count.res(STAGE, cut)

dss.count = count.res(DSS, 1)
dss.hg.mi.count = count.res(DSS.HG.MI, 1)
dss.lg.nmi.count = count.res(DSS.LG.NMI, 1)


#######################################################
## write results to js file
#######################################################

write.pie.data <-function(name, x, file) {
 str =  paste("var ", name, " = [\n",
          "{ label: \"DownSig\", data: ", x[2], ", color: \"blue\"},\n",
	  "{ label: \"DownNS\", data: ", x[4], ", color: \"#00FFFF\"},\n",
	  "{ label: \"UpNS\", data: ", x[3], ", color: \"pink\"},\n",
	  "{ label: \"UpSig\", data: ", x[1], ", color: \"red\"}\n",
	  "];", 
	  sep = "") 
  write(str, file = file, append = TRUE)
}

file = paste("/var/www/html/BCBET2/js/", commands[2], ".js", sep = "") 
write("", file = file)
write.pie.data("datatumor", tumor.count, file)
write.pie.data("datagrade", grade.count, file)
write.pie.data("datastage", stage.count, file)

write.pie.data("datadss", dss.count, file)
write.pie.data("datadsshgmi", dss.hg.mi.count, file)
write.pie.data("datadsslgnmi", dss.lg.nmi.count, file)


str.pieplot = c("pieplot(\"#placeholdertumor\", datatumor);",
                "pieplot(\"#placeholdergrade\", datagrade);",
                "pieplot(\"#placeholderstage\", datastage);",
                "pieplot(\"#placeholderdss1\", datadss);",
                "pieplot(\"#placeholderdss2\", datadsslgnmi);",
                "pieplot(\"#placeholderdss3\", datadsshgmi);")

write(str.pieplot, file = file, append = TRUE)

##############################################
# scripts to display tabular results
##############################################

rm.no.probes <- function(x) {
   keep = colSums(x == "") < nrow(x)
   x[,keep, drop = FALSE]
}

write.table.code <- function(x, func, td.class, survival) {
#  x = gsub("\\(0\\)", "\\(<0.001\\)", x)
  head = paste("function ", func, " {\n", sep = "")
#   if (is.null(x) | length(x) == 0) {
#	head = paste(head, ";}\n")
#        return(head)
#  }
#  head = "function showTumor() {\n"
  var.text = "var text = document.getElementById(\"tumorTable\");"
  if (survival) var.text = "var text = document.getElementById(\"survivalTable\");"
  t1 = "text.innerHTML = \"<table border = 1>" 
  t2 = paste("<td>", colnames(x), "</td>", sep = "", collapse = " ")
  t3 = paste("<td class = ", td.class, ">", x, "</td>", sep = "", collapse = " ")
  final = paste(head, var.text, "\n", t1,  
	"<tr>", t2, "</tr>", 
	"<tr>", t3, "</tr></table>\";\n}")
  return(final)
}



get.td.classes <-function(x, cut) {
   if (is.null(nrow(x))) {
	if (is.null(x)) return(NULL)
	x = t(x)
   }
   ans = rep(NA, nrow(x))
   ans[x[,1] > cut & x[,2] <= 0.05] = "\\\"upsig\\\"" 
   ans[x[,1] < cut & x[,2] <= 0.05] = "\\\"downsig\\\""
   ans[x[,1] > cut & x[,2] > 0.05] = "\\\"upnonsig\\\""
   ans[x[,1] < cut & x[,2] > 0.05] = "\\\"downnonsig\\\""
   return (ans)
}

TUMOR = lapply(ALL, function(x) x$w.tumor)
TUMOR = cbind.match(gene.names, TUMOR)
TUMOR = rm.no.probes(TUMOR)
td.class.TUMOR = get.td.classes(td.class.TUMOR, cut)
write(write.table.code(TUMOR, "showTumor()", td.class.TUMOR, FALSE), file = file, append = TRUE)

GRADE = lapply(ALL, function(x) x$w.grade)
GRADE = cbind.match(gene.names, GRADE)
GRADE = rm.no.probes(GRADE)
td.class.GRADE = get.td.classes(td.class.GRADE, cut)
write(write.table.code(GRADE, "showGrade()", td.class.GRADE, FALSE), file = file, append = TRUE)

STAGE = lapply(ALL, function(x) x$w.stage)
STAGE = cbind.match(gene.names, STAGE)
STAGE = rm.no.probes(STAGE)
td.class.STAGE = get.td.classes(td.class.STAGE, cut)
write(write.table.code(STAGE, "showStage()", td.class.STAGE, FALSE), file = file, append = TRUE)


cut = 1 ## for HR
DSS = lapply(ALL, function(x) x$km.all)
DSS = cbind.match(gene.names, DSS)
DSS = rm.no.probes(DSS)
td.class.DSS = get.td.classes(td.class.DSS, cut)
write(write.table.code(DSS, "showSurvivalAll()", td.class.DSS, TRUE), file = file, append = TRUE)


DSS.HG.MI = lapply(ALL, function(x) x$km.hg.mi)
DSS.HG.MI = cbind.match(gene.names, DSS.HG.MI)
DSS.HG.MI = rm.no.probes(DSS.HG.MI)
td.class.DSS.HG.MI = get.td.classes(td.class.DSS.HG.MI, cut)
write(write.table.code(DSS.HG.MI, "showSurvivalHGMI()", td.class.DSS.HG.MI, TRUE), file = file, append = TRUE)

DSS.LG.NMI = lapply(ALL, function(x) x$km.lg.nmi)
DSS.LG.NMI = cbind.match(gene.names, DSS.LG.NMI)
DSS.LG.NMI = rm.no.probes(DSS.LG.NMI)
td.class.DSS.LG.NMI = get.td.classes(td.class.DSS.LG.NMI, cut)
write(write.table.code(DSS.LG.NMI, "showSurvivalLGNMI()", td.class.DSS.LG.NMI, TRUE), file = file, append = TRUE)

if (HIDE.OUTPUT) {
  sink()
  system("rm /var/www/html/BCBET2/img/__out.txt")
}


