# run_all
# source all process_* files in the current directory

args <- commandArgs(trailingOnly=TRUE)

EXPRESSION_ONLY <- TRUE
if (length(args) >= 1) {
    if (length(args) != 1 || args[1] != "--clinical-only") {
        cat('Usage: Rscript run_all.R [--clinical-only]\n')
        q()
    } else {
        PROCESS_EXPRESSION <- FALSE 
    }
}

UPDATE_PLATFORMS <- FALSE 
if (UPDATE_PLATFORMS) {
    system('Rscript ../get_platforms.R')  
}

files <- Sys.glob('process_*')

for (f in files) {
    cat('sourcing', basename(f), ' ...\n\n')
    myenv <- new.env()
    source(f, local = myenv)
}



