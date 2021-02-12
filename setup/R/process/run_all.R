# run_all
# source all process_* files in the current directory

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



