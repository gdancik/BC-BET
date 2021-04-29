

output$downloadPage <- renderUI({
  
  showNotification('To do: need to get relevant datasets', 
                   type = "warning") 
                   
  datasets <- c('mskcc', 'cnuh')
  
  cat('\n\ndownloadPage!!!\n\n')
  
  f <- function(x) {
    paste0('<li>', x,
           ' (', downloadLink(paste0('download_',x), 'csv'), ')',
           '</li>')
  }
  
  str <- paste0(paste0(sapply(datasets, f), collapse = '\n'), 
                '</ul>',collapse = '\n')
  
  HTML('<div style = "font-size:110%">', 
       '<h4 style = "color:darkred;"> Download expression and clinical data from all cohorts</h4>',
       '<ul><li> All files (', paste0(downloadLink('download_all', 'zip')), ')</li></ul>\n',
       #'</br>',
       '<h4 style = "color:darkred;"> Download data from individual cohorts </h4>',       
       '<ul>', str, '</ul></div>')

})

observe({

  datasets <- c('mskcc', 'cnuh')
  
  createDownloadHandler <- function(x) {
    
    output[[paste0('download_',x)]] <- downloadHandler(
        filename = function() {
            paste(x, ".csv", sep = "")
        },
        content = function(file) {
            cat('writing file...\n')
          
            qry <- paste0('{"gene": "', input$geneInput, '"}')
          
            df <- get_mongo_df(x, qry)
            write.csv(df, file, row.names = FALSE)
        }
    )
  }
  
  lapply(datasets, createDownloadHandler)
  

  all_file_name <- paste0('bcbet_', input$geneInput, '.zip')
  
  output$download_all <- downloadHandler(
    filename = all_file_name,
    content = function(file) {
      
      cat('writing zip...\n')
      
      bcbet_dir <- gsub('\\.zip,','', all_file_name)
      tmpDir <- paste0(tempdir(), '/', bcbet_dir)

      if (!dir.exists(tmpDir)) {
        dir.create(tmpDir)
      } else {
        file.remove(Sys.glob(paste0(tmpDir,'/*')))
      }

      wd <- setwd(tmpDir)
                    
      write.csv(iris, file = 'iris.csv')
      write.csv(iris, file = 'iris2.csv')
      
      setwd('../')
      zip(zipfile = file,files = bcbet_dir)
      setwd(wd)
    }
  )
})
  

write_datasets <- function(ds, gene) {
  
  # check this -- not used currently
  
  tmpDir <- tempdir()
  
  tmpdirfiles <- Sys.glob(paste0(tmpDir,'/*.csv'))
  file.remove(Sys.glob(tmpdirfiles))
  qry <- paste0('{"gene": "', gene, '"}')
  
  for (ds1 in ds) {
    filename = paste(ds1, ".csv", sep = "")
    df <- get_mongo_df(ds1, qry)
    write.csv(df, paste0(tmpDir,'/',filename), row.names = FALSE)
    
  }
  
  zip( paste0('bcbet_', gene, '.zip'), tmpdirfiles)
  
}


