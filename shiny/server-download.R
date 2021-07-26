
# gets list of datasets from REACIVE_SEARCH$results_de and 
#   REACTIVE_SEARCH$results_survival
get_datasets_from_results <- function() {
  
  gd <- function(x) {
    if (is.null(x) || nrow(x) == 0) {
      return(NULL)
    }
    unique(x$dataset)
  }
  

  ds1 <- lapply(REACTIVE_SEARCH$results_de, gd)
  ds2 <- lapply(REACTIVE_SEARCH$results_survival, gd)
  ds <- unique(c(do.call('c',ds1), do.call('c', ds2)))
  
  sort(ds)
  
}


output$downloadPage <- renderUI({
  
  showNotification('To do: need to get relevant datasets', 
                   type = "warning") 

  
  datasets <- get_datasets_from_results()
  
  cat('\n\ndownloadPage!!!\n\n')
  
  f <- function(x) {
    paste0('<li>', x,
           ' (', downloadLink(paste0('download_',x), 'csv'), ')',
           '</li>')
  }
  
  str <- paste0(paste0(sapply(datasets, f), collapse = '\n'), 
                '</ul>',collapse = '\n')
  
  HTML('<div style = "font-size:110%">', 
       
       '<h4 style = "color:darkred;"> Download results (differential expression and survival)</h4>',
       '<ul><li> Results (', paste0(downloadLink('download_results', 'xlsx')), ')</li></ul><hr>\n',
       
       '<h4 style = "color:darkred;"> Download expression and clinical data from all cohorts</h4>',
       '<ul><li> All files (', paste0(downloadLink('download_all', 'zip')), ')</li></ul>\n',
       #'</br>',
       '<h4 style = "color:darkred;"> Download data from individual cohorts </h4>',       
       '<ul>', str, '</ul></div>')

})


write_dataset <- function(x, file) {
  qry <- paste0('{"gene": "', REACTIVE_SEARCH$gene, '"}')
  df <- get_mongo_df(x, qry)
  write.csv(df, file, row.names = FALSE)
}

observe({

  datasets <- get_datasets_from_results()
  
  createDownloadHandler <- function(x) {
    
    output[[paste0('download_',x)]] <- downloadHandler(
        filename = function() {
            paste('bcbet_', x, ".csv", sep = "")
        },
        content = function(file) {
            cat('writing file...\n')
          
            # qry <- paste0('{"gene": "', REACTIVE_SEARCH$geneInput, '"}')
            # 
            # df <- get_mongo_df(x, qry)
            # write.csv(df, file, row.names = FALSE)
            # 
            write_dataset(x,file)
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
      
      datasets <- get_datasets_from_results()
      
      for (ds in datasets) {
        write_dataset(ds, paste0('bcbet_', ds, '.csv'))  
      }
      
      # write.csv(iris, file = 'iris.csv')
      # write.csv(iris, file = 'iris2.csv')
      
      setwd('../')
      zip(zipfile = file,files = bcbet_dir)
      setwd(wd)
    }
  )
})
  


write_sheet <- function(sheet, RES, filename) {
  x <- RES[[sheet]]
  x$category <- NULL
  
  # format columns
  columns <- bcbet_column_names()
  
  m <- match(colnames(x), columns)
  
  colnames(x)[!is.na(m)] <- names(columns)[m[!is.na(m)]]
  
  write.xlsx(x, sheetName = toupper(sheet), file = filename,
             row.names = FALSE, append = TRUE)
}

fc_description <- function(measure, html = FALSE, multi = FALSE) {
  
  if (multi) {
    h0 <- c('', 'First, define upregulated (FC > 1 or HR > 1) to mean the following for each phenotype:</br>')
  } else {
    h0 <- 'A gene is upregulated (FC > 1 or HR > 1) for a phenotype if the following is true:'
  }
  header <- c("- tumor: expression is higher in tumors compared to normal samples",
              "- grade: expression is higher in high grade (hg) tumors compared to low grade (lg) tumors",
              "- stage: expression is higher in muscle invasive (mi) tumors compared to non-muscle invasive (nmi) tumors",
              "- survival: high expression is associated with poor prognosis")
            
  if (html) {
    header <- paste0('<li>', gsub('^- ','',header), '</li>')
  }
  
  
  
  header <- c(h0, "<ul>", header, "", "</ul>A gene is downregulated if FC < 1 or HR < 1.")
  if (multi) {
    header <- c(header, "","A gene\'s <i>score</i> for an analysis is then calculated as</br></br>",
                "<span style = 'margin-left:15px'><i>score = number of datasets with significant (P < 0.05) upregulation - number of datasets with significant (P < .05) downregulation</i></span>")
  }
    
  if (measure != 'fc') {
    header <- gsub('FC > 1', 'AUC > 0.50', header)
  }
  
  if (!html) {
    header <- gsub('<span style = \'margin-left:15px\'>|</span>|</br>|<ul>|</ul>|<i>|</i>', '', header)
  } else {
    header <- paste0(header, collapse = '\n')
  }
  
  header
  
}

write_header_sheet <- function(filename, params, multi = FALSE) {
  
  p <- paste0(names(params), ': ', params)
  p <- paste0('- ', p, sep = '')
  p <- c('Statistical parameters:\n', p)
  
  header <- fc_description(params$measure, html = FALSE, multi)
  
  if (multi) {
    header <- c('Description and score calculation:\n',header)  
  } else {
    header <- c('Description:\n',header)
  }
  
  header <- c(p, '',header)
  
  header <- gsub('pvalue: pw', 'pvalue: pw (p-values calculated using Wilcoxon test)',  header)
  header <- gsub('pvalue: pt', 'pvalue: pt (p-values calculated using t-test)',  header)
  
  header <- gsub('treated: yes', 'treated: yes (include patients treated with BCG/adjuvant chemo in survival analysis)',  header)
  header <- gsub('treated: no', 'treated: no (remove patients treated with BCG/adjuvant chemo from survival analysis)',  header)
  
  
  write.xlsx(header, sheetName = 'Settings', file = filename,
             row.names = FALSE, append = TRUE, col.names = FALSE)
  
}

write_all_results <- function(filename) {
  write_header_sheet(filename, REACTIVE_SEARCH$parameters, length(REACTIVE_SEARCH$gene) > 1)
  res1 <- isolate(REACTIVE_SEARCH$results_de)
  res2 <- isolate(REACTIVE_SEARCH$results_survival)
  sapply(names(res1), write_sheet, RES = res1, filename = filename)
  sapply(names(res2), write_sheet, RES = res2, filename = filename)
} 



resultsDownloadHandler <- downloadHandler(
  filename = function() {
    
    if (length(REACTIVE_SEARCH$gene) > 1) {
      return('bcbet_multigene.xlsx')
    }
    paste('bcbet_', REACTIVE_SEARCH$gene, '.xlsx', sep='')
  },
  content = function(con) {
    shinyjs::runjs("$('#please-wait').removeClass('hide');")
    write_all_results(con)
    shinyjs::runjs("$('#please-wait').addClass('hide');")
    #write.csv(data, con)
  }
)

output$downloadAllResults <- resultsDownloadHandler # on download page
output$download_results <- resultsDownloadHandler # hidden link to trigger download on summary page

output$downloadMultiResults <- resultsDownloadHandler  # download multiple results

