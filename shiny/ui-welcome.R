

myTextAreaInput <- function(...) {
  t <- textAreaInput(...)
  t$attribs$style <- 'display:none'
  t
}

userOptions <- list(

  fluidRow(style = 'margin-left:0px',
  h4('Statistical parameters',  
     span(style = 'font-size:80%', '(', 
          a('expand/hide', id = 'link_parameters', .noWS = 'outside'),
      ')')
  ),
  #accordion(id = 'accordionOptions',
  #          accordionItem(title = 'Statistical parameters', status = 'success', collapsed = FALSE,
    fluidRow(id = 'div_parameters',
      column(3, style = 'border-right: 1px solid black',
                    selectInput('measure', 
                        HTML(paste0('Class comparison measure (', 
                                    '<a id = "what-measure">?</a>)')),
                        c('FC' = 'fc', 'AUC' = 'auc'), 
                        selected = 'auc'),
             
                    selectInput('pvalue',  
                        HTML(paste0('P-value for differential expression')),
                        c('Two-sample t-test' = 'pt', 'Wilcoxon Rank-Sum Test' = 'pw'),
                        selected = 'pw')
             
            ),
      
      column(3,
                      selectInput('endpoint', 
                        HTML(paste0('Survival endpoint (', 
                            '<a id = "what-endpoint">?</a>)')),
                        c('Best Available (DSS, OS, RFS)' = 'ba', 'DSS' = 'dss', 'OS' = 'os', 'RFS' = 'rfs')),
      
                      selectInput('cutpoint',
                        HTML(paste0('Survival cutpoint')),
                        c('Median' = 'med', 'None (continuous expression)' = 'continuous'))
      ), column(5,
                      selectInput('treated',
                        HTML(paste0('Survival analysis includes treated patients (',
                              '<a id = "what-treated">?</a>)')),
                        c('Yes' = 'yes', 'No' = 'no')
                      )
            )
    ),
    # hr(class = "blue-button", style="height: 2px"),
    
    bsTooltip('what-measure', placement = 'right',
              'Select FC for fold change or AUC for area under the receiver operating characteristics curve'),

    bsTooltip('what-endpoint', placement = 'right',
              'DSS = disease-specific survival; OS = overall survival; RFS = recurrence-free survival; Best Available selects the first available endpoint in the list: DSS, OS, RFS'
              ),
    bsTooltip('what-treated', placement = 'right',
            "CNUH and DFCI include patients treated with BCG or adjuvant chemotherapy, though these treatments are not associated with good outcomes in these cohorts. Select Yes to include these patients, or No to remove them."
    )
  #))
  )
)

userSingleGene <- list(
  
  h4('Select a gene'),
  HTML("<p>Note: <i>BC-BET</i> uses official gene symbols. Check your genes or lookup invalid symbols from 
             <a href = 'https://www.genenames.org/tools/multi-symbol-checker/' target = '_blank'>genenames.org</a>.</p>"),

  HTML('<p>To enter multiple genes, scroll down or click <a href = "#multi-section">here</a>.'),
  
  fluidRow(
    column(3,
           selectizeInput("geneInput", label = "Select a gene (begin typing for more)", 
                          choices = sort(c("", "FPR1", "CIITA", "TP53", "HRAS", "CD24", "FGFR3")))
    ),
    column(2,style = "vertical-align:middle; padding-left:0px",
           HTML("<label class = 'control-label' style='visibility:hidden'>Hello</label>"),
           div(
             actionButton("btnGeneSearch", "Evaluate Single Gene", class = "blue-button",
                          style = "width:100%;")
           )
    )
  )
)

userGenes <- list(
  
  fluidRow(
    h4('Select multiple genes ', id = 'multi-section', style = 'padding-left:15px')
  ),
  
  fluidRow(
    column(3,
           textAreaInput('multiGeneInput', label = HTML('Enter up to 500 genes (<a id = "loadExampleLink" href = "#" class = "action-button">Example</a>)'),
                         value = "cd24\nfgfr3\nhras",
                         placeholder = 'Enter multiple genes, separated by spaces or one per line', 
                         rows = 10, resize = "none"),
           bsTooltip('loadExampleLink', placement = 'top',
                     'Load example gene list consisting of Bladder Cancer KEGG Pathway genes')
    ),
    column(4, style = "padding-left:15px",
           fileInput("multiGeneFile", "Or upload genes from file (500 max)",
                     accept = c(
                       "text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
           ),  tags$style(HTML("#multiGeneFile {display:none;}")),
           div(class = 'nohide',
               myTextAreaInput('invalidGeneOutput',
                               label = HTML("<span id = 'invalid_gene_label'>Invalid genes </span>"),
                               rows = 5, resize = "none")
           )
    ),
    column(4, style = "padding-right:0px",
           
    )
  ),
  
  fluidRow(column(3,
                  div(width = "100%",
                      #HTML("<label style = 'visibility:hidden'>hi</label>"),
                      actionButton("btnMultiGeneSearch", "Evaluate Multiple Genes", class = "blue-button",
                                   style = "width:100%;")#, disabled = 'disabled')
                  ), br(), br()
  ),
    column(4, div(width = "100%",
                  actionButton('btnRemoveInvalidGenes', 'Remove', 
                               class = 'red-button', style = "width:300px; display:none;"))
    )
  ),

  fluidRow(

    column(2, style = "vertical-align:middle; padding-left:0px", 
           
    ),
    
    actionButton('btnReprocessQuery', 'Reprocess', class = 'hide')
  )
)

welcomePage <- list(

      h2("Bladder Cancer Biomarker Evaluation Tool (BC-BET)"),
      hr(class = "blue-button", style="height: 2px"),
      
      userSingleGene,
      hr(class = "blue-button", style="height: 2px; margin-top: 5px"),
      userOptions,
      hr(class = "blue-button", style="height: 2px; margin-top: 5px"),      
      userGenes
)


