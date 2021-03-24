welcomePage <- list(

      h2("Bladder Cancer Biomarker Evaluation Tool (BC-BET)"),
      p(strong("Instructions:"), "Welcome to the..."),
        
      hr(class = "blue-button", style="height: 2px"),
      HTML("<p>Note: <i>BC-BET</i> uses official gene symbols. Check your genes or lookup invalid symbols from 
             <a href = 'https://www.genenames.org/tools/multi-symbol-checker/' target = '_blank'>genenames.org</a>.</p>"),
      
      fluidRow(
        column(3, style="padding-right:0px",
          selectizeInput("geneInput", label = "Select a gene (begin typing for more)", 
                          choices = sort(c("", "TP53", "HRAS", "CD24", "FGFR3")))
        ),
        column(2,style = "vertical-align:middle; padding-left:0px",
           HTML("<label class = 'control-label' style='visibility:hidden'>Hello</label>"),
           div(
                actionButton("btnGeneSearch", "Evaluate Single Gene", class = "blue-button",
                             style = "width:100%;")
           )
        )
      ), 
      fluidRow(
        h4('- OR- ', style = 'padding-left:15px')
      ),
      fluidRow(
        column(3, style = "padding-right:0px",
               textAreaInput('multiGeneInput', label = 'Select multiple genes (500 max)',
                              value = "",
                              placeholder = 'Enter multiple genes, separated by spaces or one per line', 
                                rows = 6, resize = "none")
        ),
        column(4, style = "padding-right:0px",
               div(class = 'hide',
               textAreaInput('invalidGeneOutput', label = HTML("<span style = 'color:red'>Invalid Genes</span>"), 
                                                               rows = 6)
               )
        )
      ),
      
      fluidRow(
        h4('- OR- ', style = 'padding-left:15px')
      ),
      
      fluidRow(
        column(3, style = "padding-left:15px",
          fileInput("multiGeneFile", "Upload genes from file (500 max)",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
          )
        ),
        column(2, style = "vertical-align:middle; padding-left:0px", 
               div(width = "100%",
                   HTML("<label style = 'visibility:hidden'>hi</label>"),
                   actionButton("btnMultiGeneSearch", "Evaluate Multiple Genes", class = "blue-button",
                                style = "width:100%;", disabled = 'disabled')
               )
        ),
        
        actionButton('btnReprocessQuery', 'Reprocess', class = 'hide')
  )
)


