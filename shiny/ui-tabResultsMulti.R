#############################################################
# tabResultsMulti UI -- results page for multi gene analysis
#############################################################

library(shinycssloaders)
library(DT)

tabDownload <- tabPanel('Download',
                        br(),
                       # h4('Downloadable files (expression and clinical data)', style = 'color:darkred'),
                        uiOutput('downloadPage')
)

# Results page
tabResultsMulti <- tabPanel("Results", value = 'MultiResults',
      uiOutput('MultiResultsHeader'), hr(style = 'margin-top:10px; margin-bottom:10px'),
      downloadLink('downloadMultiResults'),
      fluidRow(column(style='border-right: 1px solid',width = 12,
          tabsetPanel(id = "multiResultsPage",
              tabPanel('Summary',
                       h2('Look at this!'))
          ))
      )
)

