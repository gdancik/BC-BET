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
      fluidRow(column(style='border-right: 1px solid',width = 12,
          tabsetPanel(id = "multiResultsPage",
              tabPanel('Summary', br(),
                                       div('Click the download button to download an xlsx file of all results: ',
                                           downloadButton('downloadMultiResults')
                                       ), br(),
                                 
                       div(HTML("A summary of your results, consisting of the top 10 genes for each analysis, is shown below. By default results are grouped by analysis and sorted by magnitude of score",
                        "(<a id = 'score_expand' class = 'action-button'>expand/hide</a>)"),
                        ),       
                       div(id = 'score_div', class = 'hide', br(), HTML("A gene\'s <i>score</i> for an analysis is calculated as score_up - score_down, where",
                       "<ul><li>score_up = sum(upregulated AND P < 0.05)</li>",
                           "<li>score_down = sum(downregulated AND P < 0.05)</li></ul>",
                           'and where <ul>',
                       "<li>tumor: FC > 1 means that expression is higher in tumors compared to normal samples</li>",
                       "<li>grade: FC > 1 means that expression is higher in high grade (hg) tumors compared to low grade (lg) tumors </li>",
                       "<li>stage: FC > 1 means that expression is higher in muscle invasive (mi) tumors compared to non-muscle invasive (nmi) tumors </li>",
                       "<li>survival: HR > 1 means that high expression is associated with poor prognosis </li></ul>"))
                       , br(), hr(),
                      
                       fluidRow(column(1), column(4,
                          withSpinner(DT::dataTableOutput('tableMultiSummary'))
                       )), br()
              )
          ))
      )
)


