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


createTablePanel <- function(type) {
  tabPanel(type,
    DT::dataTableOutput(paste0('tableMultiSummary',type))
  )
}

# Results page
tabResultsMulti <- tabPanel("Results", value = 'MultiResults',
      uiOutput('MultiResultsHeader'), hr(style = 'margin-top:10px; margin-bottom:10px'),
      fluidRow(column(style='border-right: 1px solid',width = 12,
                      div('Click the download button to download an xlsx file of all results: ',
                          downloadButton('downloadMultiResults')
                      ), 
          tabsetPanel(id = "multiResultsPage",
              tabPanel('Summary Heatmap',
                       p(HTML("</br>A <i>score</i> is calculated for each gene and each analysis, and the magnitude of the score is roughly equal to the number of cohorts with statistically significant (P < 0.05) results.",
                                "A high score indicates that high expression (upregulation) of the gene is significantly (P < 0.05) associated with tumor, high grade, muscle invasive, or poor outcomes,",
                                "while low scores indicate the opposite relationship. (<a id = 'score_expand' class = 'action-button'>more info</a>)"),
                           bsModal('modal_score_info', 'Score calculation', 'score_expand', size = 'large',
                                   uiOutput('score_description')
                           )
                       ),
                       
                       fluidRow(
                         column(1), column(10,
                          plotOutput('heatmap')
                         )
                       ),
                       fluidRow(column(1), column(10,
                        p("The heatmap summarizes each gene/analysis by its score, with colors denoting the scaled score between -1 and 1, and is equal to score / number of datasets.",
                          "If more than 100 genes were entered, only the top 100 genes having the highest maximum scores are shown.")
                       )), br(), br()
               ), tabPanel('Tables',
                          fluidRow(column(1), column(10,
                            p(HTML("</br><b>Note</b>: scores are sorted from highest to lowest by default, so consistently down-regulated genes appear at the bottom of the list"))
                          )),
                         fluidRow(column(1), column(10, 
                                tabsetPanel(id = 'tabSummaryMultiTable',
                                              type = 'pills',
                                              tabPanel('Tumor'),
                                              tabPanel('Grade'),
                                              tabPanel('Stage'),
                                              tabPanel('Survival'),
                                              tabPanel('Survival_LG_NMI'),
                                              tabPanel('Survival_HG_MI')
                                            ),
                                  DT::dataTableOutput('tableMultiSummary'), br()
                                )
                          )# end fluid Row
                       ) # end tabPanel
              )
          )
))


