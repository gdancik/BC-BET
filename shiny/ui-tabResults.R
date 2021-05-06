############################################################
# tabResults UI -- results page for single gene analysis
############################################################

# Summary tab
tabSummary <- tabPanel('Summary',
                       
    fluidRow(column(6,
              plotOutput('plotSummary')
            ),column(6,
                     
              tabsetPanel(id = 'tabSummaryTable',
                          type = 'pills',
                          tabPanel('All'),
                          tabPanel('Tumor'),
                          tabPanel('Grade'),
                          tabPanel('Stage')
              ),
              tableOutput('tableSummary')
            )
    )
)

# template displaying one graph currently
tabPlots <- tabPanel('Plots',
        tabsetPanel(id = 'plotsPage', type = 'pills',
            tabPanel('Tumor', h4('Comparison of tumor and normal samples', class = 'plotHeader'),
                                uiOutput('graphOutputTumor')),
            tabPanel('Grade', h4('Comparison of high grade (HG) and low grade (LG) tumor samples', class = 'plotHeader'),
                                uiOutput('graphOutputGrade')),
            tabPanel('Stage', h4('Comparison of muscle invasive (MI) and non-muscle invasive (NMI) tumor samples', class = 'plotHeader'),
                                uiOutput('graphOutputStage')),
            tabPanel('Survival', h4('Kaplan Meier curves comparing high and low expressors', class = 'plotHeader'),
                                uiOutput('graphOutputSurvival'))
        )
)                        

tabDownload <- tabPanel('Download',
                        br(),
                       # h4('Downloadable files (expression and clinical data)', style = 'color:darkred'),
                        uiOutput('downloadPage')
)

# Results page
tabResults <- tabPanel("Results",
      uiOutput('ResultsHeader'), hr(style = 'margin-top:10px; margin-bottom:10px'),
      fluidRow(column(style='border-right: 1px solid',width = 12,
          tabsetPanel(id = "resultsPage",
              tabSummary,
              tabPlots, 
              tabDownload
          ))
      )
)

