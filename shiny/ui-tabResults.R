############################################################
# tabResults UI -- results page for single gene analysis
############################################################

library(shinycssloaders)

# Summary tab
tabSummary <- tabPanel('Summary',
             br(),          
    fluidRow(column(6,
              withSpinner(plotOutput('plotSummary'))
            ),column(6,
                     
              tabsetPanel(id = 'tabSummaryTable',
                          type = 'pills',
                          tabPanel('Tumor'),
                          tabPanel('Grade'),
                          tabPanel('Stage'),
                          tabPanel('Survival'),
                          tabPanel('Survival_LG_NMI'),
                          tabPanel('Survival_HG_MI')
              ),
              withSpinner(dataTableOutput('tableSummary'))
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
                                uiOutput('graphOutputSurvival')),
            tabPanel('Survival (LG/NMI)', h4('Kaplan Meier curves comparing high and low expressors in patients with low grade, non-muscle invasive tumors', class = 'plotHeader'),
                     uiOutput('graphOutputSurvivalLGNMI')),
            tabPanel('Survival (HG/MI)', h4('Kaplan Meier curves comparing high and low expressors in patients with high grade, muscle-invasive tumors', class = 'plotHeader'),
                     uiOutput('graphOutputSurvivalHGMI'))
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

