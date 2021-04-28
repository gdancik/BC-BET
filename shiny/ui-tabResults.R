############################################################
# tabResults UI -- results page for single gene analysis
############################################################

# Summary tab
tabSummary <- tabPanel('Summary',
                       
    fluidRow(column(6,
              plotOutput('plotSummary')
            ),column(6,
              tableOutput('tableSummary')
            )
    )
)

# template displaying one graph currently
tabPlots <- navbarMenu('Plots',
                       tabPanel('Tumor',uiOutput('graphOutputTumor')),
                       tabPanel('Grade', uiOutput('graphOutputGrade')),
                       tabPanel('Stage', uiOutput('graphOutputStage'))
)                        

# Results page
tabResults <- tabPanel("Results",
      uiOutput('ResultsHeader'), hr(style = 'margin-top:10px; margin-bottom:10px'),
      fluidRow(column(style='border-right: 1px solid',width = 12,
          tabsetPanel(id = "ResultsPage",
              tabSummary,
              tabPlots, 
              tabPanel('Download')
          ))
      )
)

