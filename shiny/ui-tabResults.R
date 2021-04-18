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
tabVisualization <- tabPanel('Visualization',
    fluidRow( column(4,
        plotOutput('plotDataset'))
    )
)                        

# Results page
tabResults <- tabPanel("Results",
      uiOutput('ResultsHeader'), hr(),
      fluidRow(column(style='border-right: 1px solid',width = 12,
          tabsetPanel(id = "ResultsPage",
              tabSummary,
              tabVisualization, 
              tabPanel('Download')
          ))
      )
)

