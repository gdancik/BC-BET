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

# Results page
tabResults <- tabPanel("Results",
      uiOutput('ResultsHeader'), hr(),
      fluidRow(column(style='border-right: 1px solid',width = 12,
          tabsetPanel(id = "ResultsPage",
              tabSummary,
              tabPanel('Graphs'),
              tabPanel('Download')
          ))
      )
)

