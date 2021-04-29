# graphPanelFunctions

# creates a row for 'n' plots
createPlotRow <- function(nums, id_prefix) {
  f<- function(x) {
    column(3, shinycssloaders::withSpinner(
      plotOutput(paste0(id_prefix,x), height = '300px'))
    )
  }
  fluidRow(lapply(nums, f))
}

createPlotGrid <- function(n, id_prefix) {
  s <- split(1:n, ceiling(1:n/4))
  lapply(s, createPlotRow, id_prefix)
}