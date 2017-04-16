get_ui <- function() {
  width_sub <- '400px'
  height_sub <- '350px'
  fluidPage(
    fluidRow(
      column(4, h3('model size selection', align = 'center'),
             uiOutput('statistic')),
      column(8, h3('edit the selected submodel', align = 'center'),
             dataTableOutput('vars'))
    ),
    column(
      width = 4,
      fluidRow(
        plotOutput('diff_full', width = '500px', height = '300px'),
        h5('Fraction of cv-folds that select the given variable',
           align = 'center'),
        div(dataTableOutput('heat')))),
    column(
      width = 8,
      column(
        width = 6, align = 'center', style = 'border-left: dotted #d3d3d3;',
        fluidRow(
          h4('correlated variables', align = 'center'),
          plotOutput('dendro', width = width_sub, height = height_sub),
          plotOutput('scatter', width = width_sub, height = height_sub))),
      column(
        width = 6, align = 'center', style = 'border-left: dotted #d3d3d3;',
        fluidRow(
          h4('submodel diagnostics', align = 'center'),
          plotOutput('ppc', width = width_sub, height = height_sub),
          plotOutput('statplot', width = width_sub, height = height_sub)))
    )
  )
}
