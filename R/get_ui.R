#' @importFrom shiny fluidPage fluidRow column uiOutput plotOutput em h3 h4 h5
#' radioButtons
#' @importFrom DT dataTableOutput

get_ui <- function() {
  width_sub <- '450px'
  height_sub <- '350px'
  fluidPage(
    fluidRow(
      column(5, h3('model size selection', align = 'center'),
             uiOutput('statistic')),
      column(7, h3('edit the selected submodel', align = 'center'),
             dataTableOutput('vars'))
    ),
    column(
      width = 5, align = 'center',
      fluidRow(
        plotOutput('diff_heat', width = '650px', height = '800px',
                   click = 'size_click'),
        em('Select a suggested model by clicking a column corresponding to',
           'the desired model size.', style = 'color:grey')
      )),
    column(
      width = 7,
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
          plotOutput('statplot', width = width_sub, height = height_sub),
          plotOutput('diag', width = width_sub, height = height_sub),
          radioButtons('plot_type', 'Plot type', selected = 'hist', inline = T,
                       choices = c('Variable histograms' = 'hist',
                                   'Predictive density' = 'ppd'))))
    )
  )
}
