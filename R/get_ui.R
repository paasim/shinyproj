# - Get the UI

get_ui <- function() {
  w_sb <- 650
  h_top <- 150
  css_settings <- get_css_settings()

  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(
      width = w_sb,
      plotOutput("global", width = w_sb, height = 850, click = "size_click"),
      div(em("Select a suggested model by clicking a column corresponding to",
             "the desired model size."), align = "center", style = "color:grey")
    ),
    dashboardBody(
      useShinyjs(),
      tags$style(HTML(css_settings)),
      # button for sidebar and statistic-menu, and variable selection table
      fluidRow(
        column(actionButton("toggleSidebar", "Show/hide performance plot"),
               uiOutput("stat"), width = 3, height = h_top),
        column(dataTableOutput("vars"), width = 9, height = h_top)
      ),
      fluidRow(
        column(
          width = 6,
          box(plotOutput("clust_dend"), collapsible = T, width = 12,
              title = "Dendogram of the single variable model predictions"),
          box(plotOutput("hist"), collapsible = T, width = 12,
              title = "Histogram of the selected variables"),
          box(plotOutput("diff"), collapsible = T, collapsed = T, width = 12,
              title = "Performance difference to the best model of same size")
        ),
        column(
          width = 6,
          box(plotOutput("clust_2d"), collapsible = T, width = 12,
              title = "Scatterplot of the sigle variable model predictions"),
          box(plotOutput("pairs"), collapsible = T, width = 12,
              title = "Pairs plot of the selected variables"),
          box(plotOutput("ppd"), collapsible = T, collapsed = T, width = 12,
              title = "Predictive distribution of the selected model"))
      )
    )
  )
}
