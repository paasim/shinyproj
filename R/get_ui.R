# - Get the UI

get_ui <- function() {
  w_sb <- 650
  h_top <- 150
  css_settings <- get_css_settings()

  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(
      width = w_sb,
      plotOutput("global", width = w_sb, height = 850)
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
      uiOutput("plots")
    )
  )
}
