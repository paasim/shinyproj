# - Get the UI

get_ui <- function() {
  width_sub <- "450px"
  height_sub <- "350px"
  fluidPage(
    tags$style(HTML("
      table.dataTable tr.selected td, table.dataTable td.selected {
        border: 1px solid black;
        font-weight: bold;
      }
      table thead th {
        padding: 50px 0px 0px 0px !important;
        transform: rotate(315deg) translate(22px, -8px);
        table-layout: fixed !important;
        width: 35px !important;
        border-bottom: 0px !important;
        transform-origin: bottom right;
      }
      table {
        width: 0px !important;
        table-layout: fixed !important;
      }")
    ),
    fluidRow(
      column(5, h3("model size selection", align = "center"),
             uiOutput("stat")),
      column(7, h3("edit the selected submodel", align = "center"),
             dataTableOutput("vars"))
    ),
    column(
      width = 5, align = "center",
      fluidRow(
        plotOutput("global", width = "650px", height = "800px",
                   click = "size_click"),
        em("Select a suggested model by clicking a column corresponding to",
           "the desired model size.", style = "color:grey")
      )),
    column(
      width = 7,
      column(
        width = 6, align = "center", style = "border-left: dotted #d3d3d3;",
        fluidRow(
          # h4("", align = "center"),
          plotOutput("diff", width = width_sub, height = height_sub),
          plotOutput("clust", width = width_sub, height = height_sub),
          radioButtons("clust_type", "Plot type", selected = "dend", inline = T,
                       choices = c("Dendogram" = "dend",
                                   "2D clustering" = "2d")))),
      column(
        width = 6, align = "center", style = "border-left: dotted #d3d3d3;",
        fluidRow(
          # h4("", align = "center"),
          plotOutput("hist", width = width_sub, height = height_sub),
          plotOutput("pairs_ppd", width = width_sub, height = height_sub),
          radioButtons("pairs_ppd_type", "Plot type", selected = "pairs", inline = T,
                       choices = c("Pairs plot" = "pairs",
                                   "Predictive density" = "ppd"))))
    )
  )
}
