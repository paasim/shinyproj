#' @importFrom shiny reactive observeEvent renderText debounce renderUI
#' renderPlot selectInput
#' @importFrom DT dataTableAjax reloadData selectCells renderDataTable
#' dataTableProxy
#' @importFrom tidyr gather
#' @importFrom graphics plot
#' @importFrom stats setNames

get_server <- function(data) {
  function(input, output, session) {

    statistic <- reactive(input$statistic)
    plot_type <- reactive(input$plot_type)
    observeEvent(input$size_click, {
      sizeval <- ceiling((input$size_click$x - 0.13)/0.9*data$nv)
      if(is.null(sizeval) || sizeval < 1 || sizeval > data$nv)
        return(NULL)
      rd <- setNames(data$pctch[sizeval, ], colnames(data$pctch))[-1] %>%
        t %>% data.frame
      dataTableAjax(session, rd, rownames = F, outputId = 'vars')
      reloadData(proxy_vars, resetPaging = T, clearSelection = 'none')
      selectCells(proxy_vars, matrix(c(rep(1,sizeval), 1:sizeval-1), ncol = 2))
    })

    sel_quick <- reactive(
      if(is.null(input$vars_cells_selected)) {
        0
      } else {
        data$ch[input$vars_cells_selected[, 2] + 1]
      }
    )
    sel <- sel_quick %>% debounce(1000)
    not_sel <- reactive(data$ch[!(data$ch %in% sel())])

    # dendro + scatter selection
    sel_corr <- reactive(
      null_if_cond(identical(sel(), 0),
                   sel_corrs(data$dist, data$ch, sel(), not_sel(), 4))
    )
    sel_clust <- reactive(
      null_if_cond(is.null(sel_corr()), clust_fun(data$dist, sel_corr()))
    )
    sel_pairs <- reactive(
      null_if_cond(is.null(sel_corr()), pairs_fun(data$x, sel_corr()))
    )

    # projection
    sug_proj <- reactive(
      null_if_cond(identical(sel(), 0), data$proj[[length(sel())]])
    )
    sel_proj <- reactive(
      null_if_cond(identical(sel(), 0), project(data$fit, vind = sel()))
    )
    sel_ppd <- reactive(
      null_if_cond(is.null(sel_proj()) || (plot_type() != 'ppd'), {
        proj_predict(sel_proj(), data$x) %>% t %>% data.frame %>% gather
      })
    )
    sel_hist <- reactive(
      null_if_cond(is.null(sel_proj()) || (plot_type() != 'hist'), {
        p <- sel_proj()
        p$beta %>% t %>% data.frame %>% setNames(p$ind_names) %>% gather
      })
    )
    sel_diff <- reactive({
      null_if_cond(identical(sel(), 0) || all(sel() %in% data$ch[seq_along(sel())]), {
        eval_stat(sel_proj(), sug_proj(), data$x, data$d_test, statistic()) %>%
          data.frame %>% gather
      })
    })
    stat_diff <- reactive(ifelse(is.null(sel_diff()), 0, mean(sel_diff()$value)))

    # LHS plot(s)
    output$statistic <- renderUI(
      selectInput('statistic', label = 'Summary statistic',
                  choices = data$stat_vals, selected = data$stat_def, width = '25%')
    )
    diff_full <- reactive({
      diff_plot(data$stat_arr, data$nv, statistic(), length(sel()), stat_diff())
    })
    heat <- reactive(
      gen_heat_bg(data$pct, length(sel()), names(sel()))
    )
    output$diff_heat <- renderPlot({
      comb_left(diff_full(), heat(), data$pct, sel()) %>% plot
    })
    output$vars <- renderDataTable(
      gen_vars_table(data$pctch, data$sug)
    )
    proxy_vars <- dataTableProxy(session$ns('vars'))

    # RHS plots
    output$dendro <- renderPlot(
      null_if_cond(is.null(sel_clust()), dend_plot(sel_clust()))
    )
    output$scatter <- renderPlot(
      null_if_cond(is.null(sel_pairs()), pairs_plot(sel_pairs()))
    )
    output$statplot <- renderPlot(
      null_if_cond(is.null(sel_diff()),
                   stat_plot(sel_diff(), statistic(), length(sel())))
    )

    output$diag <- renderPlot(
      if(!is.null(sel_ppd())) {
        ppd_plot(sel_ppd(), data$d_test$y)
      } else if(!is.null(sel_hist())) {
        hist_plot(sel_hist())
      } else {
        NULL
      }
    )
  }
}
