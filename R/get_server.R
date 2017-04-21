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
    output$tk1 <- renderText(length(sel()))
    output$tk2 <- renderText(names(sel()))

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
      null_if_cond(identical(sel(), 0), {
        ord <- order(data$dist[sel(), not_sel()])[1:4]
        cols <- ((ord-1) %/% length(sel())) + 1
        rows <- ((ord-1) %% length(sel())) + 1
        data$ch[data$ch %in% c(sel()[rows], not_sel()[cols])]
      })
    )
    sel_x <- reactive(
      null_if_cond(identical(sel(), 0), data$x[, sel_corr(), drop = FALSE])
    )
    sel_pairs <- reactive(
      null_if_cond(identical(sel(), 0), {
        xx <- as.data.frame(sel_x())
        names <- combn(colnames(xx), 2, simplify = F)
        pairs <- do.call(rbind, lapply(names, function(n) {
          setNames(cbind(xx[, n], n[1], n[2]), c('x','y','xn','yn'))
        }))
      })
    )
    sel_clust <- reactive(
      null_if_cond(identical(sel(), 0), {
        data$dist[sel_corr(), sel_corr()] %>% as.dist %>% hclust %>% hc2arr
      })
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
      null_if_cond(identical(sel(), 0) || all(sel() == data$ch[seq_along(sel())]), {
        eval_stat(sel_proj(), sug_proj(), data$x, data$fit$varsel$d_test,
                  statistic()) %>% data.frame %>% gather
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
    output$vars <- DT::renderDataTable(
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
        ppd_plot(sel_ppd(), data$fit$varsel$d_test$y)
      } else if(!is.null(sel_hist())) {
        hist_plot(sel_hist())
      } else {
        NULL
      }
    )
  }
}
