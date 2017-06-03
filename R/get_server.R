# - Get the server function

get_server <- function(data) {
  function(input, output, session) {

    stat <- reactive(input$stat)
    clust_type <- reactive(input$clust_type)
    pairs_ppd_type <- reactive(input$pairs_ppd_type)

    observeEvent(input$size_click, {
      sizeval <- ceiling((input$size_click$x - 0.13)/0.9*data$nv)
      if(is.null(sizeval) || sizeval < 1 || sizeval > data$nv) return(NULL)
      dataTableAjax(session, data$pctch[sizeval, -1],
                    rownames = F, outputId = 'vars')
      reloadData(proxy_vars, resetPaging = T, clearSelection = 'none')
      selectCells(proxy_vars, matrix(c(rep(1,sizeval), 1:sizeval-1), ncol = 2))
    })

    sel_quick <- reactive(
      if (length(input$vars_cells_selected) > 0) {
        data$ch[input$vars_cells_selected[, 2] + 1]
      }
    )
    sel <- sel_quick %>% debounce(1000)

    # pairs for correlation-scatterplots
    sel_pairs <- reactive(
      if (!is.null(sel()) && length(sel) > 1) pairs_fun(data$x, sel())
    )

    # projection
    sug_proj <- reactive(if (!is.null(sel())) data$proj[[length(sel())]])
    sel_proj <- reactive(
     if (!is.null(sel())) project(data$fit, vind = sel(), ns = 100)
    )
    sel_ppd <- reactive(
      if (!is.null(sel_proj()))
        proj_predict(sel_proj(), data$x, draws = 100) %>%
          t() %>% as_tibble() %>% gather()
    )
    sel_hist <- reactive(
      if (!is.null(sel_proj()))
        with(sel_proj(), setNames(as_tibble(t(beta)), names(vind)) %>% gather)
    )
    sel_diff <- reactive(
      if (!is.null(sel()) && any(!(sel() %in% data$ch[seq_along(sel())])))
        eval_stat(sel_proj(), sug_proj(), data$x, data$fit$varsel$d_test, stat())
    )
    stat_diff <- reactive(if (is.null(sel_diff())) 0 else mean(sel_diff()))

    # "Global" plot
    output$stat <- renderUI(
      selectInput("stat", "Summary statistic", unique(data$stat_arr$stat),
                  switch(data$fit$family$family, 'gaussian' = 'mse',
                         'binomial' = 'pctcorr', 'mlpd'), width = "25%")
    )
    perf <- reactive({
      perf_plot(data$stat_arr, data$nv, stat(), length(sel()), stat_diff())
    })
    heat <- reactive(
      gen_heat_bg(data$pct, length(sel()), names(sel()))
    )
    output$global <- renderPlot({
      comb_left(perf(), heat(), data$pct, sel()) %>% plot()
    })
    output$vars <- renderDataTable(
      gen_vars_table(data$pctch, data$fit$varsel$ssize)
    )
    proxy_vars <- dataTableProxy(session$ns("vars"))

    # LHS plots
    output$diff <- renderPlot(
      if (!is.null(sel_diff())) diff_plot(sel_diff(), stat(), length(sel()))
    )
    output$clust <- renderPlot(
      if (!is.null(sel()))
        if (clust_type() == "dend") {
          cl_dend_plot(data$cl_d, sel())
        } else if (clust_type() == "2d") {
          cl_2d_plot(data$cl_2d, sel())
        }
    )

    # RHS plots
    output$hist <- renderPlot(if (!is.null(sel_hist())) hist_plot(sel_hist()))
    output$pairs_ppd <- renderPlot(
      if (!is.null(sel_pairs()) && (pairs_ppd_type() == "pairs")) {
        pairs_plot(sel_pairs())
      } else if (!is.null(sel_ppd()) && (pairs_ppd_type() == "ppd")) {
        ppd_plot(sel_ppd(), data$fit$varsel$d_test$y)
      }
    )
  }
}
