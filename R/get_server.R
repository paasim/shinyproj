# - Get the server function

get_server <- function(data) {
  function(input, output, session) {

    stat <- reactive(input$stat)
    output$stat <- renderUI(
      selectInput("stat", "Summary statistic", unique(data$stat_arr$stat),
                  switch(data$fit$family$family,
                         'gaussian' = 'mse', 'binomial' = 'pctcorr', 'mlpd')))

    observeEvent(input$toggleSidebar, {
      toggleClass(selector = "body", class = "sidebar-collapse")
    })

    observeEvent(input$vars_click, {
      print(input$vars_click)
      ind <- input$vars_click[2] + 1
      if (ind < 0) return()
      match_ind <- val$sel == data$ch[ind]
      if (any(match_ind)) {
        val$sel <- val$sel[!match_ind]
      } else {
        val$sel <- c(val$sel, data$ch[ind])
      }
      paste0(get_selector(ind), ".toggleClass('selected-custom')") %>% runjs()
    })


    observeEvent(input$size_click, {
      sizeval <- ceiling((input$size_click$x - 0.13)/0.9*data$nv)
      if (is.null(sizeval) || sizeval < 1 || sizeval > data$nv) return()
      dataTableAjax(session, data$pctch[sizeval, -1, drop = F],
                    rownames = F, outputId = 'vars')
      reloadData(proxy_vars, resetPaging = T)
      val$sel <- data$ch[1:sizeval]
    })

    observeEvent(sel(), {
      runjs(paste0(get_selector(), ".removeClass('selected-custom')"))
      which(data$ch %in% sel()) %>%
        get_selector() %>%
        paste0(".addClass('selected-custom')") %>%
        lapply(runjs)
    })

    val <- reactiveValues(sel = c(' ' = 0)[-1]) # empty named numeric
    sel_quick <- reactive(val$sel)
    sel <- sel_quick %>% debounce(1000)

    # pairs for correlation-scatterplots
    sel_pairs <- reactive(
      if (length(sel()) > 1) pairs_fun(data$x, sel())
    )

    # projection
    sug_proj <- reactive(data$proj[[length(sel())]])
    sel_proj <- reactive(
     if (length(sel()) > 0) project(data$fit, vind = sel(), ns = 100)
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
      if ((length(sel()) > 0) && any(!(sel() %in% data$ch[seq_along(sel())])))
        eval_stat(sel_proj(), sug_proj(), data$x, data$fit$varsel$d_test, stat())
    )
    stat_diff <- reactive(if (is.null(sel_diff())) 0 else mean(sel_diff()))

    # Select variables
    output$vars <- renderDataTable(
      gen_vars_table(data$pctch, data$fit$varsel$ssize, FALSE)
    )
    proxy_vars <- dataTableProxy(session$ns("vars"))

    # "Global" plot
    perf <- reactive(
      if (!is.null(stat()))
        perf_plot(data$stat_arr, data$nv, stat(), length(sel()), stat_diff())
    )
    heat <- reactive(gen_heat_bg(data$pct, length(sel()), names(sel())))
    output$global <- renderPlot(
      if (!is.null(stat()))
        comb_left(perf(), heat(), data$pct, sel()) %>% plot()
    )

    # Smaller plots
    output$diff <- renderPlot(
      if (!is.null(sel_diff())) diff_plot(sel_diff(), stat(), length(sel()))
    )
    output$clust_2d <- renderPlot(cl_2d_plot(data$cl_2d, sel()))
    output$clust_dend <- renderPlot(cl_dend_plot(data$cl_d, sel()))
    output$hist <- renderPlot(
      if (!is.null(sel_hist())) hist_plot(sel_hist())
    )
    output$pairs <- renderPlot(
     if (!is.null(sel_pairs())) pairs_plot(sel_pairs())
    )
    output$ppd <- renderPlot(
      if (!is.null(sel_ppd())) ppd_plot(sel_ppd(), data$fit$varsel$d_test$y)
    )

    session$onSessionEnded(stopApp)
  }
}
