get_server <- function(data) {
  function(input, output) {

    statistic <- reactive(input$statistic)
    observeEvent(input$heat_cell_clicked, {
      info <- input$heat_cell_clicked
      if(is.null(info$value) || info$col != 0)
        return(NULL)
      proxy_vars %>% selectCells(matrix(c(rep(1, info$row),
                                          data$ch[1:info$row] - 1), ncol = 2))
    })

    sel_quick <- reactive(
      if(is.null(input$vars_cells_selected) ||
         nrow(input$vars_cells_selected) == 0) {
        0
      } else {
        input$vars_cells_selected[, 2] + 1
      }
    )
    sel <- sel_quick %>% debounce(1000)
    not_sel <- reactive(data$ch[!(data$ch %in% sel())])

    # dendro + scatter
    sel_corr <- reactive(
      if(identical(sel(), 0)) {
        return(NULL)
      } else {
        mins <- colMins(data$dist, sel(), not_sel(), which = 1)
        addits <- data$ch[which(order(mins) <= 3)]
        data$ch[data$ch %in% c(sel(), addits)]
      }
    )
    sel_x <- reactive(
      if(identical(sel(), 0)) {
        return(NULL)
      } else {
        data$x[, sel_corr(), drop = FALSE]
      }
    )
    sel_pairs <- reactive(
      if(identical(sel(), 0)) {
        return(NULL)
      } else {
        xx <- as.data.frame(sel_x())
        names <- combn(colnames(xx), 2, simplify = F)
        pairs <- do.call(rbind, lapply(names, function(n) {
          setNames(cbind(xx[, n], n[1], n[2]), c('x','y','xn','yn'))
        }))
      }
    )
    sel_clust <- reactive(
      if(identical(sel(), 0)) {
        return(NULL)
      } else {
        data$dist[sel_corr(), sel_corr()] %>% as.dist %>% hclust %>% hc2arr
      }
    )

    # projection
    sug_proj <- reactive(
      if(identical(sel(), 0)) {
        NULL
      } else {
        data$proj[[length(sel())]]
      }
    )
    sel_proj <- reactive(
      if(identical(sel(), 0)) {
        NULL
      } else {
        project(data$fit, vind = sel())
      })

    sel_ppd <- reactive(
      if(is.null(sel_proj())) {
        NULL
      } else {
        proj_predict(sel_proj(), data$x) %>% t %>% data.frame %>% gather
      }
    )

    sel_diff <- reactive({
      if(identical(sel(), 0) || all(sel() == data$ch[seq_along(sel())])) {
        return(NULL)
      } else {
        eval_stat(sel_proj(), sug_proj(), data$x, data$fit$varsel$d_test,
                  statistic()) %>% data.frame %>% gather
      }
    })

    # UI_components
    output$statistic <- renderUI(
      selectInput("statistic", label = "Summary statistic",
                  choices = data$stat_vals, selected = data$stat_def, width = '30%')
    )

    ## PLOTS
    output$diff_full <- renderPlot(
      ggplot(data = subset(data$stat_arr, stat == statistic()),
             aes(x = size, y = val)) +
        geom_errorbar(aes(ymin = lq, ymax = uq), width = 0.2, alpha = 0.5) +
        geom_line() +
        geom_point() +
        geom_hline(aes(yintercept = 0), color = 'darkred', linetype = 2) +
        scale_x_continuous(breaks = data$brks$breaks) +
        labs(y = paste('Difference in', statistic(), 'to the full model'),
             x = 'Model size',
             title = 'Performance difference to the full model') +
        theme_proj() +
        theme(strip.text = element_blank(), legend.position = 'none')
    )

    output$heat <- DT::renderDataTable(
      as.datatable(data$pct, selection = 'none', rownames = F,
                   options = list(searching = F, paging = F, bInfo = F,
                                     ordering = F, autoWidth = T),
                   class = 'compact',
                   caption = tags$caption(
                     style = 'caption-side: bottom; text-align: center;',
                     em(paste('Select a suggested model by clicking the desired',
                              'model size on the size column.' )))) %>%
        formatStyle(1, cursor = 'pointer')
    )

    output$vars <- DT::renderDataTable(
      datatable(
        data = data.frame(t(sort(data$ch))), rownames = F,
        selection = list(mode = 'multiple', target = 'cell',
                         seleced = data$ch[1:data$sug]),
        options = list(searching = F, paging = F, bInfo = F, ordering = F,
                          autoWidth = T),
        caption = tags$caption(
          style = 'caption-side: bottom; text-align: center;',
          em('Selected variables'))
      )
    )
    proxy_vars <- dataTableProxy('vars')


    output$dendro <- renderPlot(
      if(!is.null(sel_clust())) {
        cl <- sel_clust()
        ggplot(cl$df) +
          geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2)) +
          scale_x_continuous(breaks = 1:length(cl$labs), labels = cl$labs) +
          scale_y_continuous(labels = function(x) 1-x) +
          labs(x = '', y = 'correlation') +
          theme_proj()
      } else {
        NULL
      }
    )

    output$scatter <- renderPlot(
      if(!is.null(sel_pairs())) {
        ggplot(sel_pairs()) +
          geom_point(aes(x = x, y = y)) +
          facet_grid(xn~yn) +
          labs(x = '', y = '') +
          theme_proj()
      } else {
        NULL
      }
    )

    output$ppc <- renderPlot(
      if(!is.null(sel_ppd())) {
        ggplot(mapping = aes(x = value)) +
          stat_density(aes(group = key, color = 'yrep'),
                       data = sel_ppd(), geom = 'line', position = 'identity',
                       size = 0.25, alpha = 0.3) +
          stat_density(aes(color = 'y'), geom = 'line',
                       position = 'identity', size = 0.8,
                       data = data.frame(value = data$fit$varsel$d_test$y)) +
          scale_color_manual(values = c('black', '#9999CC')) +
          coord_cartesian(expand = FALSE) +
          labs(title = 'Samples from the predictive distribution') +
          theme_proj() +
          theme(axis.text.y = element_text(color = 'white'),
                axis.ticks.y = element_line(color = 'white'),
                axis.title = element_blank(), legend.title = element_blank(),
                legend.position = c(0.9, 0.9))
      } else {
        NULL
      }
    )

    output$statplot <- renderPlot(
      if(!is.null(sel_diff())) {
        ggplot(sel_diff()) +
          stat_ydensity(aes(y = value, x = ''), geom = 'violin',
                        draw_quantiles = 0.5, fill = '#9999CC') +
          labs(title = paste('Performance difference to the best model of',
                             'size', length(sel())),
               x = '', y = paste('Difference in', statistic())) +
          theme(axis.text.y = element_text(color = 'white'),
                axis.ticks.x = element_line(color = 'white')) +
          theme_proj()
      } else {
        NULL
      }
    )
  }
}
