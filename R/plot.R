# - functions related to plotting (mainly the plot on the LHS)

theme_proj <- function() {
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = 'bold', hjust = 0.6))
}

diff_plot <- function(stat_arr, nv, statistic, sel_size, stat_diff) {
  df <- subset(stat_arr, stat == statistic)
  df_diff <- subset(df, size == sel_size, c('size', 'val'))
  if(!is.null(stat_diff)) df_diff$val <- df_diff$val + stat_diff

  (ggplot(df, aes(x = size, y = val)) +
      geom_errorbar(aes(ymin = lq, ymax = uq), width = 0.2, alpha = 0.5) +
      geom_line() +
      geom_point(aes(size = (size == sel_size),
                     color = (size == sel_size + stat_diff))) +
      geom_point(data = df_diff, size = 3, color = '#B1BED9') +
      coord_cartesian(xlim = c(1-0.4, nv+0.4), expand = F) +
      geom_hline(aes(yintercept = 0), color = 'darkred', linetype = 2) +
      scale_x_continuous(breaks = 1:nv) +
      scale_color_manual(values = c('black', '#B1BED9')) +
      scale_size_manual(values = c(2.5, 3)) +
      labs(y = paste('Difference in', statistic, 'to the full model'),
           x = '', title = 'Performance difference to the full model') +
      bayesplot::theme_default() +
      theme_proj() +
      theme(strip.text = element_blank(),
            axis.text.x = element_blank(),
            legend.position = 'none',
            plot.background = element_rect(fill = 'white', color = 'white'))) %>%
    ggplotGrob
}
gen_heat_bg <- function(pct, col, rows) {
  col_brks <- get_col_brks()
  pct$val_grp <- sapply(pct$val, function(x) sum(x >= col_brks$breaks))
  if(identical(rows, 0)) rows <- pct$var[1]
  pct$sel <- (pct$size == col) & (pct$var %in% rows)

  (ggplot(pct, aes(x = size, y = var)) +
      geom_tile(aes(fill = as.character(val_grp), color = sel),
                width = 1, height = 0.9, size = 1) +
      facet_grid(. ~ size, scales = 'free_x', switch = 'x') +
      geom_text(aes(label = round(val, 2)), fontface = 'bold') +
      coord_cartesian(expand = FALSE) +
      scale_y_discrete(limits = rev(levels(pct$var))) +
      scale_color_manual(values = c('white', '#B1BED9')) +
      labs(x = 'Model size', y = '',
           title = 'Fraction of cv-folds that select the given variable') +
      scale_fill_manual(breaks = sort(unique(pct$val_grp) + 1),
                        values = col_brks$pal[sort(unique(pct$val_grp) + 1)]) +
      theme_proj() +
      theme(legend.position = 'none',
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank())) %>%
    ggplotGrob
}

gen_dummy_bg <- function(pct, inds) {
  len <- ifelse(identical(inds, 0), 0, length(inds))
  (ggplot(pct, aes(x = size, y = var)) +
      facet_grid(. ~ size, scales = 'free_x', switch = 'x') +
      geom_rect(aes(fill = (size == len)),
                xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
      scale_fill_manual(values = c('#F6F6F6', '#B1BED9')) +
      coord_cartesian(expand = FALSE) +
      theme_proj() +
      theme(legend.position = 'none',
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            strip.background = element_blank())) %>%
    ggplotGrob
}

gen_dummy_bg2 <- function(pct, inds) {
  len <- ifelse(identical(inds, 0), 0, length(inds))
  (ggplot(pct, aes(x = size, y = var)) +
      facet_grid(. ~ size, scales = 'free_x', switch = 'x') +
      geom_rect(aes(color = (size == len)),
                xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
      coord_cartesian(expand = FALSE) +
      theme_proj() +
      theme(legend.position = 'none',
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            strip.background = element_blank())) %>%
    ggplotGrob
}

#' @importFrom grid unit.pmax
#' @importFrom gtable gtable gtable_add_grob
comb_left <- function(diff, heat, pct, inds) {
  dummy <- gen_dummy_bg(pct, inds)
  dummy2 <- gen_dummy_bg2(pct, inds)
  heat_sel <- comb_heat(heat, dummy)
  new_width <-  unit.pmax(diff$widths[2:3], heat_sel$widths[2:3]) %>% as.list
  diff$widths[2:3] <- new_width
  heat_sel$widths[2:3] <- new_width
  # New gtable with space for the three plots plus a right-hand margin
  gtable(widths = unit(1, 'null'), height = unit(c(0.4, 0.6), 'null')) %>%
    gtable_add_grob(diff, 1, 1) %>% gtable_add_grob(heat_sel, 2, 1)
}

comb_heat <- function(heat, dummy) {
  panels <- grepl(pattern = 'panel', dummy$layout$name)
  strips <- grepl(pattern = 'strip-b', dummy$layout$name)
  dummy$layout$t[panels] <- dummy$layout$t[panels] + 1
  dummy$layout$b[panels] <- dummy$layout$b[panels] + 1
  new_strips <- gtable_select(dummy, panels | strips)

  gtable_stack(heat, new_strips)
}

gtable_select <- function (x, ...) {
  matches <- c(...)
  x$layout <- x$layout[matches, , drop = FALSE]
  x$grobs <- x$grobs[matches]
  x
}

gtable_stack <- function(g1, g2) {
  g1$grobs <- c(g1$grobs, g2$grobs)
  g1$layout <- transform(g1$layout, z = z - max(z), name = 'g2') %>%
    rbind(g2$layout)
  g1
}

dend_plot <- function(cl) {
  ggplot(cl$df) +
    geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2)) +
    scale_x_continuous(breaks = 1:length(cl$labs), labels = cl$labs) +
    scale_y_continuous(labels = function(x) 1-x) +
    labs(x = '', y = 'correlation') +
    bayesplot::theme_default() +
    theme_proj()
}

pairs_plot <- function(pairs) {
  ggplot(pairs, aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(color = 'darkred', method = 'lm', formula = y ~ x, se = F) +
    facet_grid(yn ~ xn) +
    labs(x = '', y = '') +
    bayesplot::theme_default() +
    theme_proj() +
    theme(axis.text = element_text(color = 'white'))
}

ppd_plot <- function(ppd, y) {
  ggplot(mapping = aes(x = value)) +
    stat_density(aes(group = key, color = 'yrep'),
                 data = ppd, geom = 'line', position = 'identity',
                 size = 0.25, alpha = 0.3) +
    stat_density(aes(color = 'y'), geom = 'line',
                 position = 'identity', size = 0.8,
                 data = data.frame(value = y)) +
    scale_color_manual(values = c('black', '#B1BED9')) +
    coord_cartesian(expand = FALSE) +
    labs(title = 'Samples from the predictive distribution') +
    bayesplot::theme_default() +
    theme_proj() +
    theme(axis.text.y = element_text(color = 'white'),
          axis.ticks.y = element_line(color = 'white'),
          axis.title = element_blank(), legend.title = element_blank(),
          legend.position = c(0.9, 0.9))
}

#' @importFrom scales pretty_breaks
hist_plot <- function(hist) {
  ggplot(hist) +
    geom_histogram(aes(x = value), bins = 15) +
    facet_wrap(~ key) +
    labs(y = '', title = 'Histograms of the selected variables') +
    scale_x_continuous(breaks = pretty_breaks(n = 3)) +
    theme_bw() +
    theme_proj() +
    theme(axis.text.y = element_text(color = 'white'),
          axis.ticks.y = element_line(color = 'white'))
}

stat_plot <- function(sel_diff, statistic, ns) {
  ggplot(sel_diff) +
    stat_ydensity(aes(y = value, x = ''), geom = 'violin',
                  draw_quantiles = 0.5, fill = '#B1BED9') +
    labs(title = paste('Performance difference to the best model of size', ns),
         x = '', y = paste('Difference in', statistic)) +
    bayesplot::theme_default() +
    theme_proj() +
    theme(axis.ticks.x = element_line(color = 'white'))
}
