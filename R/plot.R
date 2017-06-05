# - Functions related to plotting

included <- function(x, y) any(y %in% x)

cl_2d_plot <- function(cl, sel) {
  grps <- sapply(cl$inds, included, sel) %>% which()
  sel_grp <- match(x = cl$hulls$grp, table = grps, nomatch = 0) > 0
  sel_pt <- cl$pts$ind %in% sel + 0
  sel_ff <- ifelse(sel_pt, "bold", "plain")

  ggplot(cl$hulls, aes_(x = ~x, y = ~y)) +
    geom_polygon(aes_(color = ~grp), fill = NA) +
    geom_polygon(aes_(fill = ~grp, alpha = ~(sim * sel_grp))) +
    geom_point(aes_(size = ~sel_pt), cl$pts) +
    geom_label_repel(aes_(label = ~lab, fontface = ~sel_ff), cl$pts,
                     point.padding = unit(0.5, "lines"), size = 3.5) +
    scale_alpha_continuous(range = range(cl$hulls$sim * sel_grp)) +
    scale_size_continuous(range = c(1, 2)) +
    guides(color = "none", fill = "none", size = "none", alpha = "none") +
    labs(x = "", y = "") +
    theme_default() +
    theme_proj() +
    theme(axis.ticks = element_line(color = "white"),
          axis.text = element_text(color = "white"), legend.position = "bottom")
}

cl_dend_plot <- function(cl, sel) {
  lab_bold <- ifelse(cl$labs %in% sel, "bold", "plain")
  sel_ln <- sapply(cl$lines$inds, included, sel) + 0
  ggplot(cl$lines, aes_(x = ~x1, xend = ~x2, y = ~y1, yend = ~y2,
                        size = ~sel_ln)) +
    geom_segment() +
    scale_x_continuous(breaks = 1:length(cl$labs), labels = names(cl$labs)) +
    scale_y_continuous(labels = function(x) 1 - x) +
    scale_size_continuous(range = c(0.5, 1)) +
    guides(size = "none") +
    labs(x = "", y = "correlation") +
    theme_default() +
    theme_proj() +
    theme(axis.text.x = element_text(angle = -45, hjust = 0, face = lab_bold))
}

pairs_plot <- function(pairs) {
  ggplot(pairs, aes_(x = ~x1, y = ~x2)) +
    geom_point() +
    geom_smooth(color = "darkred", method = "lm", formula = y ~ x, se = F) +
    facet_grid(n2 ~ n1) +
    labs(x = "", y = "") +
    theme_default() +
    theme_proj() +
    theme(axis.text = element_text(color = "white"))
}

ppd_plot <- function(ppd, y) {
  ggplot(mapping = aes_(x = ~value)) +
    stat_density(aes_(group = ~key, color = "yrep"),
                 data = ppd, geom = "line", position = "identity",
                 size = 0.25, alpha = 0.3) +
    stat_density(aes_(color = "y"), geom = "line",
                 position = "identity", size = 0.8,
                 data = data.frame(value = y)) +
    scale_color_manual(values = c("black", "#B1BED9")) +
    coord_cartesian(expand = FALSE) +
    theme_default() +
    theme_proj() +
    theme(axis.text.y = element_text(color = "white"),
          axis.ticks.y = element_line(color = "white"),
          axis.title = element_blank(), legend.title = element_blank(),
          legend.position = c(0.9, 0.9))
}

hist_plot <- function(hist) {
  ggplot(hist, aes_(x = ~value)) +
    geom_histogram(color = "black", fill = "#B1BED9", bins = 15) +
    facet_wrap(~ key) +
    labs(y = "") +
    scale_x_continuous(breaks = pretty_breaks(n = 3)) +
    theme_bw() +
    theme_proj() +
    theme(axis.text.y = element_text(color = "white"),
          axis.ticks.y = element_line(color = "white"))
}

diff_plot <- function(sel_diff, stat, ns) {
  ggplot(tibble(y = sel_diff), aes_(y = ~y, x = "")) +
    stat_ydensity(geom = "violin",
                  draw_quantiles = 0.5, fill = "#B1BED9") +
    labs(x = "", y = paste("Difference in", stat)) +
    theme_default() +
    theme_proj() +
    theme(axis.ticks.x = element_line(color = "white"))
}

perf_plot <- function(stat_arr, nv, stat, sel_size, stat_diff) {
  df <- stat_arr[stat_arr$stat == stat, ]
  df_diff <- df[df$size == sel_size, c("size", "val")]
  if (nrow(df_diff) == 0) df_diff <- tibble(size = 0, val = 0)
  if (!is.null(stat_diff)) df_diff$val <- df_diff$val + stat_diff

  (ggplot(df, aes_(x = ~size, y = ~val)) +
      geom_hline(aes_(yintercept = 0), color = "darkred", linetype = 2) +
      geom_line() +
      geom_pointrange(aes_(ymin = ~lq, ymax = ~uq, fill = '1')) +
      geom_point(aes_(fill = '2'), df_diff,
                 color = "#000000", size = 3, shape = 21) +
      coord_cartesian(xlim = c(1-0.4, nv+0.4), expand = F) +
      scale_x_continuous(breaks = 1:nv) +
      scale_fill_manual(values = c("#000000", "#B1BED9"),
                         labels = c('Suggested', 'Selected'),
                         name = '') +
      labs(y = paste("Difference in", stat, "to the full model"),
           x = "", title = "Performance difference to the full model") +
      theme_default() +
      theme_proj() +
      theme(strip.text = element_blank(),
            axis.text.x = element_blank(),
            plot.background = element_rect(fill = "white", color = "white"),
            legend.position = c(0.9, 0.9))) %>%
    ggplotGrob()
}

gen_heat_bg <- function(pct, col, rows) {
  col_brks <- get_col_brks()
  pct$val_grp <- as.character(sapply(pct$val, function(x) sum(x >= col_brks$breaks)))
  if (identical(rows, 0)) rows <- pct$var[1]
  pct$sel <- (pct$size == col) & (pct$var %in% rows)
  brks <- sort(unique(as.numeric(pct$val_grp)) + 1)

  (ggplot(pct, aes_(x = ~size, y = ~var)) +
      geom_tile(aes_(fill = ~val_grp, color = ~sel),
                width = 1, height = 0.9, size = 1) +
      facet_grid(. ~ size, scales = "free_x", switch = "x") +
      geom_text(aes_(label = ~val, fontface = ~sel+1)) +
      coord_cartesian(expand = FALSE) +
      scale_y_discrete(limits = rev(levels(pct$var))) +
      scale_color_manual(values = c("white", "black")) +
      labs(x = "Model size", y = "",
           title = "Fraction of cv-folds that select the given variable") +
      scale_fill_manual(breaks = brks, values = col_brks$pal[brks]) +
      theme_proj() +
      theme(legend.position = "none",
            axis.text.y = element_text(angle = 45),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            panel.background=element_blank())) %>%
    ggplotGrob()
}

gen_dummy_bg <- function(pct, inds) {
  len <- ifelse(identical(inds, 0), 0, length(inds))
  (ggplot(pct, aes_(x = ~size, y = ~var)) +
      facet_grid(. ~ size, scales = "free_x", switch = "x") +
      geom_rect(aes_(fill = (~size == len)),
                xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
      scale_fill_manual(values = c("#F6F6F6", "#B1BED9")) +
      coord_cartesian(expand = FALSE) +
      theme_proj() +
      theme(legend.position = "none",
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            strip.background = element_blank())) %>%
    ggplotGrob()
}

comb_left <- function(diff, heat, pct, inds) {
  dummy <- gen_dummy_bg(pct, inds)
  heat_sel <- comb_heat(heat, dummy)
  new_width <-  unit.pmax(diff$widths[2:3], heat_sel$widths[2:3]) %>% as.list
  diff$widths[2:3] <- new_width
  heat_sel$widths[2:3] <- new_width
  # New gtable with space for the three plots plus a right-hand margin
  gtable(widths = unit(1, "null"), heights = unit(c(0.4, 0.6), "null")) %>%
    gtable_add_grob(diff, 1, 1) %>% gtable_add_grob(heat_sel, 2, 1)
}

comb_heat <- function(heat, dummy) {
  panels <- grepl(pattern = "panel", dummy$layout$name)
  strips <- grepl(pattern = "strip-b", dummy$layout$name)
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
  g1$layout$z <- g1$layout$z - max(g1$layout$z)
  g1$layout$name <- "g2"
  g1$layout <- rbind(g1$layout, g2$layout)
  g1
}

theme_proj <- function() {
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.6))
}

get_col_brks <- function() {
  list(breaks = seq(5e-3, 1-5e-3, length.out = 7),
       pal = brewer.pal(11, "RdBu")[3:10])
}
