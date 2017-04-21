extract_data <- function(fit, nv) {
  x <- get_x(fit)
  x <- x[, as.logical(attr(x, 'assign'))]
  dist <- 1 - abs(cor(x))
  stat_arr <- boot_stats(fit$varsel) %>% subset(size <= nv & size >= 1)
  ch <- with(fit$varsel, setNames(chosen, chosen_names))
  sug <- fit$varsel$ssize
  stat_vals <- unique(stat_arr$stat)
  stat_def <- switch(fit$family$family,
                     'gaussian' = 'mse', 'binomial' = 'pctcorr', 'mlpd')
  proj <- project(fit, nv = seq_along(ch))
  pctch <- round(fit$varsel$pctch, 2)
  pct <- get_pct_arr(pctch, nv)

  res <- list(fit = fit, x = x, dist = dist, stat_arr = stat_arr, ch = ch,
              nv = nv, sug = sug, stat_vals = stat_vals, stat_def = stat_def,
              proj = proj, pctch = pctch, pct = pct)
}

hc2arr <- function(hc) {
  cols <- c('x1', 'yd1', 'x2', 'yd2', 'yu')
  mat <- matrix(NA, length(hc$height), 5, dimnames = list(NULL, cols))

  fneg <- function(x) c(order(hc$order)[x], 0)
  fpos <- function(x) c(mean(mat[x, c('x1', 'x2')]), mat[x, 'yu'])
  fx <- function(x) if(x<0) fneg(abs(x)) else fpos(x)
  f0 <- function(i) c(sapply(hc$merge[i, ], fx), hc$height[i])

  for(i in 1:nrow(mat)) mat[i, ] <- f0(i)

  res <- do.call(rbind, lapply(1:nrow(mat), function(i) {
    rbind(c(rep(mat[i,'x1'], 2), mat[i,'yd1'], mat[i,'yu']),
          c(rep(mat[i,'x2'], 2), mat[i,'yd2'], mat[i,'yu']),
          c(mat[i,'x1'], mat[i,'x2'], rep(mat[i,'yu'], 2)))
  }))
  colnames(res) <- c('x1', 'x2', 'y1', 'y2')
  list(df = as.data.frame(res), labs = hc$labels[hc$order])
}

validate_varsel <- function(fit_cv) !is.null(fit_cv$varsel$ssize)

get_pct_arr <- function(pctch, nv) {
  pctch[1:nv, ] %>% as.data.frame %>% gather(var, val, -size, factor_key = T)
}

gen_vars_table <- function(pctch, sug) {
  arr <- setNames(pctch[sug, ], colnames(pctch))[-1] %>% t %>% data.frame
  op <- list(searching = F, paging = F, bInfo = F, ordering = F, autoWidth = T)
  sels <- matrix(c(rep(1, sug), 1:sug - 1), sug)
  capt <- tags$caption(style = 'caption-side: bottom; text-align: center;',
                       em('Selected variables'))
  col_brks <- with(get_col_brks(), styleInterval(breaks, pal))
  datatable(
    data = arr, options = op, class = 'compact', rownames = F, caption = capt,
    selection = list(mode = 'multiple', target = 'cell', selected = sels)) %>%
    formatStyle(names(arr), backgroundColor = col_brks)
}

#' @importFrom RColorBrewer brewer.pal
get_col_brks <- function() {
  list(breaks = seq(5e-3, 1-5e-3, length.out = 7),
       pal = brewer.pal(11, 'RdBu')[3:10])
}

null_if_cond <- function(cond, expr) {
  if(cond) {
    NULL
  } else {
    expr
  }
}
