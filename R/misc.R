# Miscellanneous functions

#' @importFrom rstanarm get_x
#' @importFrom stats cor setNames
extract_data <- function(fit, nv) {
  x <- get_x(fit)
  x <- x[, as.logical(attr(x, 'assign'))]
  dist <- 1 - abs(cor(x))
  stat_arr <- boot_stats(fit$varsel, 1:nv)
  ch <- with(fit$varsel, setNames(chosen, chosen_names))
  sug <- fit$varsel$ssize
  stat_vals <- unique(stat_arr$stat)
  stat_def <- switch(fit$family$family,
                     'gaussian' = 'mse', 'binomial' = 'pctcorr', 'mlpd')
  proj <- project(fit, nv = seq_along(ch))
  pctch <- round(fit$varsel$pctch, 2)
  pct <- get_pct_arr(pctch, nv)

  list(fit = fit, x = x, dist = dist, stat_arr = stat_arr, nv = nv, sug = sug,
       proj = proj, stat_vals = stat_vals, stat_def = stat_def, ch = ch,
       pctch = pctch, pct = pct, d_test = fit$varsel$d_test)
}

#' @importFrom stats as.dist hclust
clust_fun <- function(dist, sel_corr) {
  dist[sel_corr, sel_corr] %>% as.dist %>% hclust %>% hc2arr
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

#' @importFrom tidyr gather
get_pct_arr <- function(pctch, nv) {
  pctch[1:nv, ] %>% as.data.frame %>%
    gather('var', 'val', 2:ncol(pctch), factor_key = T)
}

#' @importFrom DT datatable formatStyle styleInterval
#' @importFrom htmltools tags
#' @importFrom stats setNames
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

sel_corrs <- function(dist, ch, sel, not_sel, n) {
  ord <- order(dist[sel, not_sel])[1:n]
  cols <- ((ord-1) %/% length(sel)) + 1
  rows <- ((ord-1) %% length(sel)) + 1
  ch[ch %in% c(sel[rows], not_sel[cols])]
}

#' @importFrom utils combn
#' @importFrom stats setNames
pairs_fun <- function(x, sel_corr) {
  x <- x[, sel_corr, drop = FALSE] %>% as.data.frame()
  names <- combn(colnames(x), 2, simplify = F)
  do.call(rbind, lapply(names, function(n) {
    setNames(cbind(x[, n], n[1], n[2]), c('x','y','xn','yn'))
  }))
}

#' @importFrom RColorBrewer brewer.pal
get_col_brks <- function() {
  list(breaks = seq(5e-3, 1-5e-3, length.out = 7),
       pal = brewer.pal(11, 'RdBu')[3:10])
}

null_if_cond <- function(cond, expr) if(cond) NULL else expr
