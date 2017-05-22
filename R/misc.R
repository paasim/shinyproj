# Miscellanneous functions

#' @importFrom rstanarm get_x
#' @importFrom magrittr "%>%"
#' @importFrom stats cor setNames nobs
extract_data <- function(fit, nv) {
  x <- matrix(get_x(fit), nrow = nobs(fit))
  if (attr(fit$terms, 'intercept')) x <- x[, -1, drop = FALSE]
  dist <- 1 - abs(cor(x))
  stat_arr <- boot_stats(fit$varsel, 1:nv)
  ch <- fit$varsel$vind
  sug <- fit$varsel$ssize
  stat_vals <- unique(stat_arr$stat)
  stat_def <- switch(fit$family$family,
                     'gaussian' = 'mse', 'binomial' = 'pctcorr', 'mlpd')
  proj <- project(fit, nv = seq_along(ch))
  pctch <- round(fit$varsel$pctch, 2) %>% as_tibble()
  pct <- get_pct_arr(pctch, nv)

  list(fit = fit, x = x, dist = dist, stat_arr = stat_arr, nv = nv, sug = sug,
       proj = proj, stat_vals = stat_vals, stat_def = stat_def, ch = ch,
       pctch = pctch, pct = pct, d_test = fit$varsel$d_test)
}

#' @importFrom stats as.dist hclust
#' @importFrom magrittr "%>%"
clust_fun <- function(dist, sel_corr) {
  res <- dist[sel_corr, sel_corr] %>% as.dist() %>% hclust() %>% hc2arr()
  list(df = res$df, labs = names(sel_corr)[res$order])
}

#' @importFrom magrittr "%>%"
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
hc2arr <- function(hc) {
  mat <- matrix(NA, length(hc$height), 5,
                dimnames = list(NULL, c('x1', 'yd1', 'x2', 'yd2', 'yu')))

  fneg <- function(x) c(order(hc$order)[x], 0)
  fpos <- function(x) c(mean(mat[x, c('x1', 'x2')]), mat[x, 'yu'])
  fx <- function(x) if (x<0) fneg(abs(x)) else fpos(x)
  f0 <- function(i) c(sapply(hc$merge[i, ], fx), hc$height[i])

  for (i in 1:nrow(mat)) mat[i, ] <- f0(i)

  res <- lapply(1:nrow(mat), function(i)
    tibble(x1 = mat[i, c('x1', 'x2', 'x1')], x2 = mat[i, c('x1', 'x2', 'x2')],
           y1 = mat[i, c('yd1', 'yd2', 'yu')], y2 = mat[i, rep('yu', 3)])
  ) %>% bind_rows()
  list(df = res, order = hc$order)
}

validate_varsel <- function(fit_cv) {
  !is.null(fit_cv$varsel$ssize)
}

#' @importFrom tidyr gather_
#' @importFrom tibble as_tibble
#' @importFrom magrittr "%>%"
get_pct_arr <- function(pctch, nv) {
  gather_(pctch[1:nv, ], 'var', 'val', names(pctch)[1:nv+1], factor_key = T)
}

#' @importFrom DT datatable formatStyle styleInterval
#' @importFrom htmltools tags
#' @importFrom stats setNames
#' @importFrom magrittr "%>%"
gen_vars_table <- function(pctch, sug) {
  arr <- pctch[sug, -1]
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
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom magrittr "%>%"
pairs_fun <- function(x, sel_corr) {
  combn(sel_corr, 2, simplify = F) %>%
    lapply(function(i)
      tibble(x1 = x[, i[1]], x2 = x[, i[2]],
             n1 = rep(x = names(i)[1], nrow(x)),
             n2 = rep(x = names(i)[2], nrow(x)))
    ) %>% bind_rows()
}

#' @importFrom RColorBrewer brewer.pal
get_col_brks <- function() {
  list(breaks = seq(5e-3, 1-5e-3, length.out = 7),
       pal = brewer.pal(11, "RdBu")[3:10])
}

null_if_cond <- function(cond, expr) if(cond) NULL else expr

.onAttach <- function(...) {
  ver <- utils::packageVersion("shinyproj")
  packageStartupMessage("This is shinyproj version ", ver)
}
