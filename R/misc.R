# - Miscellanneous functions

extract_data <- function(fit, nv) {
  x <- matrix(get_x(fit), nrow = nobs(fit))
  if (attr(fit$terms, 'intercept')) x <- x[, -1, drop = FALSE]
  ch <- fit$varsel$vind

  dfun <- function(x) 1 - cor(x)
  proj_dist <- proj_singles(fit, x) %>% dfun() %>% as.dist()
  hc <- hclust(proj_dist)
  cmd <- cmdscale(proj_dist)
  cl_2d <- hc_to_clusters(hc, cmd, fit$varsel$vind, fit$varsel$ssize)
  cl_dend <- hc2arr(hc, fit$varsel$vind)

  stat_arr <- boot_stats(fit$varsel, 1:nv)
  proj <- project(fit, nv = seq_along(ch), ns = 100)

  pctch <- round(fit$varsel$pctch, 2) %>% as_tibble()
  pct <- get_pct_arr(pctch, nv)

  list(fit = fit, nv = nv, x = x, ch = ch, cl_2d = cl_2d, cl_dend = cl_dend,
       stat_arr = stat_arr, proj = proj, pctch = pctch, pct = pct)
}

get_pct_arr <- function(pctch, nv) {
  gather(pctch[1:nv, ], 'var', 'val', names(pctch)[1:nv+1], factor_key = T)
}

gen_vars_table <- function(pctch, sug) {
  arr <- pctch[sug, -1]
  op <- list(searching = F, paging = F, bInfo = F, ordering = F, autoWidth = T)
  sels <- matrix(c(rep(1, sug), 1:sug - 1), sug)
  capt <- tags$caption(style = 'caption-side: bottom; text-align: center;',
                       em('Selected variables'))
  col_brks <- with(get_col_brks(), styleInterval(breaks, pal))
  datatable(
    data = arr,
    options = op,
    class = 'compact',
    rownames = F,
    caption = capt,
    selection = list(mode = 'multiple', target = 'cell', selected = sels)) %>%
    formatStyle(names(arr), backgroundColor = col_brks)
}

proj_singles <- function(fit, x) {
  lapply(fit$varsel$vind, function(i) {
    proj_linpred(fit, x[, i, drop = F], transform = T, integrated = T,
                      vind = i, ns = 100)
  }) %>% as_tibble()
}

validate_varsel <- function(fit_cv) {
  !is.null(fit_cv$varsel$ssize)
}

.onAttach <- function(...) {
  ver <- utils::packageVersion("shinyproj")
  packageStartupMessage("This is shinyproj version ", ver)
}
