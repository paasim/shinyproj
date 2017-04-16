validate_varsel <- function(fit_cv) !is.null(fit_cv$varsel$ssize)

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

extract_data <- function(fit_cv) {
  x <- get_x(fit_cv)
  x <- x[, as.logical(attr(x, 'assign'))]
  dist <- 1-abs(cor(x))
  stat_arr <- boot_stats(fit_cv$varsel)
  ch <- with(fit_cv$varsel, setNames(chosen, chosen_names))
  nv <- length(fit_cv$varsel$chosen)
  brks <- get_breaks(nv)
  sug <- fit_cv$varsel$ssize
  stat_vals <- unique(stat_arr$stat)
  stat_def <- switch(fit_cv$family$family,
                         'gaussian' = 'mse', 'binomial' = 'pctcorr', 'mlpd')
  proj <- project(fit_cv, nv = 1:nv)
  pct <- round(as.data.frame(fit_cv$varsel$pctch), 2)
  styles <- c(setNames(vector('list', ncol(pct)), colnames(pct)) %>%
                sapply(function(x) formatter('span', style = x ~ style(
                  padding.left = '0px', padding.right = '0px'))),
              list(area(col = -size) ~ color_tile('#D55E00', '#009E73')))
  pct <- formattable(pct, styles)

  list(fit = fit_cv, x = x, dist = dist, stat_arr = stat_arr, brks = brks,
       ch = ch, nv = nv, sug = sug, stat_vals = stat_vals, stat_def = stat_def,
       proj = proj, pct = pct)
}

# quick placeholder for evaluation of mse
eval_stat <- function(active_proj, sel_proj, x, d_test, stat) {
  pa <- proj_linpred(active_proj, x, d_test$y, integrated = T)
  ps <- proj_linpred(sel_proj, x, d_test$y, integrated = T)
  bw <- boot_weights(length(d_test$y))
  sa <- calc_stats(pa$pred, pa$lpd, d_test, active_proj$family, bw)
  ss <- calc_stats(ps$pred, ps$lpd, d_test, sel_proj$family, bw)
  sa[[stat]] - ss[[stat]]
}

boot_stats <- function(vs, alpha = 0.05) {
  a <- alpha / 2
  n <- length(vs$summaries$full$mu)
  bw <- boot_weights(n)
  eq_w <- matrix(1/n, 1, n)

  arr <- do.call(rbind, lapply(vs$summaries$sub, function(x, f) {
    # bootstrapped quantiles of the difference
    b_dif <- mapply(function(bs, bf) quantile(bs-bf, c(a, 1-a), names = F),
                    calc_stats(x$mu, x$lppd, vs$d_test, vs$family, bw),
                    calc_stats(f$mu, f$lppd, vs$d_test, vs$family, bw))
    # exact value of the difference
    val <- unlist(calc_stats(x$mu, x$lppd, vs$d_test, vs$family_kl, eq_w)) -
      unlist(calc_stats(f$mu, f$lppd, vs$d_test, vs$family_kl, eq_w))
    data.frame(val, lq=b_dif[1,],  uq=b_dif[2,])
  }, vs$summaries$full))

  arr$cn <- sub('([a-z]+)(.*)' ,'\\1.0\\2', rownames(arr))
  separate(arr, cn, c('stat', 'size'), sep ='\\.', convert = T)
}

# borrowed from glmproj
calc_stats <- function (mu, lppd, d_test, family, sample_weights) {
  arr <- list(mlpd = lppd, mse = (d_test$y - mu)^2)
  if (family$family == "binomial" && all(d_test$weights %in% c(0, 1))) {
    arr$pctcorr <- round(mu) == d_test$y
  }
  avg_ <- function(x) c(sample_weights %*% x)
  lapply(arr, avg_)
}

# default theme for plots
theme_proj <- function() {
  bayesplot::theme_default() +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 18),
          strip.text = element_text(size = 18),
          strip.background = element_blank(),
          plot.title = element_text(face = 'bold', hjust = 0.6))
}

boot_weights <- function(n, n_boot = 1000) {
  bw <- matrix(rgamma(n*n_boot, 1), ncol = n)
  bw <- bw/rowSums(bw)
}

get_breaks <- function(nv) {
  n_opts <- c(4,5,6)
  n_possible <- Filter(function(x) nv %% x == 0, n_opts)
  n_alt <- n_opts[which.min(n_opts - (nv %% n_opts))]
  nb <- ifelse(length(n_possible) > 0, min(n_possible), n_alt)
  by <- ceiling(nv/min(nv, nb))
  breaks <- seq(0, by*min(nv, nb), by)
  minor_breaks <- if(by%%2 == 0)
    seq(by/2, by*min(nv, nb), by)
  else
    NULL
  list(breaks = breaks, minor_breaks = minor_breaks)
}

heat_style <- function() {
  styleInterval(seq(0, 1, length.out = 8), brewer.pal(9, 'RdYlGn'))
}
