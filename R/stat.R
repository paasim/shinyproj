# - functions for evaluating mse/mlpd/pctcorr/etc.

eval_stat <- function(active_proj, sel_proj, x, d_test, stat) {
  pa <- proj_linpred(active_proj, x, d_test$y, integrated = T)
  ps <- proj_linpred(sel_proj, x, d_test$y, integrated = T)
  bw <- boot_weights(length(d_test$y))
  sa <- calc_stats(pa$pred, pa$lpd, d_test, active_proj$family, bw)
  ss <- calc_stats(ps$pred, ps$lpd, d_test, sel_proj$family, bw)
  sa[[stat]] - ss[[stat]]
}

#' @importFrom stats rgamma
boot_weights <- function(n, n_boot = 1000) {
  bw <- matrix(rgamma(n*n_boot, 1), ncol = n)
  bw <- bw/rowSums(bw)
}

#' @importFrom tidyr separate_
#' @importFrom stats quantile
boot_stats <- function(vs, nvs = NULL, alpha = 0.05) {
  a <- alpha / 2
  n <- length(vs$summaries$full$mu)
  bw <- boot_weights(n)
  eq_w <- matrix(1/n, 1, n)
  sub <- vs$summaries$sub
  if(!is.null(nvs))
    sub <- sub[nvs+1]

  arr <- do.call(rbind, lapply(sub, function(x, f) {
    # bootstrapped quantiles of the difference
    b_dif <- mapply(function(bs, bf) quantile(bs-bf, c(a, 1-a), names = F),
                    calc_stats(x$mu, x$lppd, vs$d_test, vs$family, bw),
                    calc_stats(f$mu, f$lppd, vs$d_test, vs$family, bw))
    # exact value of the difference
    val <- unlist(calc_stats(x$mu, x$lppd, vs$d_test, vs$family_kl, eq_w)) -
      unlist(calc_stats(f$mu, f$lppd, vs$d_test, vs$family_kl, eq_w))
    data.frame(val, lq = b_dif[1, ],  uq = b_dif[2, ])
  }, vs$summaries$full))

  arr$cn <- sub('([a-z]+)(.*)' ,'\\1.0\\2', rownames(arr))
  separate_(arr, 'cn', c('stat', 'size'), sep ='\\.', convert = T)
}

calc_stats <- function (mu, lppd, d_test, family, sample_weights) {
  arr <- list(mlpd = lppd, mse = (d_test$y - mu)^2)
  if (family$family == 'binomial' && all(d_test$weights %in% c(0, 1))) {
    arr$pctcorr <- round(mu) == d_test$y
  }
  avg_ <- function(x) c(sample_weights %*% x)
  lapply(arr, avg_)
}
