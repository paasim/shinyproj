# - Functions for evaluating mse/mlpd/pctcorr/etc.

eval_stat <- function(active_proj, sel_proj, x, d_test, stat) {
  pa <- proj_linpred(active_proj, x, d_test$y, integrated = T)
  ps <- proj_linpred(sel_proj, x, d_test$y, integrated = T)
  bw <- boot_weights(length(d_test$y))
  calc_stats(pa$pred, pa$lpd, d_test, active_proj$family, bw)[[stat]] -
    calc_stats(ps$pred, ps$lpd, d_test, sel_proj$family, bw)[[stat]]
}

boot_weights <- function(n, n_boot = 1000) {
  bw <- matrix(rgamma(n*n_boot, 1), ncol = n)
  bw <- bw/rowSums(bw)
}

boot_stats <- function(vs, nvs, alpha = 0.1) {
  a <- alpha / 2
  n <- length(vs$summaries$full$mu)
  bw <- boot_weights(n)
  eq_w <- matrix(1/n, 1, n)
  sub <- vs$summaries$sub
  full <- vs$summaries$full
  if (is.null(nvs)) nvs <- 1:(length(sub)-1)
  sub <- sub[nvs+1]
  stat_f <- with(vs$summaries$full, list(
    boot = calc_stats(mu, lppd, vs$d_test, vs$family, bw),
    val = calc_stats(mu, lppd, vs$d_test, vs$family, eq_w)
  ))

  lapply(sub, function(x) {
    # bootstrapped quantiles of the difference
    d <- mapply(function(bs, bf) quantile(bs-bf, c(a, 1-a), names = F),
                calc_stats(x$mu, x$lppd, vs$d_test, vs$family, bw), stat_f$boot)
    # exact value of the difference
    val <- calc_stats(x$mu, x$lppd, vs$d_test, vs$family_kl, eq_w) - stat_f$val

    tibble(stat = names(val), val = unlist(val), lq = d[1, ],  uq = d[2, ])
  }) %>% bind_rows(.id = "size") %>% within(size <- as.integer(size))
}

calc_stats <- function (mu, lppd, d_test, family, sample_weights) {
  arr <- list(mlpd = lppd, mse = (d_test$y - mu)^2)
  if (family$family == "binomial" && all(d_test$weights %in% c(0, 1))) {
    arr$pctcorr <- round(mu) == d_test$y
  }
  avg_ <- function(x) c(sample_weights %*% x)
  lapply(arr, avg_) %>% as_tibble()
}
