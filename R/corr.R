# - Functions related to correlation between the variables, clustering etc.

proj_dists <- function(projs, distfun) {
  d_mat <- matrix(NA, length(projs), length(projs), dimnames = list(names(projs), names(projs)))
  sapply(projs, function(p1) {
    sapply(projs, function(p2) {
      distfun(p1, p2)
    })
  })
}

hc2arr <- function(hc, vind) {
  n <- length(hc$height)
  mat <- matrix(0, n, 5, dimnames = list(NULL, c('x1', 'y1', 'x2', 'y2', 'yu')))
  inds <- replicate(n, integer())
  hc_ord <- order(hc$order)
  vind_ord <- vind[hc$order]

  fneg <- function(x) list(c(hc_ord[x], 0), vind_ord[hc_ord[x]])
  fpos <- function(x) list(c(mean(mat[x, c('x1', 'x2')]), mat[x, 'yu']), inds[[x]])

  for (i in 1L:n) {
    ln <- lapply(hc$merge[i, ], function(x) if (x < 0) fneg(-1*x) else fpos(x))
    mat[i, ] <- c(ln[[1]][[1]], ln[[2]][[1]], hc$height[i])
    inds[[i]] <- c(ln[[1]][[2]], ln[[2]][[2]])
  }

  lines <- lapply(1:nrow(mat), function(i)
    tibble(x1 = mat[i, c('x1', 'x2', 'x1')], x2 = mat[i, c('x1', 'x2', 'x2')],
           y1 = mat[i, c('y1', 'y2', 'yu')], y2 = mat[i, rep('yu', 3)],
           inds = list(NULL, NULL, inds[[i]]))) %>% bind_rows()
  list(lines = lines, labs = vind[hc$order])
}

hc_to_sets <- function(hc, k) {
  n <- length(hc$order)
  k_vals <- c(k, max(floor(k/2), 2))
  grp <- cutree(hc, k_vals)
  setsfun <- function(i, grp) {
    inds <- which(grp[, i] %in% which(table(grp[, i]) > 1))
    split(inds, grp[inds, i])
  }
  sets <- lapply(seq_along(k_vals), setsfun, grp) %>%
    unlist(recursive = F, use.names = F) %>% unique()

  heightfun <- function(inds, merge, height, n) {
    i <- ((which(merge %in% -inds) - 1) %% (n - 1)) + 1
    max(height[i])
  }
  dist_sets <- sapply(sets, heightfun, hc$merge, hc$height, n)
  missing <- as.list(setdiff(1:n, unlist(sets)))

  list(inds = c(sets, missing), dists = c(dist_sets, rep(0, length(missing))))
}

hc_to_clusters <- function(hc, dists, vind, k) {
  cmd <- cmdscale(dists)
  sets <- hc_to_sets(hc, k)

  # find the min distance between the clusters
  ids <- combn(1:nrow(cmd), 2, simplify = FALSE) %>%
    vapply(function(x, set) {
      # are the sets in which x[1] and x[2] belong to identical
      any(vapply(set, function(s) (x[1] %in% s) - (x[2] %in% s), integer(1)))
    }, logical(1), sets$inds)
  r <- min(c(dists)[ids])

  pts <- tibble(x = cmd[, 1], y = cmd[, 2], ind = vind, lab = rownames(cmd))
  hulls <- lapply(sets$inds, function(rows) {
    circ <- circle(cmd[rows, , drop = F], r/3)
    as_tibble(rbind(circ, circ[1, ])) %>% setNames(c('x', 'y'))
  }) %>% bind_rows(.id = "grp")

  hulls$sim <- 1 - sets$dists[as.numeric(hulls$grp)]
  inds <- lapply(sets$inds, function(x) vind[x])

  list(pts = pts, hulls = hulls, inds = inds, order = hc$order)
}

pairs_fun <- function(x, sel) {
  combn(sel, 2, simplify = F) %>%
    lapply(function(i)
      tibble(x1 = x[, i[1]], x2 = x[, i[2]],
             n1 = rep(x = names(i)[1], nrow(x)),
             n2 = rep(x = names(i)[2], nrow(x)))
    ) %>% bind_rows()
}
