# - Miscellanneous functions

extract_data <- function(fit, nv) {
  x <- matrix(get_x(fit), nrow = nobs(fit))
  if (attr(fit$terms, 'intercept')) x <- x[, -1, drop = FALSE]
  ch <- fit$varsel$vind

  dfun <- function(x) 1 - cor(x)
  proj_dist <- proj_singles(fit, x) %>% dfun() %>% as.dist()
  hc <- hclust(proj_dist)
  nc_2d <- min(fit$varsel$ssize, 5) # at most 5 clusters
  cl_2d <- hc_to_clusters(hc, proj_dist, fit$varsel$vind, nc_2d)
  cl_dend <- hc2arr(hc, fit$varsel$vind)

  stat_arr <- boot_stats(fit$varsel, 1:nv, alpha = 0.1)
  proj <- project(fit, nv = seq_along(ch), ns = 100)

  pctch <- round(fit$varsel$pctch, 2)
  colnames(pctch)[1] <- ".size"
  pct <- get_pct_arr(pctch, nv)

  list(fit = fit, nv = nv, x = x, ch = ch, cl_2d = cl_2d, cl_dend = cl_dend,
       stat_arr = stat_arr, proj = proj, pctch = pctch, pct = pct)
}

get_pct_arr <- function(pctch, nv) {
  as_tibble(pctch[1:nv, 1:(nv + 1)]) %>%
    gather("var", "val", colnames(pctch)[1:nv+1], factor_key = T)
}

gen_vars_table <- function(pctch, size, transpose) {
  arr <- pctch[size, -1, drop = FALSE]
  rownames(arr) <- NULL

  opt <- list(searching = FALSE, paging = FALSE, bInfo = FALSE,
              ordering = FALSE, autoWidth = TRUE)
  col_brks <- with(get_col_brks(), styleInterval(breaks, pal))
  cb <- get_sel_callback()
  capt <- em("Select variables by clicking the corresponding columns") %>%
    tags$caption(style = "caption-side: bottom; text-align: center;")

  if (transpose) arr <- t(arr)

  datatable(arr, opt, "compact", cb, caption = capt, selection = "none") %>%
    formatStyle(colnames(arr), backgroundColor = col_brks) %>%
    formatStyle(colnames(arr), cursor = "pointer")
}

proj_singles <- function(fit, x) {
  lapply(fit$varsel$vind, function(i) {
    proj_linpred(fit, x[, i, drop = F], transform = T, integrated = T,
                      vind = i, ns = 100)
  }) %>% as_tibble()
}

get_selector <- function(inds = NULL) {
  sel <- "$(DataTables_Table_0_wrapper).find('td')"
  if (!is.null(inds)) sel <- paste0(sel, ".eq(", inds - 1, ")")
  sel
}

get_sel_callback <- function() {
  JS("table.on('click.dt', 'td',
        function() {
          var row = table.cell(this).index().row;
          var col = table.cell(this).index().column;
          Shiny.onInputChange('vars_click', [row, col, Math.random()]);
     });")
}

get_css_settings <- function() {
  "
  td.dt-right.selected-custom {
    border: 2px solid black;
  }
  table thead th {
    padding: 50px 0px 0px 0px !important;
    transform: rotate(315deg) translate(-10px, -8px);
    -webkit-transform: rotate(315deg) translate(-10px, -8px);
    -ms-transform: rotate(315deg) translate(-10px, -8px);
    table-layout: fixed !important;
    width: 35px !important;
    border-bottom: 0px !important;
  }
  table {
    width: 0px !important;
    table-layout: fixed !important;
  }
  .main-sidebar {
    background-color: #ECF0F5 !important;
  }
  #global img {
    cursor : pointer;
  }
  "
}

log_event <- function(cur, new, type) {
  .GlobalEnv$.shinyproj_logs %<>%
    bind_rows(tibble(cur = list(cur), new = list(new),
                     type = type, time = date()))
  NULL
}

validate_varsel <- function(fit_cv) {
  !is.null(fit_cv$varsel$ssize)
}

empty_numeric <- function() c("a" = 0)[-1]

.onAttach <- function(...) {
  ver <- utils::packageVersion("shinyproj")
  packageStartupMessage("This is shinyproj version ", ver)
}
