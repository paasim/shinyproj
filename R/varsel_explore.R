#' Interactive variable selection using glmproj
#'
#' Visualise the results of the variable selection using shiny.
#'
#' @param fit_cv An object returned by \link[=cv_varsel]{cv_varsel}.
#' @param nv Maximum number of variables in the submodel. Defaults to
#' \code{min(12, length(fit_cv$varsel$chosen))}.
#'

#' @import projpred
#' @importFrom magrittr "%>%"
#' @importFrom shiny runApp
#' @export
varsel_explore <- function(fit_cv, nv = min(12, length(fit_cv$varsel$chosen))) {

  # check that cv_varsel has been run and perform the projection
  if(!validate_varsel(fit_cv))
    stop('Input does not contain cross-validated variable selection information.')

  server_data <- extract_data(fit_cv, nv)

  app <- list(ui = get_ui(), server = get_server(server_data))
  runApp(app)
}
