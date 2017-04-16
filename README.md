# shinyproj

An R package for interactive model selection using [glmproj][].

Installation
------------

    # shinyproj depends on the development version of glmproj
    devtools::install_github('paasim/glmproj', ref = 'development')
    devtools::install_github('paasim/shinyproj')


Usage
-----

    library(rstanarm)
    library(glmproj)
    library(shinyproj)
    options(mc.cores = parallel::detectCores())
    
    # diabetes data set from http://web.stanford.edu/~hastie/Papers/LARS/
    data('df_diabetes', package = 'shinyproj')
    
    # sparsifying prior
    n <- nrow(df_diabetes)
    D <- ncol(df_diabetes) - 1
    # prior guess for the number of relevant variables
    p0 <- 3
    # scale for tau (stan_glm will automatically scale this by sigma)
    tau0 <- p0/(D-p0) * 1/sqrt(n)
    prior_coeff <- hs(df = 1, global_df = 1, global_scale = tau0)
    
    # fit the full model with rstanarm using the sparsifying prior
    fit <- stan_glm(y ~ ., data = df_diabetes, prior = prior_coeff)

    # perform the cross-validated variable selection
    fit_cv <- cv_varsel(fit)

    # explore the results
    varsel_explore(fit_cv)



[glmproj]:  https://github.com/paasim/glmproj

