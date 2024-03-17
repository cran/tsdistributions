## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tsdistributions)
# simulate data
set.seed(101)
sim <- rgh(3000, mu = 0, sigma = 1, skew = -0.8, shape = 4, lambda = 1)
spec <- distribution_modelspec(sim, distribution = "gh")
# fix lambda to value and set it to non-estimate
spec$parmatrix[parameter == "lambda", value := 1.0]
spec$parmatrix[parameter == "lambda", estimate := 0]
mod <- estimate(spec, use_hessian = FALSE)
summary(mod, vcov_type = "QMLE")

## -----------------------------------------------------------------------------
tsmoments(mod)

