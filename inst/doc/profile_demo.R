## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tsdistributions)
library(future)
library(progressr)
plan(list(
    tweak(sequential),
    tweak(multisession, workers = 1)
))
# tracing using the progressr package

# handlers(global = TRUE)
# handlers("progress")

# set up some dummy data to establish the specification
spec <- distribution_modelspec(rnorm(100), distribution = "std")
# make sure to set all parameter values. The mu and sigma are otherwise defaulted
# to the mean and standard deviation of the data input.
spec$parmatrix[parameter %in% c("mu","sigma","shape"), value := c(0.0, 1.0, 5.0)]
sim <- tsprofile(spec, nsim = 100, sizes = c(400, 1000, 2000), seed = 100, trace = FALSE)
plan("sequential")
summary(sim)

