% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdqr.R
\name{dsstd}
\alias{dsstd}
\alias{psstd}
\alias{qsstd}
\alias{rsstd}
\title{Skewed Student Distribution of Fernandez and Steel}
\usage{
dsstd(x, mu = 0, sigma = 1, skew = 1.5, shape = 5, log = FALSE)

psstd(
  q,
  mu = 0,
  sigma = 1,
  skew = 1.5,
  shape = 5,
  lower_tail = TRUE,
  log = FALSE
)

qsstd(
  p,
  mu = 0,
  sigma = 1,
  skew = 1.5,
  shape = 5,
  lower_tail = TRUE,
  log = FALSE
)

rsstd(n, mu = 0, sigma = 1, skew = 1.5, shape = 5)
}
\arguments{
\item{x, q}{vector of quantiles.}

\item{mu}{mean.}

\item{sigma}{standard deviation.}

\item{skew}{skew parameter.}

\item{shape}{shape parameter.}

\item{log}{(logical) if TRUE, probabilities p are given as log(p).}

\item{lower_tail}{if TRUE (default), probabilities are \eqn{P[X \le x]} otherwise, \eqn{P[X > x]}.}

\item{p}{vector of probabilities.}

\item{n}{number of observations.}
}
\value{
d gives the density, p gives the distribution function, q gives the quantile function
and r generates random deviates. Output depends on x or q length, or n for the random number
generator.
}
\description{
Density, distribution, quantile function and random number
generation for the skewed student distribution parameterized in 
terms of mean, standard deviation, skew and shape parameters.
}
