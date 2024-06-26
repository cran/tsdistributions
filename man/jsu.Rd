% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdqr.R
\name{djsu}
\alias{djsu}
\alias{pjsu}
\alias{qjsu}
\alias{rjsu}
\title{Johnson's SU Distribution}
\usage{
djsu(x, mu = 0, sigma = 1, skew = 1, shape = 0.5, log = FALSE)

pjsu(
  q,
  mu = 0,
  sigma = 1,
  skew = 1,
  shape = 0.5,
  lower_tail = TRUE,
  log = FALSE
)

qjsu(
  p,
  mu = 0,
  sigma = 1,
  skew = 1,
  shape = 0.5,
  lower_tail = TRUE,
  log = FALSE
)

rjsu(n, mu = 0, sigma = 1, skew = 1, shape = 0.5)
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
generation for Johnson's SU distribution parameterized in 
terms of mean, standard deviation, skew and shape parameters.
}
