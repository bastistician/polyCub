\name{checkintrfr}
\alias{checkintrfr}
\title{Check the Integral of \eqn{r f_r(r)}}
\usage{
checkintrfr(intrfr, f, ..., center, control = list(), rs = numeric(0L),
  tolerance = control$rel.tol)
}
\arguments{
  \item{rs}{numeric vector of upper bounds for which to
  check the validity of \code{intrfr}. If it has length 0,
  no checks are performed.}

  \item{tolerance}{of \code{\link{all.equal.numeric}} when
  comparing \code{intrfr} results with numerical
  integration. Defaults to the relative tolerance used for
  \code{integrate}.}

  \item{intrfr}{analytical antiderivative of \eqn{r f_r(r)}
  from 0 to \code{R} (first argument, not necessarily named
  \code{"R"}, must be vectorized). If missing,
  \code{intrfr} is approximated numerically using
  \code{\link{integrate}} configured with \code{control}.}

  \item{f}{two-dimensional function. As its first argument
  it must take a coordinate matrix, i.e. a numeric matrix
  with two columns.}

  \item{...}{further arguments for \code{f} or
  \code{intrfr}.}

  \item{center}{numeric vector of length 2, the center of
  isotropy.}

  \item{control}{list of arguments passed to
  \code{\link{integrate}}, the quadrature rule used for the
  line integral along the polygon boundary.}
}
\value{
The \code{intrfr} function. If it was not supplied, its
quadrature version using \code{integrate} is returned.
}
\description{
This function is auxiliary to \code{\link{polyCub.iso}}.
The (analytical) integral of \eqn{r f_r(r)} from 0 to
\eqn{R} is checked against a numeric approximation using
\code{\link{integrate}} for various values of the upper
bound \eqn{R}. A warning is issued if inconsistencies are
found.
}

