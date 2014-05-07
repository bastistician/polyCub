\name{polyCub.iso}
\alias{.polyCub.iso}
\alias{polyCub.iso}
\title{Cubature of Isotropic Functions over Polygonal Domains}
\usage{
polyCub.iso(polyregion, f, intrfr, ..., center, control = list(),
  check.intrfr = FALSE, plot = FALSE)

.polyCub.iso(polys, intrfr, ..., center, control = list(),
  .witherror = FALSE)
}
\arguments{
  \item{intrfr}{analytical antiderivative of \eqn{r f_r(r)}
  from 0 to \code{R} (first argument, not necessarily named
  \code{"R"}, must be vectorized). If missing,
  \code{intrfr} is approximated numerically using
  \code{\link{integrate}} configured with \code{control}.}

  \item{...}{further arguments for \code{f} or
  \code{intrfr}.}

  \item{center}{numeric vector of length 2, the center of
  isotropy.}

  \item{control}{list of arguments passed to
  \code{\link{integrate}}, the quadrature rule used for the
  line integral along the polygon boundary.}

  \item{check.intrfr}{logical (or numeric vector)
  indicating if (for which \code{r}'s) the supplied
  \code{intrfr} function should be checked against a
  numeric approximation. This check requires \code{f} to be
  specified. If \code{TRUE}, the set of test \code{r}'s
  defaults to a \code{\link{seq}} of length 20 from 1 to
  the maximum absolute x or y coordinate of any edge of the
  \code{polyregion}.}

  \item{polys}{something like \code{owin$bdry}, but see
  \code{\link{xylist}}.}

  \item{.witherror}{logical indicating if an upperbound for
  the absolute integration error should be attached as an
  attribute to the result?}

  \item{polyregion}{a polygonal integration domain. The
  following classes are supported:
  \code{"\link[spatstat]{owin}"},
  \code{"\link[rgeos:gpc.poly-class]{gpc.poly}"},
  \code{"\linkS4class{SpatialPolygons}"},
  \code{"\linkS4class{Polygons}"}, and
  \code{"\linkS4class{Polygon}"} (for these we have an
  internal \code{\link{xylist}} method).}

  \item{f}{two-dimensional function. As its first argument
  it must take a coordinate matrix, i.e. a numeric matrix
  with two columns.}

  \item{plot}{logical indicating if an illustrative plot of
  the numerical integration should be produced.}
}
\value{
The approximate integral of the isotropic function \code{f}
over \code{polyregion}.\cr If the \code{intrfr} function is
provided (which is assumed to be exact), an upperbound for
the absolute integration error is appended to the result as
attribute \code{"abs.error"}. It equals the sum of the
absolute errors reported by all \code{\link{integrate}}
calls (there is one for each edge of \code{polyregion}).
}
\description{
Conducts numerical integration of a two-dimensional
isotropic function \eqn{f(x,y) =
f_r(||(x,y)-\boldsymbol{\mu}||)}{f(x,y) =
f_r(||(x,y)-\mu||)}, with \eqn{\mu} being the center of
isotropy, over a polygonal domain. It internally solves a
line integral along the polygon boundary using
\code{\link{integrate}} where the integrand requires the
antiderivative of \eqn{r f_r(r)}), which ideally is
analytically available and supplied to the function as
argument \code{intrfr}. The two-dimensional integration
problem thereby reduces to an efficient adaptive quadrature
in one dimension. See Meyer and Held (2014, Section 2.4 of
Supplement B) for mathematical details.

\code{.polyCub.iso} is a \dQuote{bare-bone} version of
\code{polyCub.iso}.
}
\examples{
## we use the example polygon and f (exponential decay) from
example(plotpolyf)

## numerical approximation of 'intrfr'
(intISOnum <- polyCub.iso(letterR, f, center=fcenter))

## analytical 'intrfr' (recall: f_r(r)=dexp(r), we need int_0^R r*f(r) dr)
intrfr <- function (R, rate=1) pgamma(R, 2, rate) / rate
(intISOana <- polyCub.iso(letterR, intrfr=intrfr, center=fcenter))

stopifnot(all.equal(intISOana, intISOnum, check.attributes=FALSE))


### polygon area: f(r) = 1, f(x,y) = 1, center does not really matter

intrfr.const <- function (R) R^2/2
(area.ISO <- polyCub.iso(letterR, intrfr=intrfr.const, center=c(0,0)))

stopifnot(all.equal(spatstat::area.owin(letterR),
                    area.ISO,
                    check.attributes=FALSE))
## the hole is subtracted correctly
}
\author{
Sebastian Meyer

The basic mathematical formulation of this efficient
integration for radially symmetric functions was
ascertained with great support by Emil Hedevang (2013),
Dept. of Mathematics, Aarhus University, Denmark.
}
\references{
Hedevang, E. (2013). Personal communication at the Summer
School on Topics in Space-Time Modeling and Inference (May
2013, Aalborg, Denmark).

Meyer, S. and Held, L. (2014). Power-law models for
infectious disease spread. \emph{Annals of Applied
Statistics}. In press, available as
\href{http://arxiv.org/abs/1308.5115}{arXiv:1308.5115}.
Supplements are available from
\url{http://www.biostat.uzh.ch/research/manuscripts/powerlaw.html}.
}
\seealso{
Other polyCub.methods: \code{\link{polyCub.SV}};
\code{\link{polyCub.exact.Gauss}};
\code{\link{polyCub.midpoint}}; \code{\link{polyCub}}
}
\keyword{math}
\keyword{spatial}

