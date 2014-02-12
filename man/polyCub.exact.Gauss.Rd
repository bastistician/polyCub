\name{polyCub.exact.Gauss}
\alias{polyCub.exact.Gauss}
\title{Quasi-Exact Cubature of the Bivariate Normal Density}
\usage{
polyCub.exact.Gauss(polyregion, mean = c(0, 0), Sigma = diag(2),
  plot = FALSE)
}
\arguments{
  \item{polyregion}{a
  \code{"\link[rgeos:gpc.poly-class]{gpc.poly}"} polygon or
  something that can be coerced to this class, e.g., an
  \code{"owin"} polygon (converted via
  \code{\link{owin2gpc}} and -- given \pkg{rgeos} is
  available -- \code{"SpatialPolygons"} also work.}

  \item{mean,Sigma}{mean and covariance matrix of the
  bivariate normal density to be integrated.}

  \item{plot}{logical indicating if an illustrative plot of
  the numerical integration should be produced. Note that
  the \code{polyregion} will be shifted and scaled.}
}
\value{
The integral of the bivariate normal density over
\code{polyregion}. Two attributes are appended to the
integral value: \item{nEval}{ number of triangles over
which the standard bivariate normal density had to be
integrated, i.e. number of calls to
\code{\link[mvtnorm]{pmvnorm}} and
\code{\link[stats]{pnorm}}, the former of which being the
most time-consuming operation. } \item{error}{ Approximate
absolute integration error steming from the error
introduced by the \code{nEval}
\code{\link[mvtnorm]{pmvnorm}} evaluations. For this
reason, the cubature method is in fact only quasi-exact (as
is the \code{pmvnorm} function). }
}
\description{
Integration is based on triangulation of the polygonal
domain and formulae from Chapter 26 of the Abramowitz &
Stegun handbook (Section 26.9, Example 9, pp. 956f.). This
method is quite cumbersome because the A&S formula is only
for triangles where one vertex is the origin (0,0). For
each triangle of the \code{\link[gpclib]{tristrip}} we have
to check in which of the 6 outer regions of the triangle
the origin (0,0) lies and adapt the signs in the formula
appropriately. (AOB+BOC-AOC) or (AOB-AOC-BOC) or
(AOB+AOC-BOC) or (AOC+BOC-AOB) or \ldots. However, the most
time consuming step is the evaluation of
\code{\link[mvtnorm]{pmvnorm}}.
}
\note{
The package \pkg{gpclib} (which is required to produce the
\code{tristrip}, since this is not yet implemented in
\pkg{rgeos}) has a restricted license (commercial use
prohibited). It has to be accepted explicitly via
\code{\link{gpclibPermit}()} prior to using
\code{polyCub.exact.Gauss}.
}
\examples{
# see example(polyCub)
}
\references{
M. Abramowitz and I. A. Stegun (1970). Handbook of
Mathematical Functions with Formulas, Graphs, and
Mathematical Tables (9th ed.). New York: Dover
Publications.
}
\seealso{
\code{\link{circleCub.Gauss}} for quasi-exact cubature of
the isotropic Gaussian density over a circular domain.

Other polyCub.methods: \code{\link{.polyCub.iso}},
\code{\link{polyCub.iso}}; \code{\link{polyCub.SV}};
\code{\link{polyCub.midpoint}}; \code{\link{polyCub}}
}
\keyword{math}
\keyword{spatial}

