\name{polyCub.SV}
\alias{polyCub.SV}
\title{Product Gauss Cubature over Polygonal Domains}
\usage{
polyCub.SV(polyregion, f, ..., nGQ = 20, alpha = NULL, rotation = FALSE,
  plot = FALSE)
}
\arguments{
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

  \item{...}{further arguments for \code{f}.}

  \item{nGQ}{degree of the one-dimensional Gauss-Legendre
  quadrature rule (default: 20). See
  \code{\link[statmod]{gauss.quad}} in package
  \pkg{statmod}, on which this function depends.}

  \item{plot}{logical indicating if an illustrative plot of
  the numerical integration should be produced.}

  \item{alpha}{base-line of the (rotated) polygon at \eqn{x
  = \alpha} (see Sommariva and Vianello (2007) for an
  explication). If \code{NULL} (default), the midpoint of
  the x-range of the polygon is chosen if no
  \code{rotation} is performed, and otherwise the
  \eqn{x}-coordinate of the rotated point \code{"P"}. If
  \code{f} has its maximum value at the origin \eqn{(0,0)},
  e.g., the bivariate Gaussian density with zero mean,
  \code{alpha = 0} is a reasonable choice.}

  \item{rotation}{logical (default: \code{FALSE}) or a list
  of points \code{"P"} and \code{"Q"} describing the
  preferred direction. If \code{TRUE}, the polygon is
  rotated according to the vertices \code{"P"} and
  \code{"Q"}, which are farthest apart (see Sommariva and
  Vianello, 2007). For convex polygons, this rotation
  guarantees that all nodes fall inside the polygon.}
}
\value{
The approximated value of the integral of \code{f} over
\code{polyregion}.
}
\description{
Product Gauss cubature over polygons as proposed by
Sommariva and Vianello (2007).
}
\examples{
# see example(polyCub)
}
\author{
Sebastian Meyer\cr The product Gauss cubature is based on
the original \acronym{MATLAB} implementation
\code{polygauss} by Sommariva and Vianello (2007), which is
available under the GNU GPL (>=2) license from
\url{http://www.math.unipd.it/~alvise/software.html}.
}
\references{
Sommariva, A. and Vianello, M. (2007). Product Gauss
cubature over polygons based on Green's integration
formula. \emph{Bit Numerical Mathematics}, \bold{47} (2),
441-453.
}
\seealso{
Other polyCub.methods: \code{\link{.polyCub.iso}},
\code{\link{polyCub.iso}};
\code{\link{polyCub.exact.Gauss}};
\code{\link{polyCub.midpoint}}; \code{\link{polyCub}}
}
\keyword{math}
\keyword{spatial}

