\name{polygauss}
\alias{polygauss}
\title{Calculate 2D Nodes and Weights of the Product Gauss Cubature}
\usage{
polygauss(xy, nw_MN, alpha = NULL, rotation = FALSE)
}
\arguments{
  \item{xy}{list with elements \code{"x"} and \code{"y"}
  containing the polygon vertices in \emph{anticlockwise}
  order (otherwise the result of the cubature will have a
  negative sign) with first vertex not repeated at the end
  (like \code{owin.object$bdry}).}

  \item{nw_MN}{unnamed list of nodes and weights of
  one-dimensional Gauss quadrature rules of degrees \eqn{N}
  and \eqn{M=N+1} (as returned by
  \code{\link[statmod]{gauss.quad}}): \code{list(s_M, w_M,
  s_N, w_N)}.}

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
\description{
Calculate 2D Nodes and Weights of the Product Gauss
Cubature
}
\references{
Sommariva, A. and Vianello, M. (2007): Product Gauss
cubature over polygons based on Green's integration
formula. \emph{Bit Numerical Mathematics}, \bold{47} (2),
441-453.
}
\keyword{internal}

