\name{polyCub.midpoint}
\alias{polyCub.midpoint}
\title{Two-Dimensional Midpoint Rule}
\usage{
polyCub.midpoint(polyregion, f, ..., eps = NULL, dimyx = NULL,
  plot = FALSE)
}
\arguments{
  \item{polyregion}{a polygonal integration domain. It can
  be any object coercible to the \pkg{spatstat} class
  \code{"\link[spatstat]{owin}"} (via
  \code{\link[spatstat]{as.owin}}).}

  \item{f}{two-dimensional function to be integrated. As
  its first argument the function must take a coordinate
  matrix, i.e. a numeric matrix with two columns.}

  \item{...}{further arguments for \code{f}.}

  \item{eps}{width and height of the pixels (squares), see
  \code{\link[spatstat]{as.mask}}.}

  \item{dimyx}{number of subdivisions in each dimension,
  see \code{\link[spatstat]{as.mask}}.}

  \item{plot}{logical indicating if an illustrative plot of
  the numerical integration should be produced.}
}
\value{
The approximated value of the integral of \code{f} over
\code{polyregion}.
}
\description{
The surface is converted to a binary pixel image using the
\code{\link[spatstat]{as.im.function}} method from package
\pkg{spatstat}. The integral under the surface is then
approximated as the sum over (pixel area * f(pixel
midpoint)).
}
\examples{
# see example(polyCub)
}
\references{
A. Baddeley and R. Turner (2005). Spatstat: an R package
for analyzing spatial point patterns. Journal of
Statistical Software 12 (6), 1-42.
}
\seealso{
Other polyCub.methods: \code{\link{.polyCub.iso}},
\code{\link{polyCub.iso}}; \code{\link{polyCub.SV}};
\code{\link{polyCub.exact.Gauss}}; \code{\link{polyCub}}
}
\keyword{math}
\keyword{spatial}

