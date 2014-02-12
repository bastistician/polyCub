\name{owin2gpc}
\alias{gpc2owin}
\alias{owin2gpc}
\title{Conversion from and to the \code{"gpc.poly"} Class}
\usage{
owin2gpc(object)

gpc2owin(object)
}
\arguments{
  \item{object}{an object of class \code{"gpc.poly"} or
  \code{"owin"}, respectively.}
}
\value{
The converted polygon of class \code{"gpc.poly"} or
\code{"owin"}, respectively. If neither package \pkg{rgeos}
nor \pkg{gpclib} are available, \code{owin2gpc} will just
return the \code{pts} slot of the \code{"gpc.poly"} (no
formal class) with a warning.
}
\description{
Package \pkg{polyCub} implements converters between the
classes \code{"\link[spatstat:owin.object]{owin}"} of
package \pkg{spatstat} and
\code{"\link[rgeos:gpc.poly-class]{gpc.poly}"} of package
\pkg{rgeos} (originally from \pkg{gpclib}).
}
\note{
The converter to \code{"gpc.poly"} requires the \pkg{rgeos}
(or \pkg{gpclib}) package for the formal class definition.
It will produce vertices ordered according to the \pkg{sp}
convention, i.e. clockwise for normal boundaries and
anticlockwise for holes, where, however, the first vertex
is \emph{not} repeated!
}
\author{
Sebastian Meyer\cr The converters are slightly modified
versions of the same functions in \pkg{spatstat} version
1.33-0, authored by Adrian Baddeley and Rolf Turner. (Note
that support for the \code{"gpc.poly"} class was dropped
from \pkg{spatstat} as of version 1.34-0.)
}
\seealso{
\code{\link{xylist}}\cr Conversions of \code{"gpc.poly"}
objects from and to the
\code{"\link[sp:SpatialPolygons-class]{SpatialPolygons}"}
class of package \pkg{sp} are available in the \pkg{rgeos}
package.
}
\keyword{methods}
\keyword{spatial}

