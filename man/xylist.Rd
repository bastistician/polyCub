\name{xylist}
\alias{xylist}
\alias{xylist.Polygon}
\alias{xylist.Polygons}
\alias{xylist.SpatialPolygons}
\alias{xylist.default}
\alias{xylist.gpc.poly}
\alias{xylist.owin}
\title{Convert Various Polygon Classes to a Simple List of Vertices}
\usage{
xylist(object, ...)

\method{xylist}{owin}(object, ...)

\method{xylist}{gpc.poly}(object, ...)

\method{xylist}{SpatialPolygons}(object, reverse = TRUE, ...)

\method{xylist}{Polygons}(object, reverse = TRUE, ...)

\method{xylist}{Polygon}(object, reverse = TRUE, ...)

\method{xylist}{default}(object, ...)
}
\arguments{
  \item{object}{an object of one of the supported spatial
  classes.}

  \item{...}{(unused) argument of the generic.}

  \item{reverse}{logical (\code{TRUE}) indicating if the
  vertex order of the \pkg{sp} classes should be reversed
  to get the \code{xylist}/\code{owin} convention.}
}
\value{
Applying \code{xylist} to a polygon object, one gets a
simple list, where each component (polygon) is a list of
\code{"x"} and \code{"y"} coordinates. These represent
vertex coordinates following \pkg{spatstat}'s \code{"owin"}
convention (anticlockwise order without repeating any
vertex). The opposite vertex order can be retained for the
\pkg{sp}-classes by the non-default use with
\code{reverse=FALSE}.\cr
}
\description{
Different packages concerned with spatial data use
different polygon specifications, which sometimes becomes
very confusing (see Details below). To be compatible with
the various polygon classes, package \pkg{polyCub} uses an
S3 class \code{"xylist"}, which represents polygons by
their core feature only, a list of lists of vertex
coordinates (see the "Value" section below). The generic
function \code{xylist} can deal with the following polygon
classes: \itemize{
\item{\code{"\link[spatstat:owin.object]{owin}"} from
package \pkg{spatstat}}
\item{\code{"\link[rgeos:gpc.poly-class]{gpc.poly}"} from
package \pkg{rgeos} (or \pkg{gpclib})}
\item{\code{"\link[sp:Polygons-class]{Polygons}"} from
package \pkg{sp} (as well as
\code{"\link[sp:Polygon-class]{Polygon}"} and
\code{"\link[sp:SpatialPolygons-class]{SpatialPolygons}"})}
} The (somehow useless) default \code{xylist}-method does
not perform any transformation but only ensures that the
polygons are not closed (first vertex not repeated).
}
\details{
Different packages concerned with spatial data use
different polygon specifications with respect to: \itemize{
\item{do we repeat the first vertex?} \item{which direction
represents holes?} } Package overview: \describe{
\item{\pkg{sp}:}{\emph{Repeat} first vertex at the end
(closed), anticlockwise = hole, clockwise = normal
boundary} \item{\pkg{spatstat}:}{do \emph{not repeat} first
vertex, anticlockwise = normal boundary, clockwise = hole.
This convention is also used in \code{xylist}.}
\item{\pkg{gpclib}:}{Unfortunately, there seems to be no
convention for the specification of polygons of class
\code{"gpc.poly"}.} }
}
\author{
Sebastian Meyer\cr The implementation of the
\code{"gpc.poly"}-method of \code{xylist} depends on
functionality of the \pkg{spatstat} package and borrows
large parts from the function \code{gpc2owin} (as
implemented in package \pkg{spatstat} before version
1.34-0, when support for \code{"gpc.poly"} was dropped)
authored by Adrian Baddeley and Rolf Turner.
}
\keyword{methods}
\keyword{spatial}
