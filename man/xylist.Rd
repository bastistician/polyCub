% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xylist.R
\name{xylist}
\alias{xylist}
\alias{xylist.owin}
\alias{xylist.sfg}
\alias{xylist.gpc.poly}
\alias{xylist.SpatialPolygons}
\alias{xylist.Polygons}
\alias{xylist.Polygon}
\alias{xylist.default}
\title{Convert Various Polygon Classes to a Simple List of Vertices}
\usage{
xylist(object, ...)

\method{xylist}{owin}(object, ...)

\method{xylist}{sfg}(object, ...)

\method{xylist}{gpc.poly}(object, ...)

\method{xylist}{SpatialPolygons}(object, reverse = TRUE, ...)

\method{xylist}{Polygons}(object, reverse = TRUE, ...)

\method{xylist}{Polygon}(object, reverse = TRUE, ...)

\method{xylist}{default}(object, ...)
}
\arguments{
\item{object}{an object of one of the supported spatial classes.}

\item{...}{(unused) argument of the generic.}

\item{reverse}{logical (\code{TRUE}) indicating if the vertex order of the
\pkg{sp} classes should be reversed to get the \code{xylist}/\code{owin}
convention.}
}
\value{
Applying \code{xylist} to a polygon object, one gets a simple list,
where each component (polygon) is a list of \code{"x"} and \code{"y"}
coordinates. These represent vertex coordinates following \pkg{spatstat.geom}'s
\code{"owin"} convention (anticlockwise order for exterior boundaries,
without repeating any vertex).
}
\description{
Different packages concerned with spatial data use different polygon
specifications, which sometimes becomes very confusing (see Details below).
To be compatible with the various polygon classes, package \pkg{polyCub}
uses an S3 class \code{"xylist"}, which represents a polygonal domain
(of potentially multiple polygons) by its core feature only: a list of lists
of vertex coordinates (see the "Value" section below).
The generic function \code{xylist} can deal with the
following polygon classes:
\itemize{
\item \code{"\link[spatstat.geom:owin.object]{owin}"} from package \pkg{spatstat.geom}
\item \code{"gpc.poly"} from package \pkg{gpclib}
\item \code{"\link[sp:Polygons-class]{Polygons}"} from package \pkg{sp}
(as well as \code{"\link[sp:Polygon-class]{Polygon}"} and
\code{"\link[sp:SpatialPolygons-class]{SpatialPolygons}"})
\item \code{"\link[sf:st_polygon]{(MULTI)POLYGON}"} from package \pkg{sf}
}
The (somehow useless) default \code{xylist}-method
does not perform any transformation but only ensures that the polygons are
not closed (first vertex not repeated).
}
\details{
Polygon specifications differ with respect to:
\itemize{
\item is the first vertex repeated?
\item which ring direction represents holes?
}
Package overview:
\describe{
\item{\pkg{spatstat.geom}:}{\code{"owin"} does \emph{not repeat} the
first vertex, and anticlockwise = normal boundary, clockwise = hole.
This convention is also used for the return value of \code{xylist}.}
\item{\pkg{sp}:}{\emph{Repeat} first vertex at the end (closed),
anticlockwise = hole, clockwise = normal boundary}
\item{\pkg{sf}:}{\emph{Repeat} first vertex at the end (closed),
clockwise = hole, anticlockwise = normal boundary;
\emph{however}, \pkg{sf} does not check the ring direction by default, so
it cannot be relied upon.}
\item{\pkg{gpclib}:}{There seem to be no such conventions
for polygons of class \code{"gpc.poly"}.}
}
Thus, for polygons from \pkg{sf} and \pkg{gpclib}, \code{xylist} needs
to check the ring direction, which makes these two formats the least
efficient for integration domains in \pkg{polyCub}.
}
\author{
Sebastian Meyer
}
\keyword{methods}
\keyword{spatial}
