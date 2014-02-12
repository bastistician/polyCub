\name{plot_polyregion}
\alias{plot_polyregion}
\title{Plots a Polygon (of Various Classes)}
\usage{
plot_polyregion(polyregion, lwd = 2, add = FALSE)
}
\arguments{
  \item{lwd}{line width.}

  \item{add}{logical. Add to existing plot?}

  \item{polyregion}{a polygonal integration domain. The
  following classes are supported:
  \code{"\link[spatstat]{owin}"},
  \code{"\link[rgeos:gpc.poly-class]{gpc.poly}"},
  \code{"\linkS4class{SpatialPolygons}"},
  \code{"\linkS4class{Polygons}"}, and
  \code{"\linkS4class{Polygon}"} (for these we have an
  internal \code{\link{xylist}} method).}
}
\description{
Plots a Polygon (of Various Classes)
}

