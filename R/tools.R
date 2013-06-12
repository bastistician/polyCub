################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2009-2013 Sebastian Meyer
### Time-stamp: <[tools.R] by SM Die 11/06/2013 23:48 (CEST)>
###
### Tiny toolbox
################################################################################


##' Check if Polygon is Closed
##'
##' Check if the first and last coordinates of a coordinate matrix are
##' identical.
##' @param coords numeric coordinate matrix. It is interpreted by
##' \code{\link{xy.coords}}.
##' @return logical
##' @keywords spatial internal
isClosed <- function (coords)
{
    xycoords <- xy.coords(coords)[c("x","y")]
    n <- length(xycoords$x)
    return(identical(xycoords$x[1], xycoords$x[n]) &&
           identical(xycoords$y[1], xycoords$y[n]))
}


##' Dot/Scalar Product of Two Vectors
##'
##' This is nothing else than \code{sum(x*y)}.
##' @param x,y numeric vectors (of compatible lengths).
##' @return \code{sum(x*y)}
##' @keywords math internal
dotprod <- function (x,y) sum(x*y)

##' Euclidean Vector Norm (Length)
##'
##' This is nothing else than \code{sqrt(sum(x^2))}.
##' @param x numeric vector.
##' @return \code{sqrt(sum(x^2))}
##' @keywords math internal
vecnorm <- function (x) sqrt(sum(x^2))
    
##' Checks if Argument is Scalar
##' 
##' Check if the argument is scalar, i.e. a numeric vector of length 1.
##' @param x any object
##' @return logical
##' @keywords internal
isScalar <- function (x) {
    length(x) == 1L && is.vector(x, mode = "numeric")
}


##' Simply Plots a Polygon (of Various Classes)
##'
##' @inheritParams polyCub.SV
plot_polyregion <- function (polyregion)
{
    if (inherits(polyregion, "Polygon"))
        polyregion <- Polygons(list(polyregion), "ID")
    if (inherits(polyregion, "Polygons"))
        polyregion <- SpatialPolygons(list(polyregion))
    if (inherits(polyregion, "gpc.poly")) {
        plot(polyregion, poly.args=list(lwd=2), ann=FALSE)
    } else plot(polyregion, lwd=2, axes=TRUE, main="")
}
