################################################################################
### Internal Functions
###
### Copyright (C) 2009-2015,2017 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


##' Check if Polygon is Closed
##'
##' Check if the first and last coordinates of a coordinate matrix are
##' identical.
##' @param coords numeric coordinate matrix. It is interpreted by
##' \code{\link{xy.coords}}.
##' @return logical
##' @keywords spatial internal
##' @importFrom grDevices xy.coords
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


##' Plots a Polygonal Domain (of Various Classes)
##'
##' @inheritParams plotpolyf
##' @param add logical. Add to existing plot?
##' @import methods
##' @import sp
##' @importFrom graphics polygon
## NOTE: we don't import graphics::plot since it is already imported via sp
plot_polyregion <- function (polyregion, lwd=2, add=FALSE)
{
    if (is.vector(polyregion, mode="list")) { # internal xylist object
        stopifnot(add)
        lapply(polyregion, polygon, lwd=lwd)
        invisible()
    } else if (inherits(polyregion, "gpc.poly")) {
        plot(polyregion, poly.args=list(lwd=lwd), ann=FALSE, add=add)
    } else {
        if (inherits(polyregion, "Polygon"))
            polyregion <- Polygons(list(polyregion), "ID")
        if (inherits(polyregion, "Polygons"))
            polyregion <- SpatialPolygons(list(polyregion))
        if (inherits(polyregion, "owin"))
            ## && ! "plot.owin" %in% getNamespaceInfo("spatstat", "S3methods")
            plot <- spatstat::plot.owin  # spatstat <1.33-0 has no registration
        ## plot call which works for "SpatialPolygons" and "owin"
        plot(polyregion, lwd=lwd, axes=TRUE, main="", add=add)
    }
}


##' Constructs Equally-Spaced Grid
##'
##' Construct an equally-spaced grid given a range and the number of cut points
##' (one more than the number of resulting bins).
##' This is nothing else than \code{seq(range[1], range[2], length.out=n)}.
##' @param range numeric vector of length 2.
##' @param n length of the desired grid, i.e. number of bins + 1.
##' @return the desired grid, a numeric vector of length \code{n} covering
##' \code{range}.
##' @keywords internal
makegrid <- function(range, n) seq(range[1], range[2], length.out=n)
