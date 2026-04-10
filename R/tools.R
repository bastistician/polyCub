################################################################################
### Internal Functions
###
### Copyright (C) 2009-2015,2017,2021,2026 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


## Check if a Polygon is Closed
## (first and last row of coordinate matrix are identical)
#' @importFrom grDevices xy.coords
isClosed <- function (coords)
{
    xycoords <- xy.coords(coords)[c("x","y")]
    n <- length(xycoords$x)
    return(identical(xycoords$x[1], xycoords$x[n]) &&
           identical(xycoords$y[1], xycoords$y[n]))
}


## Dot/Scalar Product of Two Vectors
dotprod <- function (x,y) sum(x*y)

## Euclidean Vector Norm (Length)
vecnorm <- function (x) sqrt(sum(x^2))

## Check if an R object is scalar (a numeric vector of length 1)
isScalar <- function (x) {
    length(x) == 1L && is.vector(x, mode = "numeric")
}


## Plot a Polygonal Domain (of Various Classes)
#' @importMethodsFrom sp plot
plot_polyregion <- function (polyregion, lwd=2, add=FALSE)
{
    if (is.vector(polyregion, mode="list")) { # internal xylist object
        stopifnot(add)
        lapply(polyregion, graphics::polygon, lwd=lwd)
        invisible()
    } else if (inherits(polyregion, "gpc.poly")) {
        plot(polyregion, poly.args=list(lwd=lwd), ann=FALSE, add=add)
    } else {
        if (inherits(polyregion, "Polygon"))
            polyregion <- sp::Polygons(list(polyregion), "ID")
        if (inherits(polyregion, "Polygons"))
            polyregion <- sp::SpatialPolygons(list(polyregion))
        ## plot call which works for "SpatialPolygons", "owin", and "sfg"
        plot(polyregion, lwd=lwd, axes=TRUE, main="", add=add)
    }
}
