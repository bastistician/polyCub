################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2009-2013 Sebastian Meyer
### Time-stamp: <[tools.R] by SM Don 09/05/2013 13:06 (CEST)>
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
dotprod <- function (x,y)
{
    sum(x*y)
}


##' Checks if Argument is Scalar
##' 
##' Check if the argument is scalar, i.e. a numeric vector of length 1.
##' @param x any object
##' @return logical
##' @keywords internal
isScalar <- function (x) {
    length(x) == 1L && is.vector(x, mode = "numeric")
}
