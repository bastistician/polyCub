################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2009-2013 Sebastian Meyer
### Time-stamp: <[discpoly.R] by SM Don 09/05/2013 14:59 (CEST)>
################################################################################


##' Polygonal Approximation of a Disc/Circle
##'
##' Generates a polygon representing a disc/circle (in planar coordinates)
##' as an object of one of three possible classes:
##' \code{"\link[rgeos:gpc.poly-class]{gpc.poly}"},
##' \code{"\link[spatstat]{owin}"}, or \code{"\link[sp:Polygon-class]{Polygon}"}.
##'
##' @param center numeric vector of length 2 (center coordinates of the circle).
##' @param r single numeric value (radius of the circle).
##' @param npoly single integer. Number of edges of the polygonal approximation.
##' @param class class of the resulting polygon (partial name matching applies).
##' For \code{"owin"}, this is just a wrapper around \pkg{spatstat}'s own
##' \code{\link[spatstat]{disc}} function.
##' @param hole logical. Does the resulting polygon represent a hole?
##' @author Sebastian Meyer\cr
##' This function is inspired by the \code{\link[spatstat]{disc}} function from
##' package \pkg{spatstat} authored by Adrian Baddeley and Rolf Turner.
##' @return A polygon of class \code{class} representing a circle/disc with
##' \code{npoly} edges accuracy.
##' @seealso \link[spatstat]{disc} in package \pkg{spatstat}.
##' @keywords spatial datagen
##' @importFrom spatstat disc
##' @importClassesFrom rgeos gpc.poly
##' @export
##' @example inst/examples/discpoly.R

discpoly <- function (center, r, npoly = 64,
                      class = c("Polygon", "owin", "gpc.poly"), hole = FALSE)
{
    class <- match.arg(class)
    if (class == "owin") { # use spatstat::disc
        res <- disc(radius = r, centre = center, mask = FALSE, npoly = npoly)
        if (hole) {
            res$bdry[[1]]$x <- rev(res$bdry[[1]]$x)
            res$bdry[[1]]$y <- rev(res$bdry[[1]]$y)
            res$bdry[[1]]$hole <- TRUE
        }
        return(res)
    }

    ## do it myself for the "Polygon" and "gpc.poly" classes
    stopifnot(r > 0, isScalar(npoly), npoly > 2)
    theta <- seq(2*pi, 0, length = npoly+1)[-(npoly+1)]   # for clockwise order
    if (hole) theta <- rev(theta)   # for anticlockwise order
    x <- center[1] + r * cos(theta)
    y <- center[2] + r * sin(theta)
    switch(class,
        "Polygon" = Polygon(cbind(c(x,x[1]),c(y,y[1])), hole=hole),
        "gpc.poly" = new("gpc.poly", pts = list(list(x=x, y=y, hole=hole)))
    )
}
