################################################################################
### polyCub.midpoint: Two-Dimensional Midpoint Rule
###
### Copyright (C) 2009-2015,2017,2021 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


#' Two-Dimensional Midpoint Rule
#'
#' The surface is converted to a binary pixel image
#' using the \code{\link[spatstat.geom]{as.im.function}} method from package
#' \CRANpkg{spatstat.geom} (Baddeley et al., 2015).
#' The integral under the surface is then approximated as the
#' sum over (pixel area * f(pixel midpoint)).
#'
#' @inheritParams plotpolyf
#' @param polyregion a polygonal integration domain.
#' It can be any object coercible to the \pkg{spatstat.geom} class
#' \code{"\link[spatstat.geom]{owin}"} via a corresponding
#' \code{\link[spatstat.geom]{as.owin}}-method.
#' Note that this includes polygons of the classes \code{"gpc.poly"} and
#' \code{"\link[sp:SpatialPolygons-class]{SpatialPolygons}"}, because
#' \pkg{polyCub} defines methods \code{\link{as.owin.gpc.poly}} and
#' \code{\link{as.owin.SpatialPolygons}}, respectively.
#' \pkg{sf} also registers suitable \code{as.owin} methods for its
#' \code{"\link[sf:st_polygon]{(MULTI)POLYGON}"} classes.
#' @param eps width and height of the pixels (squares),
#' see \code{\link[spatstat.geom]{as.mask}}.
#' @param dimyx number of subdivisions in each dimension,
#' see \code{\link[spatstat.geom]{as.mask}}.
#' @param plot logical indicating if an illustrative plot of the numerical
#' integration should be produced.
#' @return The approximated value of the integral of \code{f} over
#' \code{polyregion}.
#' @references
#' Baddeley A, Rubak E, Turner R (2015).
#' \emph{Spatial Point Patterns: Methodology and Applications with R}.
#' Chapman and Hall/CRC Press, London.
#' @keywords math spatial
#' @family polyCub-methods
#' @importFrom sp plot
#' @importFrom grDevices gray
#' @example examples/setting.R
#' @example examples/polyCub.midpoint.R
#' @export

polyCub.midpoint <- function (polyregion, f, ...,
                              eps = NULL, dimyx = NULL, plot = FALSE)
{
    ## as.im needs separate x and y arguments
    fxy <- function (x, y, ...) f(cbind(x,y), ...)

    ## calculate pixel values of fxy
    IM <- tryCatch(
        spatstat.geom::as.im.function(X = fxy, W = polyregion, ...,
                                      eps = eps, dimyx = dimyx),
        error = function (e) {
        ## if eps was to small such that the dimensions of the image would
        ## be too big then the operation matrix(TRUE, nr, nc) throws an
        ## error. (try e.g. devnull <- matrix(TRUE, 1e6,1e6))
        ## unfortunately, it is not clear what we should do in this case ...
        stop("inapplicable choice of bandwidth (eps=", format(eps),
             ") in midpoint rule:\n", e)
    })

### ILLUSTRATION ###
    if (plot) {
        spatstat.geom::plot.im(IM, axes=TRUE, col=gray(31:4/35), main="")
        ## add evaluation points
        #with(IM, points(expand.grid(xcol, yrow), col=!is.na(v), cex=0.5))
        plot(polyregion, add=TRUE, poly.args=list(lwd=2), lwd=2)
        ##<- two 'lwd'-specifications such that it works with owin and gpc.poly
    }
####################

    ## return the approximated integral
    spatstat.geom::integral.im(IM)
}
