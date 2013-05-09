################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2009-2013 Sebastian Meyer
### Time-stamp: <[polyCub.midpoint.R] by SM Don 09/05/2013 15:01 (CEST)>
################################################################################


#' Two-Dimensional Midpoint Rule
#'
#' The surface is converted to a binary pixel image
#' using the \code{\link[spatstat]{as.im.function}} method from package
#' \pkg{spatstat}. The integral under the surface is then approximated as the
#' sum over (pixel area * f(pixel midpoint)).
#' 
#' @param polyregion a polygonal integration domain.
#' It can be any object coercible to the \pkg{spatstat} class
#' \code{"\link[spatstat]{owin}"} (via \code{\link[spatstat]{as.owin}}).
#' @param f two-dimensional function to be integrated.
#' As its first argument the function must take a coordinate matrix, i.e. a
#' numeric matrix with two columns.
#' @param ... further arguments for \code{f}.
#' @param eps width and height of the pixels (squares),
#' see \code{\link[spatstat]{as.mask}}.
#' @param dimyx number of subdivisions in each dimension,
#' see \code{\link[spatstat]{as.mask}}.
#' @param plot logical indicating if an illustrative plot of the numerical
#' integration should be produced.
#' @return The approximated value of the integral of \code{f} over
#' \code{polyregion}.
#' @references
#' A. Baddeley and R. Turner (2005).
#' Spatstat: an R package for analyzing spatial point patterns.
#' Journal of Statistical Software 12 (6), 1-42.
#' @keywords math spatial
#' @family polyCub-methods
#' @importFrom spatstat as.im.function plot.im plot.owin
#' @examples # see example(polyCub)
#' @export

polyCub.midpoint <- function (polyregion, f, ...,
                              eps = NULL, dimyx = NULL, plot = FALSE)
{
    ## as.im needs seperate x and y arguments
    fxy <- function (x, y, ...) f(cbind(x,y), ...)

    ## calculate pixel values of fxy
    IM <- tryCatch(
          as.im.function(X=fxy, W=polyregion, ..., eps=eps, dimyx=dimyx),
          error = function (e) {
              ## if eps was to small such that the dimensions of the image would
              ## be too big then the operation matrix(TRUE, nr, nc) throws an
              ## error. (try e.g. devnull <- matrix(TRUE, 1e6,1e6))
              ## unfortunately, it is not clear what we should do in this
              ## case... => stop
              stop("inapplicable choice of bandwidth (eps=", format(eps),
                   ") in midpoint rule:\n", e)
          })
    
### ILLUSTRATION ###
    if (plot) {
        plot.im(IM, axes=TRUE, col=grey(31:4/35), main="")
        ## add evaluation points (unsure about spatstat implementation of class "im")
        ## both of the following commands worked with different versions of spatstat
        #with(IM, points(expand.grid(xcol, yrow), col=!is.na(v), cex=0.5))
        #with(IM, points(expand.grid(y=yrow, x=xcol)[2:1], col=!is.na(v), cex=0.5))
        plot(polyregion, add=TRUE, poly.args=list(lwd=2), lwd=2)
        ##<- two 'lwd'-specifications such that it works with owin and gpc.poly
    }
####################
    
    ## return the approximated integral
    pixelarea <- IM$xstep * IM$ystep
    int <- pixelarea * sum(IM$v, na.rm = TRUE)
    int
}
