################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2013 Sebastian Meyer
### Time-stamp: <[plotpolyf.R] by SM Don 07/11/2013 20:57 (CET)>
###
### Plot polygonal domain with image of bivariate function
################################################################################


##' Plot Polygonal Domain on Image of Bivariate Function
##'
##' Produces a combined plot of a polygonal domain and an image of a bivariate
##' function, using either \code{\link[lattice:levelplot]{lattice::levelplot}}
##' or \code{\link{image}}.
##' 
##' @inheritParams polyCub.SV
##' @param npixel numeric vector of length 1 or 2 setting the number of pixels
##' in each dimension.
##' @param cuts number of cut points in the \eqn{z} dimension.
##' The range of function values will be divided into \code{cuts+1} levels.
##' @param col colour vector used for the function levels.
##' @param lwd line width of the polygon edges.
##' @param xlim,ylim numeric vectors of length 2 setting the axis limits.
##' \code{NULL} means using the bounding box of \code{polyregion}.
##' @param use.lattice logical indicating if \pkg{lattice} graphics
##' (\code{\link[lattice]{levelplot}}) should be used.
##' @author Sebastian Meyer
##' @keywords hplot
##' @example inst/examples/plotpolyf.R
##' @importFrom grDevices extendrange heat.colors
##' @importFrom graphics image
##' @export

plotpolyf <- function (polyregion, f, ...,
                       npixel=100, cuts=15, col=rev(heat.colors(cuts+1)), lwd=3,
                       xlim=NULL, ylim=NULL, use.lattice=TRUE)
{
    polys <- xylist(polyregion)
    npixel <- rep(npixel, length.out=2)

    ## make two-dimensional grid
    if (is.null(xlim))
        xlim <- extendrange(unlist(lapply(polys, "[[", "x"), use.names=FALSE))
    if (is.null(ylim))
        ylim <- extendrange(unlist(lapply(polys, "[[", "y"), use.names=FALSE))
    xgrid <- makegrid(xlim, npixel[1])
    ygrid <- makegrid(ylim, npixel[2])
    xygrid <- expand.grid(x=xgrid, y=ygrid, KEEP.OUT.ATTRS=FALSE)

    ## compute function values on the grid
    xygrid$fval <- f(xygrid, ...)

    ## plot
    if (use.lattice && require("lattice")) {
        mypanel <- function(...) {
            panel.levelplot(...)
            lapply(polys, function(xy) panel.polygon(xy, lwd=lwd))
        }
        print(levelplot(fval ~ x*y, data=xygrid, aspect="iso",
                        cuts=cuts, col.regions=col, panel=mypanel))
    } else {
        image(xgrid, ygrid, matrix(xygrid$fval, npixel[1], npixel[2]), col=col,
              xlab="x", ylab="y", asp=1)
        plot_polyregion(polyregion, lwd=lwd, add=TRUE)
    }
}
