################################################################################
### plotpolyf: Plot Polygonal Domain on Image of Bivariate Function
###
### Copyright (C) 2013-2014,2018 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


##' Plot Polygonal Domain on Image of Bivariate Function
##'
##' Produces a combined plot of a polygonal domain and an image of a bivariate
##' function, using either \code{\link[lattice:levelplot]{lattice::levelplot}}
##' or \code{\link{image}}.
##'
##' @param polyregion a polygonal domain.
##' The following classes are supported:
##' \code{"\link[spatstat]{owin}"} from package \pkg{spatstat},
##' \code{"\link[rgeos:gpc.poly-class]{gpc.poly}"} from \pkg{rgeos} (or
##' \pkg{gpclib}), as well as
##' \code{"\linkS4class{SpatialPolygons}"}, \code{"\linkS4class{Polygons}"},
##' and \code{"\linkS4class{Polygon}"} from package \pkg{sp}.
##' (For these classes, \pkg{polyCub} knows how to get an \code{\link{xylist}}.)
##' @param f a two-dimensional real-valued function.
##' As its first argument it must take a coordinate matrix, i.e., a
##' numeric matrix with two columns, and it must return a numeric vector of
##' length the number of coordinates.
##' @param ... further arguments for \code{f}.
##' @param npixel numeric vector of length 1 or 2 setting the number of pixels
##' in each dimension.
##' @param cuts number of cut points in the \eqn{z} dimension.
##' The range of function values will be divided into \code{cuts+1} levels.
##' @param col color vector used for the function levels.
##' @param lwd line width of the polygon edges.
##' @param xlim,ylim numeric vectors of length 2 setting the axis limits.
##' \code{NULL} means using the bounding box of \code{polyregion}.
##' @param use.lattice logical indicating if \pkg{lattice} graphics
##' (\code{\link[lattice]{levelplot}}) should be used.
##' @param print.args a list of arguments passed to \code{\link{print.trellis}}
##' for plotting the produced \code{\link[lattice:trellis.object]{"trellis"}} object
##' (given \code{use.lattice = TRUE}). The latter will be returned without
##' explicit \code{print}ing if \code{print.args} is not a list.
##' @author Sebastian Meyer
##' @keywords hplot
##' @example examples/plotpolyf.R
##' @importFrom grDevices extendrange heat.colors
##' @importFrom graphics image
##' @export

plotpolyf <- function (polyregion, f, ..., npixel = 100, cuts = 15,
                       col = rev(heat.colors(cuts+1)), lwd = 3,
                       xlim = NULL, ylim = NULL,
                       use.lattice = TRUE, print.args = list())
{
    polys <- xylist(polyregion)
    npixel <- rep(npixel, length.out = 2L)

    ## make two-dimensional grid
    if (is.null(xlim))
        xlim <- extendrange(unlist(lapply(polys, "[[", "x"), use.names=FALSE))
    if (is.null(ylim))
        ylim <- extendrange(unlist(lapply(polys, "[[", "y"), use.names=FALSE))
    xgrid <- seq(xlim[1L], xlim[2L], length.out = npixel[1L])
    ygrid <- seq(ylim[1L], ylim[2L], length.out = npixel[2L])
    xygrid <- expand.grid(x=xgrid, y=ygrid, KEEP.OUT.ATTRS=FALSE)

    ## compute function values on the grid
    xygrid$fval <- f(as.matrix(xygrid, rownames.force = FALSE), ...)

    ## plot
    if (use.lattice && requireNamespace("lattice")) {
        mypanel <- function(...) {
            lattice::panel.levelplot(...)
            lapply(polys, function(xy) lattice::panel.polygon(xy, lwd=lwd))
        }
        trobj <- lattice::levelplot(fval ~ x*y, data=xygrid, aspect="iso",
                                    cuts=cuts, col.regions=col, panel=mypanel)
        if (is.list(print.args)) {
            do.call("print", c(alist(x=trobj), print.args))
        } else trobj
    } else {
        image(xgrid, ygrid, matrix(xygrid$fval, npixel[1L], npixel[2L]),
              col=col, xlab="x", ylab="y", asp=1)
        plot_polyregion(polyregion, lwd=lwd, add=TRUE)
    }
}
