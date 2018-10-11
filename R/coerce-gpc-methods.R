################################################################################
### Conversion between polygonal "owin" and "gpc.poly"
###
### Copyright (C) 2012-2015,2017-2018 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


##' Conversion between polygonal \code{"owin"} and \code{"gpc.poly"}
##'
##' Package \pkg{polyCub} implements converters between the classes
##' \code{"\link[spatstat:owin.object]{owin}"} of package \pkg{spatstat} and
##' \code{"\link[rgeos:gpc.poly-class]{gpc.poly}"} of package \pkg{rgeos}
##' (originally from \pkg{gpclib}).
##' Support for the \code{"gpc.poly"} class was dropped from
##' \pkg{spatstat} as of version 1.34-0.
##'
##' @param object an object of class \code{"gpc.poly"} or \code{"owin"},
##' respectively.
##' @return The converted polygon of class \code{"gpc.poly"} or \code{"owin"},
##' respectively. If neither package \pkg{rgeos} nor \pkg{gpclib} are available,
##' \code{owin2gpc} will just return the \code{pts} slot of the
##' \code{"gpc.poly"} (no formal class) with a warning.
##' @author Sebastian Meyer
##' @note The converter \code{owin2gpc} requires the package \pkg{rgeos} (or
##' \pkg{gpclib}) for the formal class definition of a \code{"gpc.poly"}.
##' It will produce vertices ordered according to the \pkg{sp} convention,
##' i.e. clockwise for normal boundaries and anticlockwise for holes, where,
##' however, the first vertex is \emph{not} repeated!
##' @seealso \code{\link{xylist}}, and the package \pkg{rgeos} for
##' conversions of \code{"gpc.poly"} objects from and to \pkg{sp}'s
##' \code{"\linkS4class{SpatialPolygons}"} class.
##' @name coerce-gpc-methods
##' @rdname coerce-gpc-methods
##' @keywords spatial methods
##' @import methods
##' @export
##' @examples
##' if (gpclibPermit() && require("spatstat")) {
##'     ## use example polygons from
##'     example(plotpolyf, ask = FALSE)
##'
##'     letterR  # a simple "xylist"
##'     letterR.owin <- owin(poly = letterR)
##'     letterR.gpc_from_owin <- owin2gpc(letterR.owin)
##'     letterR.xylist_from_gpc <- xylist(letterR.gpc_from_owin)
##'     stopifnot(all.equal(letterR, lapply(letterR.xylist_from_gpc, "[", 1:2)))
##'     letterR.owin_from_gpc <- as.owin(letterR.gpc_from_owin)
##'     stopifnot(all.equal(letterR.owin, letterR.owin_from_gpc))
##' }
owin2gpc <- function (object)
{
    object <- spatstat::as.polygonal(object)

    ## determine hole flags of the individual polygons
    hole <- spatstat::summary.owin(object)$areas < 0

    ## reverse vertices and set hole flags
    pts <- mapply(
        FUN = function (poly, hole) {
            list(x = rev.default(poly$x), y = rev.default(poly$y),
                 hole = hole)  # or spatstat.utils::is.hole.xypolygon(poly)
        },
        poly = object$bdry, hole = hole,
        SIMPLIFY = FALSE, USE.NAMES = FALSE)

    ## formal class
    if (know_gpc.poly()) {
        new("gpc.poly", pts = pts)
    } else {
        warning("formal class \"gpc.poly\" not available")
        pts
    }
}

##' @inheritParams owin2gpc
##' @param ... further arguments passed to \code{\link[spatstat]{owin}}.
##' @rdname coerce-gpc-methods
##' @export
gpc2owin <- function (object, ...)
{
    ## first convert to an "owin" without checking areas etc.
    ## to determine the hole status according to vertex order (area)
    res <- spatstat::owin(poly = object@pts, check = FALSE)
    holes_owin <- spatstat::summary.owin(res)$areas < 0
    ## or directly lapply spatstat.utils::is.hole.xypolygon

    ## now fix the vertex order
    bdry <- mapply(
        FUN = function (poly, owinhole) {
            if (poly$hole != owinhole) {
                poly$x <- rev(poly$x)
                poly$y <- rev(poly$y)
            }
            poly
        },
        poly = object@pts, owinhole = holes_owin,
        SIMPLIFY = FALSE, USE.NAMES = FALSE)

    ## now really convert to owin with appropriate vertex order
    spatstat::owin(poly = bdry, ...)
}

##' @inheritParams gpc2owin
##' @param W an object of class \code{"gpc.poly"}.
##' @rdname coerce-gpc-methods
##' @export
##' @rawNamespace if(getRversion() >= "3.6.0") {  # delayed registration
##'     S3method(spatstat::as.owin, gpc.poly)
##' }
as.owin.gpc.poly <- function (W, ...)
{
    gpc2owin(W, ...)
}


## check for the formal class "gpc.poly" (loading rgeos or gpclib if necessary)
##' @import methods
know_gpc.poly <- function ()
{
    isClass("gpc.poly") ||
        suppressWarnings(requireNamespace("rgeos", quietly=TRUE) ||
                         requireNamespace("gpclib", quietly=TRUE))
}
