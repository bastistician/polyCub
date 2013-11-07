################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2012-2013 Sebastian Meyer
### Time-stamp: <[coerce-gpc-methods.R] by SM Don 07/11/2013 20:53 (CET)>
###
### Conversion from and to the "gpc.poly" class
################################################################################


##' Conversion from and to the \code{"gpc.poly"} Class
##' 
##' Package \pkg{polyCub} implements converters between the classes 
##' \code{"\link[spatstat:owin.object]{owin}"} of package \pkg{spatstat} and
##' \code{"\link[rgeos:gpc.poly-class]{gpc.poly}"} of package \pkg{rgeos}
##' (originally from \pkg{gpclib}).
##' 
##' @param object an object of class \code{"gpc.poly"} or \code{"owin"},
##' respectively.
##' @return The converted polygon of class \code{"gpc.poly"} or \code{"owin"},
##' respectively. If neither package \pkg{rgeos} nor \pkg{gpclib} are available,
##' \code{owin2gpc} will just return the \code{pts} slot of the
##' \code{"gpc.poly"} (no formal class) with a warning.
##' @author Sebastian Meyer\cr
##' The converters are slightly modified versions of the same functions in 
##' \pkg{spatstat} version 1.33-0, authored by Adrian Baddeley and Rolf Turner.
##' (Note that support for the \code{"gpc.poly"} class was dropped from
##' \pkg{spatstat} as of version 1.34-0.)
##' @note The converter to \code{"gpc.poly"} requires the \pkg{rgeos} (or
##' \pkg{gpclib}) package for the formal class definition. It will produce 
##' vertices ordered according to the \pkg{sp} convention, i.e. clockwise for
##' normal boundaries and anticlockwise for holes, where, however, the first 
##' vertex is \emph{not} repeated!
##' @seealso \code{\link{xylist}}\cr
##' Conversions of \code{"gpc.poly"} objects from and to the
##' \code{"\link[sp:SpatialPolygons-class]{SpatialPolygons}"} class of package
##' \pkg{sp} are available in the \pkg{rgeos} package.
##' @rdname coerce-gpc-methods
##' @keywords spatial methods
##' @importFrom spatstat as.polygonal is.hole.xypolygon
##' @import methods
##' @export
owin2gpc <- function (object)
{
    pts <- lapply(as.polygonal(object)$bdry, function (poly) {
        list(x = rev(poly$x), y = rev(poly$y),
             hole = is.hole.xypolygon(poly))
    })
    if (know_gpc.poly()) new("gpc.poly", pts = pts) else {
        warning("formal class \"gpc.poly\" not available")
        pts
    }
}

##' @inheritParams owin2gpc
##' @rdname coerce-gpc-methods
##' @importFrom spatstat owin
##' @export
gpc2owin <- function (object) owin(poly = xylist.gpc.poly(object))


## check for the formal class "gpc.poly" (loading rgeos or gpclib if necessary)
##' @import methods
know_gpc.poly <- function ()
{
    isClass("gpc.poly") ||
        suppressWarnings(requireNamespace("rgeos", quietly=TRUE) ||
                         requireNamespace("gpclib", quietly=TRUE))
}
