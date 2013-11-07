################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2012-2013 Sebastian Meyer
### Time-stamp: <[xylist.R] by SM Don 07/11/2013 21:10 (CET)>
###
### Convert various polygon classes to a simple "xylist"
################################################################################


##' Convert Various Polygon Classes to a Simple List of Vertices
##' 
##' Different packages concerned with spatial data use different polygon
##' specifications, which sometimes becomes very confusing (see Details below).
##' To be compatible with the various polygon classes, package \pkg{polyCub}
##' uses an S3 class \code{"xylist"}, which represents
##' polygons by their core feature only, a list of lists of vertex coordinates
##' (see the "Value" section below).
##' The generic function \code{xylist} can deal with the
##' following polygon classes:
##' \itemize{
##' \item{\code{"\link[spatstat:owin.object]{owin}"} from package \pkg{spatstat}}
##' \item{\code{"\link[rgeos:gpc.poly-class]{gpc.poly}"} from package
##' \pkg{rgeos} (or \pkg{gpclib})}
##' \item{\code{"\link[sp:Polygons-class]{Polygons}"} from package \pkg{sp}
##' (as well as \code{"\link[sp:Polygon-class]{Polygon}"} and
##' \code{"\link[sp:SpatialPolygons-class]{SpatialPolygons}"})}
##' }
##' The (somehow useless) default \code{xylist}-method
##' does not perform any transformation but only ensures that the polygons are
##' not closed (first vertex not repeated).
##' 
##' Different packages concerned with spatial data use different polygon
##' specifications with respect to:
##' \itemize{
##' \item{do we repeat the first vertex?}
##' \item{which direction represents holes?}
##' }
##' Package overview:
##' \describe{
##' \item{\pkg{sp}:}{\emph{Repeat} first vertex at the end (closed),
##' anticlockwise = hole, clockwise = normal boundary}
##' \item{\pkg{spatstat}:}{do \emph{not repeat} first vertex,
##' anticlockwise = normal boundary, clockwise = hole. This convention is also
##' used in \code{xylist}.}
##' \item{\pkg{gpclib}:}{Unfortunately, there seems to be no convention
##' for the specification of polygons of class \code{"gpc.poly"}.}
##' }
##'
##' @param object an object of one of the supported spatial classes.
##' @param ... (unused) argument of the generic.
##' @return Applying \code{xylist} to a polygon object, one gets a simple list,
##' where each component (polygon) is a list of \code{"x"} and \code{"y"}
##' coordinates. These represent vertex coordinates following \pkg{spatstat}'s
##' \code{"owin"} convention (anticlockwise order without repeating any vertex).
##' The opposite vertex order can be retained for the \pkg{sp}-classes
##' by the non-default use with \code{reverse=FALSE}.\cr
##' @author Sebastian Meyer\cr 
##' The implementation of the \code{"gpc.poly"}-method of \code{xylist}
##' depends on functionality of the \pkg{spatstat} package and borrows
##' large parts from the function \code{gpc2owin} (as implemented in package
##' \pkg{spatstat} before version 1.34-0, when support for \code{"gpc.poly"} was
##' dropped) authored by Adrian Baddeley and Rolf Turner.
##' @name xylist
##' @keywords spatial methods
##' @export
xylist <- function (object, ...) UseMethod("xylist")

##' @method xylist owin
##' @S3method xylist owin
##' @rdname xylist
##' @importFrom spatstat is.polygonal
xylist.owin <- function (object, ...) {
    if (is.polygonal(object)) object$bdry else {
        stop("object is not polygonal")
    }
}

##' @method xylist gpc.poly
##' @S3method xylist gpc.poly
##' @rdname xylist
##' @importFrom spatstat area.xypolygon reverse.xypolygon
xylist.gpc.poly <- function (object, ...)
{
    lapply(object@pts, function (poly) {
        if (poly$hole != (area.xypolygon(poly) < 0))
            poly <- reverse.xypolygon(poly)
        poly[c("x","y")]
    })
}

##' @method xylist SpatialPolygons
##' @S3method xylist SpatialPolygons
##' @rdname xylist
##' @inheritParams xylist.Polygons
xylist.SpatialPolygons <- function (object, reverse = TRUE, ...)
{
    unlist(lapply(object@polygons, xylist.Polygons, reverse=reverse, ...),
           recursive=FALSE, use.names=FALSE)
}

##' @method xylist Polygons
##' @S3method xylist Polygons
##' @rdname xylist
##' @param reverse logical (\code{TRUE}) indicating if the vertex order of the
##' \pkg{sp} classes should be reversed to get the \code{xylist}/\code{owin}
##' convention.
##' @import sp
xylist.Polygons <- function (object, reverse = TRUE, ...)
{
    lapply(object@Polygons, function (sr) {
        coords <- coordinates(sr)
        n <- nrow(coords) - 1L   # number of vertices
        idxs <- if (reverse) seq.int(n,1) else seq_len(n)
        list(x = coords[idxs,1L], y = coords[idxs,2L])
             #area = sr@area, hole = sr@hole
    })
}

##' @method xylist Polygon
##' @S3method xylist Polygon
##' @rdname xylist
##' @import methods
xylist.Polygon <- function (object, reverse = TRUE, ...)
    xylist.Polygons(as(object,"Polygons"), reverse=reverse, ...)

##' @method xylist default
##' @S3method xylist default
##' @rdname xylist
##' @importFrom grDevices xy.coords
xylist.default <- function (object, ...) {
    lapply(object, function (xy) {
        poly <- xy.coords(xy)[c("x","y")]
        if (isClosed(poly)) {
            sel <- seq_len(length(poly$x) - 1L)
            poly$x <- poly$x[sel]
            poly$y <- poly$y[sel]
        }
        poly
    })
}