################################################################################
### xylist: Convert Various Polygon Classes to a Simple List of Vertices
###
### Copyright (C) 2012-2014,2017,2021 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


#' Convert Various Polygon Classes to a Simple List of Vertices
#'
#' Different packages concerned with spatial data use different polygon
#' specifications, which sometimes becomes very confusing (see Details below).
#' To be compatible with the various polygon classes, package \pkg{polyCub}
#' uses an S3 class \code{"xylist"}, which represents a polygonal domain
#' (of potentially multiple polygons) by its core feature only: a list of lists
#' of vertex coordinates (see the "Value" section below).
#' The generic function \code{xylist} can deal with the
#' following polygon classes:
#' \itemize{
#' \item \code{"\link[spatstat.geom:owin.object]{owin}"} from package \pkg{spatstat.geom}
#' \item \code{"gpc.poly"} from package \pkg{gpclib}
#' \item \code{"\link[sp:Polygons-class]{Polygons}"} from package \pkg{sp}
#' (as well as \code{"\link[sp:Polygon-class]{Polygon}"} and
#' \code{"\link[sp:SpatialPolygons-class]{SpatialPolygons}"})
#' \item \code{"\link[sf:st_polygon]{(MULTI)POLYGON}"} from package \pkg{sf}
#' }
#' The (somehow useless) default \code{xylist}-method
#' does not perform any transformation but only ensures that the polygons are
#' not closed (first vertex not repeated).
#'
#' Polygon specifications differ with respect to:
#' \itemize{
#' \item is the first vertex repeated?
#' \item which ring direction represents holes?
#' }
#' Package overview:
#' \describe{
#' \item{\pkg{spatstat.geom}:}{\code{"owin"} does \emph{not repeat} the
#' first vertex, and anticlockwise = normal boundary, clockwise = hole.
#' This convention is also used for the return value of \code{xylist}.}
#' \item{\pkg{sp}:}{\emph{Repeat} first vertex at the end (closed),
#' anticlockwise = hole, clockwise = normal boundary}
#' \item{\pkg{sf}:}{\emph{Repeat} first vertex at the end (closed),
#' clockwise = hole, anticlockwise = normal boundary;
#' \emph{however}, \pkg{sf} does not check the ring direction by default, so
#' it cannot be relied upon.}
#' \item{\pkg{gpclib}:}{There seem to be no such conventions
#' for polygons of class \code{"gpc.poly"}.}
#' }
#' Thus, for polygons from \pkg{sf} and \pkg{gpclib}, \code{xylist} needs
#' to check the ring direction, which makes these two formats the least
#' efficient for integration domains in \pkg{polyCub}.
#'
#' @param object an object of one of the supported spatial classes.
#' @param ... (unused) argument of the generic.
#' @return Applying \code{xylist} to a polygon object, one gets a simple list,
#' where each component (polygon) is a list of \code{"x"} and \code{"y"}
#' coordinates. These represent vertex coordinates following \pkg{spatstat.geom}'s
#' \code{"owin"} convention (anticlockwise order for exterior boundaries,
#' without repeating any vertex).
#' @author Sebastian Meyer
#' @name xylist
#' @keywords spatial methods
#' @export
xylist <- function (object, ...) UseMethod("xylist")

#' @rdname xylist
#' @export
xylist.owin <- function (object, ...)
{
    spatstat.geom::as.polygonal(object)$bdry
}

#' @rdname xylist
#' @export
xylist.sfg <- function (object, ...)
{
    assert_polygonal_sfg(object)
    obj <- sf::st_sfc(object, check_ring_dir = TRUE)[[1L]]
    ## it would be more efficient to use sf's check_ring_dir() directly
    ## unfortunately, that function is not exported from sf (0.9-7)
    if (inherits(obj, "MULTIPOLYGON"))
        obj <- unlist(obj, recursive = FALSE)
    lapply(obj, function (coords) {
        idx <- seq_len(nrow(coords) - 1L)
        list(x = coords[idx, 1L], y = coords[idx, 2L])
    })
}

#' @rdname xylist
#' @export
xylist.gpc.poly <- function (object, ...)
{
    xylist.owin(gpc2owin(object, check = FALSE))
}

#' @rdname xylist
#' @export
xylist.SpatialPolygons <- function (object, reverse = TRUE, ...)
{
    unlist(lapply(object@polygons, xylist.Polygons, reverse=reverse, ...),
           recursive=FALSE, use.names=FALSE)
}

#' @rdname xylist
#' @param reverse logical (\code{TRUE}) indicating if the vertex order of the
#' \pkg{sp} classes should be reversed to get the \code{xylist}/\code{owin}
#' convention.
#' @importFrom sp coordinates
#' @export
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

#' @rdname xylist
#' @import methods
#' @export
xylist.Polygon <- function (object, reverse = TRUE, ...)
    xylist.Polygons(as(object,"Polygons"), reverse=reverse, ...)

#' @rdname xylist
#' @importFrom grDevices xy.coords
#' @export
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
