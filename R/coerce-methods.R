################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Some coerce-methods between different spatial classes
###
### Copyright (C) 2012-2013 Sebastian Meyer
### $Revision$
### $Date$
################################################################################


##' Polygon Coercion Between Different Spatial Classes
##' 
##' Different packages concerned with spatial data use different polygon
##' specifications. 
##' The package \pkg{polyCub} offers S4-style coerce-methods to convert
##' \code{"\link[sp:SpatialPolygons-class]{SpatialPolygons}"} and
##' \code{"\link[sp:Polygons-class]{Polygons}"} of the \pkg{sp} package,
##' and \code{"\link[spatstat:owin.object]{owin}"} objects of the
##' \pkg{spatstat} package to
##' the \code{"\link[gpclib:gpc.poly-class]{gpc.poly}"} class of the
##' \pkg{gpclib} package. It also defines conversion from \code{"gpc.poly"}
##' to \code{"Polygons"}. Note that conversions from and to the \code{"owin"}
##' class are also available as functions \code{\link[spatstat]{owin2gpc}} and
##' \code{\link[spatstat]{gpc2owin}} in package \pkg{spatstat}.
##' Furthermore, the (rather internal) \code{xylist} methods break down 
##' (convert) polygons from these classes to their core feature,
##' a list of vertex coordinates.
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
##' for the specification of polygons of class \code{"gpc.poly"}.
##' The \code{coerce}-methods defined here assume vertices
##' ordered according to the \pkg{sp} convention, i.e. clockwise for
##' normal boundaries and anticlockwise for holes; however, the first vertex
##' should \emph{not} be repeated!}
##' }
##'
##' @param object an object of one of the supported spatial classes.
##' @param ... (unused) argument of the generic.
##' @return Applying \code{xylist} to a polygon object, one gets a simple list,
##' where each component (polygon) is a list of \code{"x"} and \code{"y"}
##' coordinates, which give the coordinates of the vertices of the polygon
##' following \pkg{spatstat}'s \code{"owin"} convention
##' (anticlockwise order without repeating any vertex).
##' Additional elements like \code{"area"} and \code{"hole"} in each
##' component are retained. The default \code{xylist}-method does not perform
##' any transformation but only checks that the polygons are not closed
##' (first vertex not repeated).
##' @name coerce-methods
##' @aliases xylist
##' @exportMethod coerce
##' @keywords spatial methods
xylist <- function (object, ...) UseMethod("xylist")

##' @method xylist owin
##' @rdname coerce-methods
xylist.owin <- function (object, ...) object$bdry

##' @method xylist gpc.poly
##' @rdname coerce-methods
xylist.gpc.poly <- function (object, ...) xylist.owin(gpc2owin(object))

##' @method xylist SpatialPolygons
##' @rdname coerce-methods
xylist.SpatialPolygons <- function (object, ...)
    xylist.owin(maptools::as.owin.SpatialPolygons(object))

##' @method xylist Polygons
##' @rdname coerce-methods
xylist.Polygons <- function (object, ...)
    xylist.SpatialPolygons(sp::SpatialPolygons(list(object)))

##' @method xylist Polygon
##' @rdname coerce-methods
xylist.Polygon <- function (object, ...)
    xylist.Polygons(sp::Polygons(list(object), "ID"))

##' @method xylist default
##' @rdname coerce-methods
xylist.default <- function (object, ...) {
    lapply(object, function(xy) {
        poly <- xy.coords(xy)[c("x","y")]
        if (isClosed(poly)) {
            n <- length(poly$x)
            sel <- seq_len(n-1L)
            poly$x <- poly$x[sel]
            poly$y <- poly$y[sel]
        }
        poly
    })
}



######################
### coerce-methods ###
######################

##' @name coerce,Polygons,gpc.poly-method
##' @rdname coerce-methods
setAs(from = "Polygons", to = "gpc.poly", def = function (from)
    {
        gpclibCheck()
        pls <- slot(from, "Polygons")
        pts <- lapply(pls, function (sr) {
            coords <- coordinates(sr)
            n <- nrow(coords) - 1   # number of vertices
            list(x = coords[seq_len(n),1],
                 y = coords[seq_len(n),2],
                 hole = sr@hole)
        })
        new("gpc.poly", pts = pts)
    }
)

##' @name coerce,gpc.poly,Polygons-method
##' @rdname coerce-methods
setAs(from = "gpc.poly", to = "Polygons", def = function (from)
    {
        srl <- lapply(from@pts, function (poly) {
            if (isClosed(poly)) {
                sp::Polygon(cbind(poly$x,poly$y), hole = poly$hole)
            } else {
                sp::Polygon(cbind(c(poly$x,poly$x[1]), c(poly$y,poly$y[1])),
                            hole = poly$hole)
            }
        })
        sp::Polygons(srl, ID = "1")
    }
)

##' @name coerce,SpatialPolygons,gpc.poly-method
##' @rdname coerce-methods
## This method also applies to "SpatialPolygonsDataFrame" (inherited)
setAs(from = "SpatialPolygons", to = "gpc.poly", def = function (from)
    {
        gpclibCheck()
        polygonsList <- polygons(from)@polygons
        gpc <- new("gpc.poly")
        for (i in seq_along(polygonsList))
        {
            gpc <- gpclib::append.poly(gpc, as(polygonsList[[i]], "gpc.poly"))
        }
        gpc
    }
)

##' @name coerce,owin,gpc.poly-method
##' @rdname coerce-methods
setAs(from = "owin", to = "gpc.poly", def = function (from)
    {
        gpclibCheck()
        pts <- lapply(from$bdry, function (poly) {
            list(x = rev(poly$x), y = rev(poly$y), hole = poly$hole)
        })
        new("gpc.poly", pts = pts)
    }
)
