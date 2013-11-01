################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2012-2013 Sebastian Meyer
### Time-stamp: <[xylist.R] by SM Fre 01/11/2013 09:45 (CET)>
###
### Convert various polygon classes to a simple "xylist"
################################################################################


##' Convert Various Polygon Classes to a Simple List of Vertices
##' 
##' Different packages concerned with spatial data use different polygon
##' specifications, which sometimes becomes very confusing (see Details below).
##' To be compatible with the various polygon classes, package \pkg{polyCub}
##' uses an (internal) S3 class \code{"xylist"}, which represents
##' polygons by their core feature only, a list of lists of vertex coordinates
##' (see the "Value" section below).
##' The internal generic function \code{xylist} can deal with the
##' following polygon classes:
##' \itemize{
##' \item{\code{"\link[spatstat:owin.object]{owin}"} from package \pkg{spatstat}}
##' \item{\code{"\link[rgeos:gpc.poly-class]{gpc.poly}"} from package
##' \pkg{rgeos} (or \pkg{gpclib})}
##' \item{\code{"\link[sp:Polygons-class]{Polygons}"} from package \pkg{sp}
##' (as well as \code{"\link[sp:Polygon-class]{Polygon}"} and
##' \code{"\link[sp:SpatialPolygons-class]{SpatialPolygons}"})}
##' }
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
##' coordinates, which give the coordinates of the vertices of the polygon
##' following \pkg{spatstat}'s \code{"owin"} convention
##' (anticlockwise order without repeating any vertex).
##' Additional elements like \code{"area"} and \code{"hole"} in each
##' component are retained. The (somehow useless) default \code{xylist}-method
##' does not perform any transformation but only checks that the polygons are
##' not closed (first vertex not repeated).
##' @author Sebastian Meyer\cr 
##' The implementation of the \code{"gpc.poly"}-method of \code{xylist}
##' depends on functionality of the \pkg{spatstat} package and borrows
##' large parts from the function \code{gpc2owin} (as implemented in package
##' \pkg{spatstat} before version 1.34-0, when support for \code{"gpc.poly"} was
##' dropped) authored by Adrian Baddeley and Rolf Turner.
##' @name xylist
##' @keywords spatial methods
xylist <- function (object, ...) UseMethod("xylist")

##' @method xylist owin
##' @rdname xylist
##' @importFrom spatstat is.polygonal
xylist.owin <- function (object, ...) {
    if (is.polygonal(object)) object$bdry else {
        stop("object is not polygonal")
    }
}

##' @method xylist gpc.poly
##' @rdname xylist
##' @importFrom spatstat area.xypolygon reverse.xypolygon
xylist.gpc.poly <- function (object, ...)
{
    pts <- object@pts
    processpolygon <- function (p) {
        area <- area.xypolygon(p)
        neg <- (area < 0)
        if (p$hole != neg) {
            p <- reverse.xypolygon(p)
            area <- -area
        }
        p$area <- area
        return(p)
    }
    pts <- lapply(pts, processpolygon)
    return(pts)
}

##' @method xylist SpatialPolygons
##' @rdname xylist
xylist.SpatialPolygons <- function (object, ...)
{
    unlist(lapply(object@polygons, xylist.Polygons),
           recursive=FALSE, use.names=FALSE)
}
    
##' @method xylist Polygons
##' @rdname xylist
xylist.Polygons <- function (object, ...)
{
      pls <- object@Polygons
      lapply(pls, function (sr) {
          coords <- coordinates(sr)
          n <- nrow(coords) - 1   # number of vertices
          list(x = coords[seq.int(n,1),1], # reverse direction to get
               y = coords[seq.int(n,1),2], # "xylist"/"owin" convention
               hole = sr@hole,
               area = sr@area)
      })
}

##' @method xylist Polygon
##' @rdname xylist
xylist.Polygon <- function (object, ...)
    xylist.Polygons(as(object,"Polygons"))

##' @method xylist default
##' @rdname xylist
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


## @name coerce,Polygon,Polygons-method
## @exportMethod coerce
## @rdname xylist
setAs(from = "Polygon", to = "Polygons",
      def = function (from) Polygons(list(from), "Polygon"))
