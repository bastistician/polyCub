################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2012-2013 Sebastian Meyer
### Time-stamp: <[coerce-methods.R] by SM Sam 06/07/2013 13:40 (CEST)>
###
### Some coerce-methods between different spatial classes
################################################################################


##' Polygon Coercion Between Different Spatial Classes
##' 
##' Different packages concerned with spatial data use different polygon
##' specifications. 
##' The package \pkg{polyCub} offers \code{coerce}-methods
##' (\code{as(object, Class)}) to convert
##' \code{"\link[sp:Polygons-class]{Polygons}"} of the \pkg{sp} package,
##' and \code{"\link[spatstat:owin.object]{owin}"} objects of the
##' \pkg{spatstat} package to and from
##' the \code{"\link[rgeos:gpc.poly-class]{gpc.poly}"} class of the
##' \pkg{rgeos} package (originally from \pkg{gpclib}), as well as
##' from \code{"\link[sp:Polygon-class]{Polygon}"} to \code{"gpc.poly"}.
##' Note that conversions from and to the \code{"owin"}
##' class are also available as functions \code{\link[spatstat]{owin2gpc}} and
##' \code{\link[spatstat]{gpc2owin}} in package \pkg{spatstat},
##' and conversions from and to the
##' \code{"\link[sp:SpatialPolygons-class]{SpatialPolygons}"} class are included
##' in the \pkg{rgeos} package.
##' Furthermore, the (internal) \code{xylist} methods break down 
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
##' The \code{coerce}-methods defined here produce vertices
##' ordered according to the \pkg{sp} convention, i.e. clockwise for
##' normal boundaries and anticlockwise for holes; however, the first vertex
##' is \emph{not} repeated!}
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
##' @author Sebastian Meyer;
##' the implementation of the \code{"gpc.poly"}-method of \code{xylist} borrows
##' large parts from \code{\link[spatstat]{gpc2owin}} and depends on
##' functionality of the \pkg{spatstat} package
##' authored by Adrian Baddeley and Rolf Turner.
##' @name coerce-methods
##' @aliases xylist
##' @importClassesFrom rgeos gpc.poly
##' @importMethodsFrom rgeos coerce
##' @exportMethod coerce
##' @keywords spatial methods
xylist <- function (object, ...) UseMethod("xylist")

##' @method xylist owin
##' @rdname coerce-methods
xylist.owin <- function (object, ...) object$bdry

##' @method xylist gpc.poly
##' @rdname coerce-methods
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
##' @rdname coerce-methods
xylist.SpatialPolygons <- function (object, ...)
    xylist.gpc.poly(as(object,"gpc.poly"))

##' @method xylist Polygons
##' @rdname coerce-methods
xylist.Polygons <- function (object, ...)
    xylist.gpc.poly(as(object,"gpc.poly"))

##' @method xylist Polygon
##' @rdname coerce-methods
xylist.Polygon <- function (object, ...)
    xylist.gpc.poly(as(object,"gpc.poly"))

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

##' @name coerce,Polygon,gpc.poly-method
##' @rdname coerce-methods
setAs(from = "Polygon", to = "gpc.poly", def = function (from)
  {
      as(Polygons(list(from), "ID"), "gpc.poly")
  }
      )


##' @name coerce,gpc.poly,Polygons-method
##' @rdname coerce-methods
setAs(from = "gpc.poly", to = "Polygons", def = function (from)
  {
      srl <- lapply(from@pts, function (poly) {
          if (isClosed(poly)) {
              Polygon(cbind(poly$x,poly$y), hole = poly$hole)
          } else {
              Polygon(cbind(c(poly$x,poly$x[1]), c(poly$y,poly$y[1])),
                      hole = poly$hole)
          }
      })
      Polygons(srl, ID = "1")
  }
      )


## Register "owin" as class in S4 so we can define methods for it
#setClass("owin")
## -> no need to register "owin", since we depend on sp which does it !
## Otherwise we would get the following warning upon package installation:
## Warning in .simpleDuplicateClass(def, prev) :
##   the specification for class "owin" in package 'polyCub' seems equivalent to
##   one from package 'sp' and is not turning on duplicate class definitions for
##   this class 
## Using setOldClass("owin") is incompatible with package "maptools", which
## does setClass("owin") _and_ exports this class! Specifically, loading
## library("polyCub"); library("maptools"); library("gpclib")
## in this order would not work (no idea why) throwing:
## Error : package slot missing from signature for generic ‘plot’
## and classes gpc.poly, ANY
## cannot use with duplicate class names (the package may need to be re-installed)
## Error: package/namespace load failed for ‘gpclib’

##' @name coerce,owin,gpc.poly-method
##' @rdname coerce-methods
##' @importFrom spatstat as.polygonal is.hole.xypolygon
setAs(from = "owin", to = "gpc.poly", def = function (from)
  {
      x <- as.polygonal(from)
      pts <- lapply(x$bdry, function (poly) {
          list(x = rev(poly$x), y = rev(poly$y),
               hole = is.hole.xypolygon(poly))
      })
      new("gpc.poly", pts = pts)
  }
      )


##' @name coerce,gpc.poly,owin-method
##' @rdname coerce-methods
##' @importFrom rgeos get.bbox
##' @importFrom spatstat owin
setAs(from = "gpc.poly", to = "owin", def = function (from)
  {
      pts <- xylist.gpc.poly(from)
      bb <- get.bbox(from)
      w <- owin(bb$x, bb$y, poly = pts, check = FALSE)
      return(w)
  }
      )
