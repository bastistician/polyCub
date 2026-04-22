################################################################################
### as.owin.SpatialPolygons: Coerce "SpatialPolygons" to "owin"
###
### Copyright (C) 2012-2013,2015,2017-2018,2021 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


#' Coerce \code{"SpatialPolygons"} to \code{"owin"}
#'
#' Package \pkg{polyCub} implements \code{coerce}-methods
#' (\code{as(object, Class)}) to convert
#' \code{"\link[sp:SpatialPolygons-class]{SpatialPolygons}"}
#' (or \code{"\link[sp:Polygons-class]{Polygons}"}
#' or \code{"\link[sp:Polygon-class]{Polygon}"})
#' of package \CRANpkg{sp}
#' to \code{"\link[spatstat.geom:owin.object]{owin}"}
#' of package \CRANpkg{spatstat.geom}.
#' They are also available as \code{as.owin.*} functions to support
#' \code{\link{polyCub.midpoint}}.
#' @author Sebastian Meyer
#' @seealso \code{\link{xylist}}
#' @keywords spatial methods
#' @name coerce-sp-methods
#' @rdname coerce-sp-methods
#' @import methods
#' @importClassesFrom sp Polygon Polygons SpatialPolygons owin
#' @exportMethod coerce
#' @example examples/diamond.R
#' @examples
#' if (require("spatstat.geom")) {
#'     diamond.owin <- owin(poly = diamond)
#'     diamond.owin_from_Polygon <- as.owin(diamond.sp)
#'     stopifnot(all.equal(diamond.owin, diamond.owin_from_Polygon))
#'     ## also for "Polygons" and "SpatialPolygons", using S3 or S4 methods:
#'     stopifnot(identical(diamond.owin, as.owin(diamond.Ps)))
#'     stopifnot(identical(diamond.owin, as(diamond.SpPs, "owin")))
#' }
NULL

#' @param W an object of class \code{"SpatialPolygons"},
#' \code{"Polygons"}, or \code{"Polygon"}.
#' @param ... further arguments passed to \code{\link[spatstat.geom]{owin}}.
#' @return The polygon(s) as an
#' \code{"\link[spatstat.geom:owin.object]{owin}"} object.
#' @rdname coerce-sp-methods
#' @export
#' @rawNamespace if(getRversion() >= "3.6.0") {  # delayed registration
#'     S3method(spatstat.geom::as.owin, SpatialPolygons)
#'     S3method(spatstat.geom::as.owin, Polygons)
#'     S3method(spatstat.geom::as.owin, Polygon)
#' }
as.owin.SpatialPolygons <- function (W, ...)
    spatstat.geom::owin(poly = xylist.SpatialPolygons(W), ...)

#' @rdname coerce-sp-methods
#' @export
as.owin.Polygons <- function (W, ...)
    spatstat.geom::owin(poly = xylist.Polygons(W), ...)

#' @rdname coerce-sp-methods
#' @export
as.owin.Polygon <- function (W, ...)
    spatstat.geom::owin(poly = xylist.Polygon(W), ...)


#' @name coerce,SpatialPolygons,owin-method
#' @rdname coerce-sp-methods
setAs(from = "SpatialPolygons", to = "owin",
      def = function (from) as.owin.SpatialPolygons(from))

#' @name coerce,Polygons,owin-method
#' @rdname coerce-sp-methods
setAs(from = "Polygons", to = "owin",
      def = function (from) as.owin.Polygons(from))

#' @name coerce,Polygon,owin-method
#' @rdname coerce-sp-methods
setAs(from = "Polygon", to = "owin",
      def = function (from) as.owin.Polygon(from))


#' @name coerce,Polygon,Polygons-method
#' @rdname coerce-sp-methods
setAs(from = "Polygon", to = "Polygons",
      def = function (from) sp::Polygons(list(from), "Polygon"))
