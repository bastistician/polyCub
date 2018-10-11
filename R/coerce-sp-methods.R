################################################################################
### as.owin.SpatialPolygons: Coerce "SpatialPolygons" to "owin"
###
### Copyright (C) 2012-2013,2015,2017-2018 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


##' Coerce \code{"SpatialPolygons"} to \code{"owin"}
##'
##' Package \pkg{polyCub} implements \code{coerce}-methods
##' (\code{as(object, Class)}) to convert \code{"\linkS4class{SpatialPolygons}"}
##' (or \code{"\linkS4class{Polygons}"} or \code{"\linkS4class{Polygon}"})
##' to \code{"\link[spatstat:owin.object]{owin}"}.
##' They are also available as \code{as.owin.*} functions to support
##' \code{\link{polyCub.midpoint}}. However, these are no registered S3 methods
##' for \code{\link[spatstat]{as.owin}}, since package \pkg{spatstat} is
##' optional.
##' Note that the \pkg{maptools} package contains an alternative implementation
##' of coercion from \code{"SpatialPolygons"} to \code{"owin"} (and reverse),
##' and \R will use the S4 \code{coerce}-method that was loaded last,
##' and prefer the \code{as.owin.SpatialPolygons} S3-method exported from
##' \pkg{maptools} if attached.
##' @author Sebastian Meyer
##' @keywords spatial methods
##' @name coerce-sp-methods
##' @rdname coerce-sp-methods
##' @import methods
##' @importClassesFrom sp Polygon Polygons SpatialPolygons owin
##' @exportMethod coerce
##' @examples
##' if (require("spatstat") && require("sp")) {
##'     diamond <- list(x = c(1,2,1,0), y = c(1,2,3,2))  # anti-clockwise
##'     diamond.owin <- owin(poly = diamond)
##'     diamond.sp <- Polygon(lapply(diamond, rev))      # clockwise
##'     diamond.owin_from_sp <- as(diamond.sp, "owin")
##'     stopifnot(all.equal(diamond.owin, diamond.owin_from_sp))
##'
##'     ## similarly works for Polygons and SpatialPolygons
##'     diamond.Ps <- as(diamond.sp, "Polygons")
##'     stopifnot(identical(diamond.owin, as.owin(diamond.Ps)))
##'     diamond.SpPs <- SpatialPolygons(list(diamond.Ps))
##'     stopifnot(identical(diamond.owin, as.owin(diamond.SpPs)))
##' }
NULL

##' @param W an object of class \code{"SpatialPolygons"},
##' \code{"Polygons"}, or \code{"Polygon"}.
##' @param ... further arguments passed to \code{\link[spatstat]{owin}}.
##' @rdname coerce-sp-methods
##' @export
##' @rawNamespace if(getRversion() >= "3.6.0") {  # delayed registration
##'     S3method(spatstat::as.owin, SpatialPolygons)
##'     S3method(spatstat::as.owin, Polygons)
##'     S3method(spatstat::as.owin, Polygon)
##' }
as.owin.SpatialPolygons <- function (W, ...)
    spatstat::owin(poly = xylist.SpatialPolygons(W), ...)

##' @rdname coerce-sp-methods
##' @export
as.owin.Polygons <- function (W, ...)
    spatstat::owin(poly = xylist.Polygons(W), ...)

##' @rdname coerce-sp-methods
##' @export
as.owin.Polygon <- function (W, ...)
    spatstat::owin(poly = xylist.Polygon(W), ...)


##' @name coerce,SpatialPolygons,owin-method
##' @rdname coerce-sp-methods
setAs(from = "SpatialPolygons", to = "owin",
      def = function (from) as.owin.SpatialPolygons(from))

##' @name coerce,Polygons,owin-method
##' @rdname coerce-sp-methods
setAs(from = "Polygons", to = "owin",
      def = function (from) as.owin.Polygons(from))

##' @name coerce,Polygon,owin-method
##' @rdname coerce-sp-methods
setAs(from = "Polygon", to = "owin",
      def = function (from) as.owin.Polygon(from))


##' @name coerce,Polygon,Polygons-method
##' @rdname coerce-sp-methods
setAs(from = "Polygon", to = "Polygons",
      def = function (from) Polygons(list(from), "Polygon"))
