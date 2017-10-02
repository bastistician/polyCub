################################################################################
### as.owin.SpatialPolygons: Coerce "SpatialPolygons" to "owin"
###
### Copyright (C) 2012-2013,2015,2017 Sebastian Meyer
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
##' @exportMethod coerce
NULL

##' @param W an object of class \code{"SpatialPolygons"},
##' \code{"Polygons"}, or \code{"Polygon"}.
##' @param ... further arguments passed to \code{\link[spatstat]{owin}}.
##' @rdname coerce-sp-methods
##' @export
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


## Register "owin" as class in S4 so we can define methods for it
##setClass("owin")
## -> no need to register "owin", since we depend on sp which does it !
## Otherwise we would get the following warning upon package installation:
## Warning in .simpleDuplicateClass(def, prev) :
##   the specification for class "owin" in package 'polyCub' seems
##   equivalent to one from package 'sp' and is not turning on
##   duplicate class definitions for this class
## Using setOldClass("owin") is incompatible with package "maptools", which
## does setClass("owin") _and_ exports this class! Specifically, loading
## library("polyCub"); library("maptools"); library("gpclib")
## in this order would not work (no idea why) throwing:
## Error : package slot missing from signature for generic 'plot'
## and classes gpc.poly, ANY
## cannot use with duplicate class names (the package may need to be
## re-installed)
## Error: package/namespace load failed for 'gpclib'

##' @importClassesFrom sp owin
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
