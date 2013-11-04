################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2012-2013 Sebastian Meyer
### Time-stamp: <[coerce-sp-methods.R] by SM Mon 04/11/2013 22:02 (CET)>
###
### Coerce "Polygons" to and from "owin"
################################################################################


##' Coerce \code{"Polygons"} to \code{"owin"}
##' 
##' Package \pkg{polyCub} also implements \code{coerce}-methods
##' (\code{as(object, Class)}) to convert
##' \code{"\link[sp:Polygons-class]{Polygons}"}
##' (or \code{"\link[sp:SpatialPolygons-class]{SpatialPolygons}"} or
##' \code{"\link[sp:Polygon-class]{Polygon}"}) to
##' \code{"\link[spatstat:owin.object]{owin}"}.
##' @author Sebastian Meyer
##' @keywords spatial methods
##' @name coerce-sp-methods
##' @rdname coerce-sp-methods
##' @exportMethod coerce
NULL

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
## Error : package slot missing from signature for generic ‘plot’
## and classes gpc.poly, ANY
## cannot use with duplicate class names (the package may need to be
## re-installed)
## Error: package/namespace load failed for ‘gpclib’

##' @name coerce,SpatialPolygons,owin-method
##' @rdname coerce-sp-methods
##' @importFrom spatstat owin
setAs(from = "SpatialPolygons", to = "owin",
      def = function (from) owin(poly=xylist.SpatialPolygons(from)))

##' @name coerce,Polygons,owin-method
##' @rdname coerce-sp-methods
##' @importFrom spatstat owin
setAs(from = "Polygons", to = "owin",
      def = function (from) owin(poly=xylist.Polygons(from)))

##' @name coerce,Polygon,owin-method
##' @rdname coerce-sp-methods
##' @importFrom spatstat owin
setAs(from = "Polygon", to = "owin",
      def = function (from) owin(poly=xylist.Polygon(from)))


##' @name coerce,Polygon,Polygons-method
##' @rdname coerce-sp-methods
setAs(from = "Polygon", to = "Polygons",
      def = function (from) Polygons(list(from), "Polygon"))
