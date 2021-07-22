################################################################################
### Convert polygonal "sfg" to "gpc.poly" (for polyCub.exact.Gauss)
###
### Copyright (C) 2021 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


#' Convert polygonal \code{"sfg"} to \code{"gpc.poly"}
#'
#' Package \pkg{polyCub} implements a converter from class
#' \code{"\link[sf:st_polygon]{(MULTI)POLYGON}"} of package \pkg{sf} to
#' \code{"\link[rgeos:gpc.poly-class]{gpc.poly}"} of package \pkg{rgeos}
#' (originally from \pkg{gpclib}) such that \code{\link{polyCub.exact.Gauss}}
#' can be used with simple feature polygons.
#'
#' @param object a \code{"POLYGON"} or \code{"MULTIPOLYGON"} \code{"sfg"} object.
#' @return The converted polygon of class \code{"gpc.poly"}.
#' If neither package \pkg{rgeos} nor \pkg{gpclib} are available,
#' \code{sfg2gpc} will just return the \code{pts} slot of the
#' \code{"gpc.poly"} (no formal class) with a warning.
#' @author Sebastian Meyer
#' @note Package \pkg{rgeos} (or \pkg{gpclib}) is required for the formal class
#' definition of a \code{"gpc.poly"}.
#' @seealso \code{\link{xylist}}
#' @keywords spatial methods
#' @import methods
#' @export
#' @examples
#' if (require("rgeos") && require("sf")) withAutoprint({
#'
#'     ## use example polygons from
#'     example(plotpolyf, ask = FALSE)
#'
#'     letterR  # a simple "xylist"
#'     letterR.sfg <- st_polygon(lapply(letterR, function(xy)
#'         rbind(cbind(xy$x, xy$y), c(xy$x[1], xy$y[1]))))
#'     letterR.sfg
#'     stopifnot(identical(letterR, xylist(letterR.sfg)))
#'     \dontshow{
#'     stopifnot(identical(rep(letterR, 2),
#'         xylist(st_multipolygon(list(letterR.sfg, letterR.sfg)))))
#'     }
#'     ## convert sf "POLYGON" to a "gpc.poly"
#'     letterR.gpc_from_sfg <- sfg2gpc(letterR.sfg)
#'     letterR.gpc_from_sfg
#'     \dontshow{if (requireNamespace("spatstat.geom")) {
#'         letterR.xylist_from_gpc <- xylist(letterR.gpc_from_sfg) # with hole info
#'         stopifnot(identical(letterR, lapply(letterR.xylist_from_gpc, "[", 1:2)))
#'     }}
#' })
sfg2gpc <- function (object)
{
    assert_polygonal_sfg(object)

    ## determine hole flags of the individual polygons
    if (inherits(object, "MULTIPOLYGON")) {
        hole <- unlist(lapply(object, seq_along)) > 1L
        object <- unlist(object, recursive = FALSE)
    } else {
        hole <- seq_along(object) > 1L
    }

    pts <- mapply(
        FUN = function (coords, hole) {
            idx <- seq_len(nrow(coords) - 1L)
            list(x = coords[idx, 1L], y = coords[idx, 2L], hole = hole)
        },
        coords = object, hole = hole,
        SIMPLIFY = FALSE, USE.NAMES = FALSE)

    if (know_gpc.poly()) {
        new("gpc.poly", pts = pts)
    } else {
        warning("formal class \"gpc.poly\" not available")
        pts
    }
}

assert_polygonal_sfg <- function (object)
{
    if (!inherits(object, c("POLYGON", "MULTIPOLYGON")))
        stop("only *polygonal* SF geometries are supported")
    invisible(object)
}
