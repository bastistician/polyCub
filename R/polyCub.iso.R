################################################################################
### polyCub.iso: Cubature of Isotropic Functions over Polygonal Domains
###
### Copyright (C) 2013-2018 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


#' Cubature of Isotropic Functions over Polygonal Domains
#'
#' \code{polyCub.iso} numerically integrates a radially symmetric function
#' \eqn{f(x,y) = f_r(||(x,y)-\boldsymbol{\mu}||)}{f(x,y) = f_r(||(x,y)-\mu||)},
#' with \eqn{\mu} being the center of isotropy, over a polygonal domain.
#' It internally approximates a line integral along the polygon boundary using
#' \code{\link{integrate}}. The integrand requires the antiderivative of
#' \eqn{r f_r(r)}), which should be supplied as argument \code{intrfr}
#' (\code{f} itself is only required if \code{check.intrfr=TRUE}).
#' The two-dimensional integration problem thereby reduces to an efficient
#' adaptive quadrature in one dimension.
#' If \code{intrfr} is not available analytically, \code{polyCub.iso} can use a
#' numerical approximation (meaning \code{integrate} within \code{integrate}),
#' but the general-purpose cubature method \code{\link{polyCub.SV}} might be
#' more efficient in this case.
#' See Meyer and Held (2014, Supplement B, Section 2.4) for mathematical
#' details.
#'
#' @inheritParams plotpolyf
#' @param intrfr a \code{function(R, ...)}, which implements the (analytical)
#' antiderivative of \eqn{r f_r(r)} from 0 to \code{R}. The first argument
#' must be vectorized but not necessarily named \code{R}.\cr
#' If \code{intrfr} is missing, it will be approximated numerically via
#' \code{\link{integrate}(function(r, ...)
#' r * f(cbind(x0 + r, y0), ...), 0, R, ..., control=control)},
#' where \code{c(x0, y0)} is the \code{center} of isotropy.
#' Note that \code{f} will \emph{not} be checked for isotropy.
#' @param ... further arguments for \code{f} or \code{intrfr}.
#' @param center numeric vector of length 2, the center of isotropy.
#' @param control list of arguments passed to \code{\link{integrate}}, the
#' quadrature rule used for the line integral along the polygon boundary.
#' @param check.intrfr logical (or numeric vector) indicating if
#' (for which \code{r}'s) the supplied \code{intrfr} function should be
#' checked against a numeric approximation. This check requires \code{f}
#' to be specified. If \code{TRUE}, the set of test
#' \code{r}'s defaults to a \code{\link{seq}} of length 20 from 1 to
#' the maximum absolute x or y coordinate of any edge of the \code{polyregion}.
#' @param plot logical indicating if an image of the function should be plotted
#' together with the polygonal domain, i.e.,
#' \code{\link{plotpolyf}(polyregion, f, \dots)}.
#' @return The approximate integral of the isotropic function
#' \code{f} over \code{polyregion}.\cr
#' If the \code{intrfr} function is provided (which is assumed to be exact), an
#' upper bound for the absolute integration error is appended to the result as
#' attribute \code{"abs.error"}. It equals the sum of the absolute errors
#' reported by all \code{\link{integrate}} calls
#' (there is one for each edge of \code{polyregion}).
#' @author Sebastian Meyer
#'
#' The basic mathematical formulation of this efficient integration for radially
#' symmetric functions was ascertained with great support by
#' Emil Hedevang (2013), Dept. of Mathematics, Aarhus University, Denmark.
#' @references
#' Hedevang, E. (2013). Personal communication at the Summer School on Topics in
#' Space-Time Modeling and Inference (May 2013, Aalborg, Denmark).
#'
#' Meyer, S. and Held, L. (2014).
#' Power-law models for infectious disease spread.
#' \emph{The Annals of Applied Statistics}, \bold{8} (3), 1612-1639.\cr
#' DOI-Link: \url{https://doi.org/10.1214/14-AOAS743},
#' \href{https://arxiv.org/abs/1308.5115}{arXiv:1308.5115}
#' @seealso
#' \code{system.file("include", "polyCubAPI.h", package = "polyCub")}
#' for a full C-implementation of this cubature method (for a \emph{single}
#' polygon). The corresponding C-routine \code{polyCub_iso} can be used by
#' other \R packages, notably \pkg{surveillance}, via \samp{LinkingTo: polyCub}
#' (in the \file{DESCRIPTION}) and \samp{#include <polyCubAPI.h>} (in suitable
#' \file{/src} files). Note that the \code{intrfr} function must then also be
#' supplied as a C-routine. An example can be found in the package tests.
#' @keywords math spatial
#' @family polyCub-methods
#' @example examples/polyCub.iso.R
#' @importFrom stats integrate
#' @export

polyCub.iso <- function (polyregion, f, intrfr, ..., center,
                         control = list(), check.intrfr = FALSE, plot = FALSE)
{
    polys <- xylist(polyregion) # transform to something like "owin$bdry"
                                # which means anticlockwise vertex order with
                                # first vertex not repeated
    getError <- !missing(intrfr) # can't estimate error of double approximation
    center <- as.vector(center, mode = "numeric")
    stopifnot(length(center) == 2L, is.finite(center))

    ## check 'intrfr' function
    rs <- if (isTRUE(check.intrfr)) {
        seq(1, max(abs(unlist(lapply(polys, "[", c("x","y"))))), length.out=20L)
    } else if (identical(check.intrfr, FALSE)) {
        numeric(0L)
    } else {
        check.intrfr
    }
    intrfr <- checkintrfr(intrfr, f, ..., center=center, control=control, rs=rs)

    ## plot polygon and function image
    if (plot) plotpolyf(polys, f, ...)

    ## do the cubature over all polygons of the 'polys' list
    .polyCub.iso(polys, intrfr, ..., center=center,
                 control=control, .witherror=getError)
}


##' Check the Integral of \eqn{r f_r(r)}
##'
##' This function is auxiliary to \code{\link{polyCub.iso}}.
##' The (analytical) integral of \eqn{r f_r(r)} from 0 to \eqn{R} is checked
##' against a numeric approximation using \code{\link{integrate}} for various
##' values of the upper bound \eqn{R}. A warning is issued if inconsistencies
##' are found.
##'
##' @inheritParams polyCub.iso
##' @param rs numeric vector of upper bounds for which to check the validity of
##' \code{intrfr}. If it has length 0 (default), no checks are performed.
##' @param tolerance of \code{\link{all.equal.numeric}} when comparing
##' \code{intrfr} results with numerical integration. Defaults to the
##' relative tolerance used for \code{integrate}.
##' @return The \code{intrfr} function. If it was not supplied, its quadrature
##' version using \code{integrate} is returned.
##' @importFrom stats integrate
##' @export
checkintrfr <- function (intrfr, f, ..., center, control = list(),
                         rs = numeric(0L), tolerance = control$rel.tol)
{
    doCheck <- length(rs) > 0L
    if (!missing(f)) {
        f <- match.fun(f)
        rfr <- function (r, ...)
            r * f(cbind(center[1L]+r, center[2L], deparse.level=0L), ...)
        quadrfr1 <- function (R, ...) integrate(rfr, 0, R, ...)$value
        if (length(control))
            body(quadrfr1)[[2L]] <- as.call(c(as.list(body(quadrfr1)[[2L]]),
                                             control))
        quadrfr <- function (R, ...)
            vapply(X = R, FUN = quadrfr1, FUN.VALUE = 0, ..., USE.NAMES = FALSE)
        if (missing(intrfr)) {
            return(quadrfr)
        } else if (doCheck) {
            cat("Checking 'intrfr' against a numeric approximation ... ")
            stopifnot(is.vector(rs, mode="numeric"))
            if (is.null(tolerance))
                tolerance <- eval(formals(integrate)$rel.tol)
            ana <- intrfr(rs, ...)
            num <- quadrfr(rs, ...)
            if (!isTRUE(comp <- all.equal(num, ana, tolerance=tolerance))) {
                cat("\n->", comp, "\n")
                warning("'intrfr' might be incorrect: ", comp)
            } else cat("OK\n")
        }
    } else if (doCheck) {
        stop("numerical verification of 'intrfr' requires 'f'")
    }

    match.fun(intrfr)
}


##' @description
##' \code{.polyCub.iso} is a \dQuote{bare-bone} version of \code{polyCub.iso}.
##' @rdname polyCub.iso
##' @param polys something like \code{owin$bdry}, but see \code{\link{xylist}}.
##' @param .witherror logical indicating if an upper bound for the absolute
##' integration error should be attached as an attribute to the result?
##' @export
.polyCub.iso <- function (polys, intrfr, ..., center,
                          control = list(), .witherror = FALSE)
{
    ints <- lapply(polys, polyCub1.iso,
                   intrfr, ..., center=center,
                   control=control, .witherror=.witherror)
    if (.witherror) {
        res <- sum(vapply(X=ints, FUN="[", FUN.VALUE=0, 1L, USE.NAMES=FALSE))
        attr(res, "abs.error") <-
            sum(vapply(X=ints, FUN="[", FUN.VALUE=0, 2L, USE.NAMES=FALSE))
        res
    } else {
        sum(unlist(ints, recursive=FALSE, use.names=FALSE))
    }
}

## cubature method for a single polygon
polyCub1.iso <- function (poly, intrfr, ..., center,
                          control = list(), .witherror = TRUE)
{
    xy <- cbind(poly[["x"]], poly[["y"]], deparse.level=0L)
    nedges <- nrow(xy)
    intedges <- erredges <- numeric(nedges)
    for (i in seq_len(nedges)) {
        v0 <- xy[i, ] - center
        v1 <- xy[if (i==nedges) 1L else i+1L, ] - center
        int <- lineInt(v0, v1, intrfr, ..., control=control)
        intedges[i] <- int$value
        erredges[i] <- int$abs.error
    }
    int <- sum(intedges)
    ## if (!is.null(poly$hole) && !isTRUE(all.equal(0, int))) {
    ##     if ((1 - 2 * as.numeric(poly$hole)) * sign(int) == -1)
    ##         warning("wrong sign if positive integral")
    ## }
    if (.witherror) {
        c(int, sum(erredges))
    } else {
        int
    }
}

## line integral for one edge
##' @importFrom stats integrate
lineInt <- function (v0, v1, intrfr, ..., control = list())
{
    d <- v1 - v0
    num <- v1[2L]*v0[1L] - v1[1L]*v0[2L]  # = d[2]*p[,1] - d[1]*p[,2]
                                          # for any point p on the edge
    if (num == 0) { # i.e., if 'center' is part of this polygon edge
        return(list(value = 0, abs.error = 0))
    }
    integrand <- function (t) {
        ## get the points on the edge corresponding to t
        p <- cbind(v0[1L] + t*d[1L], v0[2L] + t*d[2L], deparse.level=0L)
        norm2 <- .rowSums(p^2, length(t), 2L)
        ints <- intrfr(sqrt(norm2), ...)
        ##ints[is.infinite(ints)] <- 1e300
        num * ints / norm2
    }
    if (length(control)) {              # use slower do.call()-construct
        do.call("integrate", c(list(integrand, 0, 1), control))
    } else {
        integrate(integrand, 0, 1)
    }
}
