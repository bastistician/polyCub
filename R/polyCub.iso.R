################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2013-2014 Sebastian Meyer
### Time-stamp: <[polyCub.iso.R] by SM Mit 12/02/2014 11:48 (CET)>
################################################################################


#' Cubature of Isotropic Functions over Polygonal Domains
#'
#' Conducts numerical integration of a two-dimensional isotropic function
#' \eqn{f(x,y) = f_r(||(x,y)-\boldsymbol{\mu}||)}{f(x,y) = f_r(||(x,y)-\mu||)},
#' with \eqn{\mu} being the center of isotropy, over a polygonal domain. 
#' It internally solves a line integral along the polygon boundary using
#' \code{\link{integrate}} where the integrand requires the antiderivative of
#' \eqn{r f_r(r)}), which ideally is analytically available and supplied to the
#' function as argument \code{intrfr}.
#' The two-dimensional integration problem thereby reduces to an efficient
#' adaptive quadrature in one dimension.
#' See Meyer and Held (2014, Section 2.4 of Supplement B) for mathematical
#' details.
#'
#' @inheritParams polyCub.SV
#' @param intrfr analytical antiderivative of \eqn{r f_r(r)} from 0 to \code{R}
#' (first argument, not necessarily named \code{"R"}, must be vectorized).
#' If given, \code{f} is not required (except for plotting)!
#' If missing, \code{intrfr} is approximated numerically, again using
#' \code{\link{integrate}}.
#' @param ... further arguments for \code{f} or \code{intrfr}.
#' @param center numeric vector of length 2, the center of isotropy.
#' @param control list of arguments passed to \code{\link{integrate}}, the
#' quadrature rule used for the line integral along the polygon boundary.
#' @param check.intrfr logical (or numeric vector) indicating if
#' (for which \code{r}'s) the supplied \code{intrfr} function should be
#' checked against a numeric approximation. If \code{TRUE}, the set of test
#' \code{r}'s defaults to a \code{\link{seq}} of length 20 from 1 to
#' the maximum absolute x or y coordinate of any edge of the \code{polyregion}.
#' @return The approximate integral of the isotropic function
#' \code{f} over \code{polyregion}.\cr
#' If the \code{intrfr} function is provided (which is assumed to be exact), an
#' upperbound for the absolute integration error is appended to the result as
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
#' Available from
#' \url{http://www.biostat.uzh.ch/research/manuscripts/powerlaw.html}.
#' @keywords math spatial
#' @family polyCub-methods
#' @example inst/examples/polyCub.iso.R
#' @importFrom stats integrate
#' @export

polyCub.iso <- function (polyregion, f, intrfr, ..., center,
                         control = list(), check.intrfr = FALSE, plot = FALSE)
{
    polys <- xylist(polyregion) # transform to something like "owin$bdry"
                                # which means anticlockwise vertex order with
                                # first vertex not repeated
    getError <- !missing(intrfr) # can't estimate error of double approximation

    ## check 'intrfr' function
    rs <- if (isTRUE(check.intrfr)) {
        seq(1, max(abs(unlist(lapply(polys, "[", c("x","y"))))), length.out=20L)
    } else if (identical(check.intrfr, FALSE)) {
        numeric(0L)
    } else check.intrfr
    intrfr <- checkintrfr(intrfr, f, ..., center=center, rs=rs)

    ## plot polygon and function image
    if (plot) plotpolyf(polys, f, ...)
    
    ## do the cubature over all polygons of the 'polys' list
    .polyCub.iso(polys, intrfr, ..., center=center,
                 control=control, .witherror=getError)
}

## check the supplied 'intrfr' function
checkintrfr <- function (intrfr, f, ..., center, rs = numeric(0L))
{
    doCheck <- length(rs) > 0L
    if (!missing(f)) {
        f <- match.fun(f)
        rfr <- function (r, ...) r * f(cbind(center[1]+r,center[2],deparse.level=0), ...)
        quadrfr1 <- function (r, ...) integrate(rfr, 0, r, ...)$value
        quadrfr <- function (r, ...) sapply(r, quadrfr1, ..., USE.NAMES=FALSE)
        if (missing(intrfr)) {
            return(quadrfr)
        } else if (doCheck) {
            cat("Checking 'intrfr' against a numeric approximation ... ")
            stopifnot(is.vector(rs, mode="numeric"))
            ana <- intrfr(rs, ...)
            num <- quadrfr(rs, ...)
            if (!isTRUE(comp <- all.equal(num, ana))) {
                cat("\n->", comp, "\n")
                warning("'intrfr' might be incorrect: ", comp)
            } else cat("OK\n")
        }
    } else if (doCheck)
        stop("numerical verification of 'intrfr' requires 'f'")
    
    match.fun(intrfr)
}


##' \code{.polyCub.iso} is a \dQuote{bare-bone} version of \code{polyCub.iso}.
##' @rdname polyCub.iso
##' @param polys something like \code{owin$bdry}, but see \code{\link{xylist}}.
##' @param .witherror logical indicating if an upperbound for the absolute
##' integration error should be attached as an attribute to the result?
##' @export
.polyCub.iso <- function (polys, intrfr, ..., center,
                          control = list(), .witherror = FALSE)
{
    ints <- lapply(polys, polyCub1.iso,
                   intrfr, ..., center=center,
                   control=control, .witherror=.witherror)
    if (.witherror) {
        structure(sum(sapply(ints, "[", 1, simplify=TRUE, USE.NAMES=FALSE)),
                  abs.error=sum(sapply(ints, "[", 2,
                  simplify=TRUE, USE.NAMES=FALSE)))
    } else sum(unlist(ints, recursive=FALSE, use.names=FALSE))
}

## cubature method for a single polygon
polyCub1.iso <- function (poly, intrfr, ..., center, control, .witherror = TRUE)
{
    xy <- cbind(poly[["x"]], poly[["y"]], deparse.level=0)
    nedges <- nrow(xy)
    intedges <- erredges <- numeric(nedges)
    for (i in seq_len(nedges)) {
        v0 <- xy[i,] - center
        v1 <- xy[if (i==nedges) 1L else i+1L,] - center
        int <- lineInt(v0, v1, intrfr, ..., control=control)
        intedges[i] <- int$value
        erredges[i] <- int$abs.error
    }
    int <- sum(intedges)
    ## if (!is.null(poly$hole) && !isTRUE(all.equal(0, int))) {
    ##     if ((1 - 2 * as.numeric(poly$hole)) * sign(int) == -1)
    ##         warning("wrong sign if positive integral")
    ## }
    c(int, if (.witherror) sum(erredges))
}

## line integral for one edge
##' @importFrom stats integrate
lineInt <- function (v0, v1, intrfr, ..., control)
{
    d <- v1 - v0
    num <- v1[2]*v0[1] - v1[1]*v0[2]  # = d[2]*p[,1] - d[1]*p[,2]
                                      # for any point p on the edge
    integrand <- function (t) {
        ## get the points on the edge corresponding to t
        p <- cbind(v0[1] + t*d[1], v0[2] + t*d[2], deparse.level=0)
        norm2 <- .rowSums(p^2, length(t), 2)
        ints <- intrfr(sqrt(norm2), ...)
        ##ints[is.infinite(ints)] <- 1e300
        num * ints / norm2
    }
    if (length(control)) {              # use slower do.call()-construct
        do.call("integrate", c(list(integrand, 0, 1), control=control))
    } else integrate(integrand, 0, 1)
}

## equally fast method _only_ for convex polygonal domains including the origin
## (formula obtained via polar coordinate representation)
lineInt2 <- function (v0, v1, intrfr, ..., control)
{
    d <- v1 - v0
    ld <- vecnorm(d)
    l0 <- vecnorm(v0)
    l1 <- vecnorm(v1)
    dp <- dotprod(v0,v1)
    theta <- acos((l0 - dp/l0) / ld)
    num <- sin(theta) * l0
    phispan <- acos(dp / l0 / l1)
    integrand <- function (phi, ...) {
        r <- num / sin(theta+phi)
        intrfr(r, ...)
    }
    do.call("integrate", c(list(integrand, 0, phispan, ...), control=control))
}
