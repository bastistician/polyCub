################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2013 Sebastian Meyer
### Time-stamp: <[polyCub.iso.R] by SM Mit 12/06/2013 03:16 (CEST)>
################################################################################


#' Cubature of Isotropic Functions over Polygonal Domains
#'
#' Conducts numerical integration along the polygon boundary where the integrand
#' is analytically available (it requires the antiderivative of \eqn{x f(x)}).
#' Thus, the two-dimensional integration problem
#' reduces to one dimension for isotropic functions.
#'
#' @inheritParams polyCub.SV
#' @param intfx analytical integral of \eqn{x f(x)} from 0 to \code{r}
#' (first argument, not necessarily named \code{"r"}, must be vectorized).
#' If given, \code{f} is only required for plotting.
#' If missing it is approximated numerically, however, in that case it might be
#' more appealing to use the product-Gauss cubature \code{\link{polyCub.SV}}.
#' @param ... further arguments for \code{f} or \code{intfx}.
#' @param control list of arguments for the quadrature rule along the edges,
#' i.e., \code{\link{integrate}}.
#' @param check.intfx logical (or numeric vector) indicating if
#' (for which \code{r}'s) the supplied \code{intfx} function should be
#' checked against a numeric approximation. If \code{TRUE}, the set of
#' \code{r}'s being evaluated is a \code{\link{seq}} of length 20 from 1 to
#' the maximum absolute x or y coordinate of any edge of the \code{polyregion}.
#' @return The approximate value of the integral of the isotropic function
#' \code{f} over \code{polyregion}.
#' @author Sebastian Meyer\cr
#' The idea for this cubature rule is due to Emil Hedevang
#' (Dept. of Mathematics, Aarhus University, Denmark)
#' @references
#' E. Hedevang (2013). Personal communication at the Summer School on Topics in
#' Space-Time Modeling and Inference (May 2013, Aalborg, Denmark).
#' @keywords math spatial
#' @family polyCub-methods
#' @examples # see example(polyCub)
#' @export

polyCub.iso <- function (polyregion, f, intfx, ..., control = list(),
                         check.intfx = FALSE, plot = FALSE)
{
    polys <- xylist(polyregion) # transform to something like "owin$bdry"
                                # which means anticlockwise vertex order with
                                # first vertex not repeated
    docheck <- !identical(FALSE, check.intfx)
    ##isTRUE(check.intfx) || is.vector(check.intfx, mode="numeric")
    if (!missing(f)) {
        f <- match.fun(f)
        fx <- function (x, ...) x * f(cbind(x,0,deparse.level=0), ...)
        quadfx1 <- function (r, ...) integrate(fx, 0, r, ...)$value
        quadfx <- function (r, ...) sapply(r, quadfx1, ..., USE.NAMES=FALSE)
        if (missing(intfx)) intfx <- quadfx else if (docheck) {
            cat("Checking 'intfx' against a numeric approximation ... ")
            .rs <- if (isTRUE(check.intfx)) {
                seq(1, max(abs(unlist(lapply(polys, "[", c("x","y"))))),
                    length.out=20)
            } else {
                stopifnot(is.vector(check.intfx, mode="numeric"))
                check.intfx
            }
            ana <- intfx(.rs, ...)
            num <- quadfx(.rs, ...)
            cat(comp <- all.equal(num, ana), "\n")
            if (!isTRUE(comp)) {
                cat("->", comp, "\n")
                warning("'intfx' might be incorrect: ", comp)
            }
        }
    } else if (docheck)
        stop("numerical verification of 'intfx' requires 'f'")
    intfx <- match.fun(intfx)

    ## do the cubature over all polygons of the 'polys' list
    int <- .polyCub.iso(polys, intfx, ..., control=control, .witherror=TRUE)

### ILLUSTRATION ###
    if (plot && !missing(f)) {
        plot_polyregion(polyregion)
        ## TODO...
    }
###################

    int
}


##' \code{.polyCub.iso} is a \dQuote{bare-bone} version of \code{polyCub.iso}.
##' @rdname polyCub.iso
##' @param polys something like \code{owin$bdry}.
##' @param .witherror logical indicating if an upperbound for the absolute
##' integration error should be attached as an attribute to the result?
##' @export
.polyCub.iso <- function (polys, intfx, ...,
                          control = list(), .witherror = FALSE)
{
    ints <- lapply(polys, polyCub1.iso,
                   intfx, ..., control=control, .witherror=.witherror)
    if (.witherror) {
        structure(sum(sapply(ints, "[", 1, simplify=TRUE, USE.NAMES=FALSE)),
                  abs.error=sum(sapply(ints, "[", 2,
                  simplify=TRUE, USE.NAMES=FALSE)))
    } else sum(unlist(ints, recursive=FALSE, use.names=FALSE))
}

## cubature method for a single polygon
polyCub1.iso <- function (poly, intfx, ..., control, .witherror = TRUE)
{
    xy <- cbind(poly[["x"]], poly[["y"]], deparse.level=0)
    nedges <- nrow(xy)
    intedges <- erredges <- numeric(nedges)
    for (i in seq_len(nedges)) {
        v0 <- xy[i,]
        v1 <- xy[if (i==nedges) 1 else i+1,]
        int <- lineInt(v0, v1, intfx, ..., control=control)
        intedges[i] <- int$value
        erredges[i] <- int$abs.error
    }
    int <- sum(intedges)
    ## if (!is.null(poly$hole) && !isTRUE(all.equal(0, int))) {
    ##     if ((1 - 2 * as.numeric(poly$hole)) * sign(int) == -1)
    ##         warning("wrong sign if positive integral")
    ## }
    if (.witherror) c(int, sum(erredges)) else int
}

## line integral for one edge
lineInt <- function (v0, v1, intfx, ..., control)
{
    d <- v1 - v0
    num <- v1[2]*v0[1] - v1[1]*v0[2]
    integrand <- function (t, ...) {
        ## get the points on the edge corresponding to t
        p <- cbind(v0[1] + t*d[1], v0[2] + t*d[2], deparse.level=0)
        ##num <- d[2]*p[,1] - d[1]*p[,2] # actually independent of t
        norm2 <- .rowSums(p^2, length(t), 2)
        ints <- intfx(sqrt(norm2), ...)
        ##ints[is.infinite(ints)] <- 1e300
        num * ints / norm2
    }
    if (length(control)) {              # use slower do.call()-construct
        do.call("integrate", c(list(integrand, 0, 1, ...), control=control))
    } else integrate(integrand, 0, 1, ...)
}

## equally fast method _only_ for convex polygonal domains including the origin
## (formula obtained via polar coordinate representation)
lineInt2 <- function (v0, v1, intfx, ..., control)
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
        intfx(r, ...)
    }
    do.call("integrate", c(list(integrand, 0, phispan, ...), control=control))
}



if (FALSE) {
    library("devtools")
    unload("/home/sebastian/Projekte/polyCub/")
    f.powerlaw <- surveillance::siaf.powerlaw()$f
    load_all("/home/sebastian/Projekte/polyCub/")
    intfx.powerlaw <- function (r, logpars, types=NULL)
    {
        logsigma <- logpars[[1L]]
        logd <- logpars[[2L]]
        sigma <- exp(logsigma)
        d <- exp(logd)
        ## FIXME: for d not 1 or 2:
        r*(r+sigma)^(1-d)/(1-d) - ((r+sigma)^(2-d)-sigma^(2-d)) / ((1-d)*(2-d))
    }
    polyregion <- discpoly(c(3,2), 5, npoly=512, class="owin")
    logpars <- c(1.16, 0.84)
    print(polyCub.iso(polyregion, f.powerlaw, intfx=intfx.powerlaw,
                      logpars=logpars, 
                      check.intfx=c(1, 5, 10, 50, 100, 500)), digits=10)
    system.time(print(polyCub.SV(polyregion, f.powerlaw, logpars=logpars,
                                 nGQ=200), digits=10))
    ## YEEEHAAAW, polyCub.iso works and is much more efficient!!!
    ## even with nGQ=200 product-Gauss cub. is not as exact as isotropic rule!!
    ## also look at runtime if intfx is not given (=> also approx. numerically):
    system.time(print(polyCub.iso(polyregion, f.powerlaw, logpars=logpars),
                      digits=10))
    system.time(print(polyCub.iso(polyregion, intfx=intfx.powerlaw,
                                  logpars=logpars), digits=10))
    ## takes 9x longer, but same result on first 10 digits!

    ## non-convex domain
    poly <- list(x = c(2, 0.9, 2.4, 1.7, 3, 4.4, 3.3, 3.3),
                 y = c(2.5, 2, 1.4, 1.1, 0.6, 2.4, 2.2, 3.5))
    poly <- lapply(poly, function(z) z-2)
    plot(poly, type="l")
    poly <- list(poly)
    polyCub.iso(poly, intfx=intfx.powerlaw, logpars=logpars)
    polyCub.SV(poly, f.powerlaw, logpars=logpars, nGQ=200)
    ## also correct
    polyCub.iso(list(lapply(poly[[1]], rev)),
                intfx=intfx.powerlaw, logpars=logpars)
    ## hole has negative result
}
