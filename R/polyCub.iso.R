################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2013 Sebastian Meyer
### Time-stamp: <[polyCub.iso.R] by SM Mit 05/06/2013 23:53 (CEST)>
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
#' The idea of this cubature rule is due to Emil Hedevang
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
    docheck <- isTRUE(check.intfx) || is.vector(check.intfx, mode="numeric")
    if (!missing(f)) {
        f <- match.fun(f)
        fx <- function (x, ...) x * f(cbind(x,0,deparse.level=0), ...)
        quadfx1 <- function (r, ...) integrate(fx, 0, r, ...)$value
        quadfx <- function (r, ...) sapply(r, quadfx1, ..., USE.NAMES=FALSE)
        if (missing(intfx)) intfx <- quadfx else if (docheck) {
            cat("Checking 'intfx' against a numeric approximation ... ")
            .rs <- if (isTRUE(check.intfx))
                seq(1, max(abs(unlist(lapply(polys, "[", c("x","y"))))),
                    length.out=20) else check.intfx
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
    
    ## Cubature of every single polygon of the "polys" list
    int1 <- function (poly) {
        xy <- cbind(poly[["x"]], poly[["y"]], deparse.level=0)
        nedges <- nrow(xy)
        intedges <- numeric(nedges)
        erredges <- numeric(nedges)
        for (i in seq_len(nedges)) {
            v0 <- xy[i,]
            v1 <- xy[if (i==nedges) 1 else i+1,]
            int <- intedge(v0, v1, intfx, ..., control=control)
            intedges[i] <- int$value
            erredges[i] <- int$abs.error
        }
        cubature_val <- sum(intedges)
        ## FIXME: return error
        ## FIXME: remove sign check -> tests
        if (!isTRUE(all.equal(0, cubature_val))) {
            if ((1 - 2 * as.numeric(poly$hole)) * sign(cubature_val) == -1)
                warning("wrong sign if positive integral")
        }
        cubature_val
    }
    respolys <- sapply(polys, int1, simplify = TRUE, USE.NAMES = FALSE)
    int <- sum(respolys)

### ILLUSTRATION ###
    if (plot && !missing(f)) {
        plot_polyregion(polyregion)
        ## TODO...
    }
###################

    int
}

intedge <- function (v0, v1, intfx, ..., control)
{
    d <- v1 - v0                        # direction
    ## parametrization of the edge (vectorized in t)
    e <- function (t) cbind(v0[1] + t*d[1], v0[2] + t*d[2])
    integrand <- function (t, ...) {
        p <- e(t)
        num <- d[2]*p[,1] - d[1]*p[,2] # FIXME: always same result for each t?
        norm2 <- rowSums(p^2)
        num/norm2 * intfx(sqrt(norm2), ...)
    }
    do.call("integrate", c(alist(integrand, 0, 1, ...), control))
}


if (FALSE) {
    library("devtools")
    unload("/home/sebastian/Projekte/polyCub/")
    f.powerlaw <- surveillance::siaf.powerlaw()$f
    load_all("/home/sebastian/Projekte/polyCub/")
    intfx.powerlaw <- function (r, logpars)
    {
        logsigma <- logpars[[1L]]
        logd <- logpars[[2L]]
        sigma <- exp(logsigma)
        d <- exp(logd)
        ## FIXME: for d not 1 or 2:
        r*(r+sigma)^(1-d)/(1-d) - ((r+sigma)^(2-d)-sigma^(2-d)) / ((1-d)*(2-d))
    }
    polyregion <- discpoly(c(3,2), 5, npoly=8, class="owin")
    logpars <- c(1.16, 0.84)
    polyCub.iso(polyregion, f.powerlaw, intfx=intfx.powerlaw, logpars=logpars,
                check.intfx=c(1, 5, 10, 50, 100, 500))
    polyCub.SV(polyregion, f.powerlaw, logpars=logpars, nGQ=200)
    ### YEEEHAAAW, polyCub.iso works and is much more efficient!!!
}
