################################################################################
### polyCub.exact.Gauss: Quasi-Exact Cubature of the Bivariate Normal Density
###
### Copyright (C) 2009-2018,2021-2023 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


#' Quasi-Exact Cubature of the Bivariate Normal Density (DEFUNCT)
#'
#' This cubature method is \strong{defunct} as of \pkg{polyCub} version 0.9.0.
#' It relied on \code{tristrip()} from package \CRANpkg{gpclib} for polygon
#' triangulation, but that package did not have a \acronym{FOSS} license and
#' was no longer maintained on a mainstream repository.\cr
#' Contributions to resurrect this cubature method are welcome: an alternative
#' implementation for constrained polygon triangulation is needed, see
#' \url{https://github.com/bastistician/polyCub/issues/2}.
#'
#' The bivariate Gaussian density can be integrated based on a triangulation of
#' the (transformed) polygonal domain, using formulae from the
#' Abramowitz and Stegun (1972) handbook (Section 26.9, Example 9, pp. 956f.).
#' This method is quite cumbersome because the A&S formula is only for triangles
#' where one vertex is the origin (0,0). For each triangle
#' we have to check in which of the 6 outer
#' regions of the triangle the origin (0,0) lies and adapt the signs in the
#' formula appropriately: \eqn{(AOB+BOC-AOC)} or \eqn{(AOB-AOC-BOC)} or
#' \eqn{(AOB+AOC-BOC)} or \eqn{(AOC+BOC-AOB)} or \ldots.
#' However, the most time consuming step is the
#' evaluation of \code{\link[mvtnorm]{pmvnorm}}.
#'
#' @param polyregion a \code{"gpc.poly"} polygon or
#' something that can be coerced to this class, e.g., an \code{"owin"} polygon
#' (via \code{\link{owin2gpc}}), or an \code{"sfg"} polygon (via
#' \code{\link{sfg2gpc}}).
#' @param mean,Sigma mean and covariance matrix of the bivariate normal density
#' to be integrated.
#' @param plot logical indicating if an illustrative plot of the numerical
#' integration should be produced. Note that the \code{polyregion} will be
#' transformed (shifted and scaled).
#' @return The integral of the bivariate normal density over \code{polyregion}.
#' Two attributes are appended to the integral value:
#' \item{nEval}{
#' number of triangles over which the standard bivariate normal density had to
#' be integrated, i.e. number of calls to \code{\link[mvtnorm]{pmvnorm}} and
#' \code{\link[stats]{pnorm}}, the former of which being the most time-consuming
#' operation.
#' }
#' \item{error}{
#' Approximate absolute integration error stemming from the error introduced by
#' the \code{nEval} \code{\link[mvtnorm]{pmvnorm}} evaluations.
#' For this reason, the cubature method is in fact only
#' quasi-exact (as is the \code{pmvnorm} function).
#' }
#' @references
#' Abramowitz, M. and Stegun, I. A. (1972).
#' Handbook of Mathematical Functions with Formulas, Graphs, and Mathematical
#' Tables. New York: Dover Publications.
#' @keywords math spatial
#' @seealso \code{\link{circleCub.Gauss}} for quasi-exact cubature of the
#' isotropic Gaussian density over a circular domain.
#' @family polyCub-methods
#' @example examples/setting.R
#' @examples
#' ## quasi-exact integration based on gpclib::tristrip() and mvtnorm::pmvnorm()
#' \dontrun{## (this example requires gpclib)
#' hexagon.gpc <- new("gpc.poly", pts = lapply(hexagon, c, list(hole = FALSE)))
#' plotpolyf(hexagon.gpc, f, xlim = c(-8,8), ylim = c(-8,8))
#' print(polyCub.exact.Gauss(hexagon.gpc, mean = c(0,0), Sigma = 5^2*diag(2),
#'                           plot = TRUE), digits = 16)
#' }
#' @import methods
#' @importFrom sp plot
#' @importFrom stats cov2cor
#' @importFrom graphics lines
#' @export

polyCub.exact.Gauss <- function (polyregion, mean = c(0,0), Sigma = diag(2),
                                 plot = FALSE)
{
    ## defunctify with a maintainer-level backdoor for building the vignette
    if (!identical(Sys.getenv("R_GPCLIBPERMIT"), "true"))
    .Defunct(msg = paste0(
        "'polyCub.exact.Gauss' is currently unavailable.\n",
        "Contributions are welcome: <https://github.com/bastistician/polyCub/issues/2>"
        ))

    if (inherits(polyregion, "owin")) {
        polyregion <- owin2gpc(polyregion)
    } else if (inherits(polyregion, "sfg")) {
        polyregion <- sfg2gpc(polyregion)
    } else if (!inherits(polyregion, "gpc.poly")) {
        polyregion <- as(polyregion, "gpc.poly")
    }

    stopifnot(is.numeric(mean), length(mean) == 2L, !is.na(mean),
              is.matrix(Sigma), identical(dim(Sigma), c(2L, 2L)),
              is.numeric(Sigma), diag(Sigma) > 0, !is.na(Sigma))

    ## coordinate transformation so that the standard bivariat normal density
    ## can be used in integrations (cf. formula 26.3.22)
    polyregion@pts <- transform_pts(polyregion@pts, mean = mean, Sigma = Sigma)

    ## triangulation: tristrip() returns a list where each element is a
    ## coordinate matrix of vertices of triangles
    ## FIXME: need a reliable tristrip() alternative
    triangleSets <- utils::getFromNamespace("tristrip", "gpclib")(polyregion)

### ILLUSTRATION ###
    if (plot) {
        plot(polyregion, poly.args=list(lwd=2), ann=FALSE)
        lapply(triangleSets, lines, lty=2)
    }
####################

    integrals <- vapply(X = triangleSets, FUN = function (triangles) {
        int <- 0
        error <- 0
        nTriangles <- nrow(triangles) - 2L
        for (i in seq_len(nTriangles)) {
            res <- .intTriangleAS(triangles[i+(0:2),])
            int <- int + res
            error <- error + attr(res, "error")
        }
        c(int, nTriangles, error)
    }, FUN.VALUE = numeric(3L), USE.NAMES = FALSE)
    int <- sum(integrals[1,])

    ## number of .V() evaluations (if there were no degenerate triangles)
    attr(int, "nEval") <- 6 * sum(integrals[2,])
    ## approximate absolute integration error
    attr(int, "error") <- sum(integrals[3,])
    return(int)
}



###########################
### Auxiliary Functions ###
###########################

## transform coordinates according to Formula 26.3.22
transform_pts <- function (pts, mean, Sigma)
{
    mx <- mean[1L]
    my <- mean[2L]
    stopifnot(abs(rho <- cov2cor(Sigma)[1L,2L]) < 1)
    sdx <- sqrt(Sigma[1L,1L])
    sdy <- sqrt(Sigma[2L,2L])
    lapply(pts, function (poly) {
        x0 <- (poly[["x"]] - mx) / sdx
        y0 <- (poly[["y"]] - my) / sdy
        list(x = (x0 + y0) / sqrt(2 + 2*rho),
             y = (y0 - x0) / sqrt(2 - 2*rho),
             hole = poly[["hole"]])
    })
}

## calculates the integral of the standard bivariat normal over a triangle ABC
.intTriangleAS <- function (xy)
{
    if (anyDuplicated(xy)) # degenerate triangle
        return(structure(0, error = 0))
    A <- xy[1,]
    B <- xy[2,]
    C <- xy[3,]
    intAOB <- .intTriangleAS0(A, B)
    intBOC <- .intTriangleAS0(B, C)
    intAOC <- .intTriangleAS0(A, C)

    # determine signs of integrals
    signAOB <- -1 + 2*.pointsOnSameSide(A,B,C)
    signBOC <- -1 + 2*.pointsOnSameSide(B,C,A)
    signAOC <- -1 + 2*.pointsOnSameSide(A,C,B)

    int <- signAOB*intAOB + signBOC*intBOC + signAOC*intAOC
    attr(int, "error") <- attr(intAOB, "error") +
        attr(intBOC, "error") + attr(intAOC, "error")
    return(int)
}

## calculates the integral of the standard bivariat normal over a triangle A0B
.intTriangleAS0 <- function (A, B)
{
    BmA <- B - A
    d <- vecnorm(BmA)
    h <- abs(B[2L]*A[1L] - A[2L]*B[1L]) / d   # distance of AB to the origin
    if (d == 0 || h == 0) # degenerate triangle: A == B or 0, A, B on a line
        return(structure(0, error = 0))

    k1 <- dotprod(A, BmA) / d
    k2 <- dotprod(B, BmA) / d
    V2 <- .V(h, abs(k2))
    V1 <- .V(h, abs(k1))
    res <- if (sign(k1) == sign(k2)) {
        ## A and B are on the same side of the normal line through 0
        abs(V2 - V1)
    } else {
        V2 + V1
    }
    attr(res, "error") <- attr(V1, "error") + attr(V2, "error")
    return(res)
}

## checks if point1 and point2 lie on the same side of a line through
## linepoint1 and linepoint2
.pointsOnSameSide <- function (linepoint1, linepoint2, point1, point2 = c(0,0))
{
    n <- c(-1,1) * rev(linepoint2-linepoint1)   # normal vector
    S <- dotprod(point1-linepoint1,n) * dotprod(point2-linepoint1,n)
    return(S > 0)
}

## calculates the integral of the standard bivariat normal
## over a triangle bounded by y=0, y=ax, x=h (cf. formula 26.3.23)
#' @importFrom stats pnorm
.V <- function(h,k) {
    if (k == 0) # degenerate triangle
        return(structure(0, error = 0))
    a <- k/h
    rho <- -a/sqrt(1+a^2)
    # V = 0.25 + L(h,0,rho) - L(0,0,rho) - Q(h) / 2
    # L(0,0,rho) = 0.25 + asin(rho) / (2*pi)
    # V = L(h,0,rho) - asin(rho)/(2*pi) - Q(h) / 2
    Lh0rho <- mvtnorm::pmvnorm(
        lower = c(h,0), upper = c(Inf,Inf),
        mean = c(0,0), corr = matrix(c(1,rho,rho,1), 2L, 2L)
    )
    Qh <- pnorm(h, mean = 0, sd = 1, lower.tail = FALSE)
    return(Lh0rho - asin(rho)/2/pi - Qh/2)
}
