################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2009-2013 Sebastian Meyer
### Time-stamp: <[polyCub.exact.Gauss.R] by SM Sam 11/05/2013 16:43 (CEST)>
################################################################################


#' Quasi-Exact Cubature of the Bivariate Normal Density
#'
#' Integration is based on triangulation and formulae from Chapter 26 of the
#' Abramowitz & Stegun handbook (Section 26.9, Example 9, pp. 956f.).
#' This method is quite cumbersome because the A&S formula is only for triangles
#' where one vertex is the origin (0,0). For each triangle of the
#' \code{\link[gpclib]{tristrip}} we have to check in which of the 6 outer 
#' regions of the triangle the origin (0,0) lies and adapt the signs in the 
#' formula appropriately. (AOB+BOC-AOC) or (AOB-AOC-BOC) or (AOB+AOC-BOC) or
#' (AOC+BOC-AOB) or \ldots. However, the most time consuming step is the
#' evaluation of \code{\link[mvtnorm]{pmvnorm}}.
#' 
#' @note The package \pkg{gpclib} (which is required to produce the
#' \code{tristrip}, since this is not yet implemented in \pkg{rgeos})
#' has a restricted license (commercial use prohibited).
#' It has to be accepted explicitly via
#' \code{\link{gpclibPermit}()} prior to using \code{polyCub.exact.Gauss}.
#'
#' @param polyregion anything coercible to a
#' \code{"\link[gpclib:gpc.poly-class]{gpc.poly}"} polygon.
#' @param mean,Sigma mean and covariance matrix of the bivariate normal density
#' to be integrated.
#' @param plot logical indicating if an illustrative plot of the numerical
#' integration should be produced. Note that the \code{polyregion} will be
#' shifted and scaled.
#' @return The integral of the bivariate normal density over \code{polyregion}.
#' Two attributes are appended to the integral value:
#' \item{nEval}{
#' number of triangles over which the standard bivariate normal density had to 
#' be integrated, i.e. number of calls to \code{\link[mvtnorm]{pmvnorm}} and
#' \code{\link[stats]{pnorm}}, the former of which being the most time-consuming
#' operation.
#' }
#' \item{error}{
#' Approximate absolute integration error steming from the error introduced by
#' the \code{nEval} \code{\link[mvtnorm]{pmvnorm}} evaluations.
#' For this reason, the cubature method is in fact only
#' quasi-exact (as is the \code{pmvnorm} function).
#' }
#' @references
#' M. Abramowitz and I. A. Stegun (1970).
#' Handbook of Mathematical Functions with Formulas, Graphs, and Mathematical
#' Tables (9th ed.). New York: Dover Publications.
#' @keywords math spatial
#' @family polyCub-methods
#' @examples # see example(polyCub)
#' @export

polyCub.exact.Gauss <- function (polyregion, mean = c(0,0), Sigma = diag(2),
                                 plot = FALSE)
{
    gpclibCheck()
    polyregion <- as(polyregion, "gpc.poly")
    
    ## coordinate transformation so that the standard bivariat normal density
    ## can be used in integrations (cf. formula 26.3.22)
    rho <- cov2cor(Sigma)[1,2]
    sdx <- sqrt(Sigma[1,1])
    sdy <- sqrt(Sigma[2,2])
    polyregion@pts <- lapply(polyregion@pts, function (poly) {
        list(x = ((poly$x-mean[1])/sdx + (poly$y-mean[2])/sdy) / sqrt(2+2*rho),
             y = ((poly$y-mean[2])/sdy - (poly$x-mean[1])/sdx) / sqrt(2-2*rho),
             hole = poly$hole)
    })
    
    ## triangulation: tristrip() returns a list where each element is a
    ## coordinate matrix of vertices of triangles 
    triangleSets <- gpclib::tristrip(polyregion)
    
### ILLUSTRATION ###
    if (plot) {
        plot(polyregion, poly.args=list(lwd=2), ann=FALSE)
        lapply(triangleSets, lines, lty=2)
    }
####################

    integrals <- sapply(triangleSets, function (triangles) {
        int <- 0
        error <- 0
        nTriangles <- nrow(triangles) - 2
        for (i in seq_len(nTriangles)) {
            res <- .intTriangleAS(triangles[i+(0:2),])
            err <- attr(res, "error")
            int <- int + res
            if (length(err) == 1L) error <- error + err
            ##<- sometimes err==numeric(0) (probably meaning err=0)
        }
        c(int, nTriangles, error)
    })
    int <- sum(integrals[1,])
    
    ## number of .V() evaluations
    ## (if 'h' in .intTriangleAS0 was always different from 0)
    attr(int, "nEval") <- 6 * sum(integrals[2,])
    ## approximate absolute integration error
    attr(int, "error") <- sum(integrals[3,])
    return(int)
}



###########################
### Auxiliary Functions ###
###########################

## calculates the integral of the standard bivariat normal over a triangle ABC
.intTriangleAS <- function (xy)
{
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
    d <- sqrt(sum((B-A)^2))
    h <- abs(B[2]*A[1] - A[2]*B[1]) / d
    if (h == 0) return(0)
    k1 <- abs(A[1]*(B[1]-A[1]) + A[2]*(B[2]-A[2])) / d
    k2 <- abs(B[1]*(B[1]-A[1]) + B[2]*(B[2]-A[2])) / d
    
    V2 <- .V(h, k2)
    V1 <- .V(h, k1)
    res <- if (isTRUE(all.equal(k1+k2, d))) V2 + V1
        else if (isTRUE(all.equal(abs(k2-k1), d))) abs(V2 - V1)
        else stop("something went wrong...")
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
.V <- function(h,k) {
    a <- k/h
    rho <- -a/sqrt(1+a^2)
    # V = 0.25 + L(h,0,rho) - L(0,0,rho) - Q(h) / 2
    # L(0,0,rho) = 0.25 + asin(rho) / (2*pi)
    # V = L(h,0,rho) - asin(rho)/(2*pi) - Q(h) / 2
    Lh0rho <- mvtnorm::pmvnorm(
        lower = c(h,0), upper = c(Inf,Inf),
        mean = c(0,0), corr = matrix(c(1,rho,rho,1),2,2)
    )
    Qh <- pnorm(h, mean = 0, sd = 1, lower.tail = FALSE)
    return(Lh0rho - asin(rho)/2/pi - Qh/2)
}
