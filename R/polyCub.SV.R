################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2009-2013 Sebastian Meyer
### Time-stamp: <[polyCub.SV.R] by SM Sam 06/07/2013 17:29 (CEST)>
################################################################################


#' Product Gauss Cubature over Polygonal Domains
#'
#' Product Gauss cubature over polygons as proposed by
#' Sommariva and Vianello (2007).
#' 
#' @param polyregion a polygonal integration domain.
#' The following classes are supported: \code{"\link[spatstat]{owin}"},
#' \code{"\link[rgeos:gpc.poly-class]{gpc.poly}"},
#' \code{"\linkS4class{SpatialPolygons}"}, \code{"\linkS4class{Polygons}"},
#' and \code{"\linkS4class{Polygon}"}
#' (for these we have an internal \code{\link{xylist}} method).
#' @param f two-dimensional function.
#' As its first argument it must take a coordinate matrix, i.e. a
#' numeric matrix with two columns.
#' @param ... further arguments for \code{f}.
#' @param nGQ degree of the one-dimensional Gauss-Legendre quadrature rule
#' (default: 20). See \code{\link[statmod]{gauss.quad}} in package
#' \pkg{statmod}, on which this function depends.
#' @param plot logical indicating if an illustrative plot of the numerical
#' integration should be produced.
#' @inheritParams polygauss
#' @return The approximated value of the integral of \code{f} over
#' \code{polyregion}.
#' @author Sebastian Meyer\cr
#' The product Gauss cubature is based on the
#' original \acronym{MATLAB} implementation \code{polygauss} by Sommariva and
#' Vianello (2007), which is available under the GNU GPL (>=2) license from
#' \url{http://www.math.unipd.it/~alvise/software.html}.
#' @references
#' A. Sommariva and M. Vianello (2007).
#' Product Gauss cubature over polygons based on Green's integration formula.
#' Bit Numerical Mathematics, 47 (2), 441-453.
#' @keywords math spatial
#' @family polyCub-methods
#' @importFrom statmod gauss.quad
#' @importMethodsFrom rgeos plot
#' @examples # see example(polyCub)
#' @export

polyCub.SV <- function (polyregion, f, ...,
                        nGQ = 20, alpha = NULL, rotation = FALSE,
                        plot = FALSE)
{
    polys <- xylist(polyregion) # transform to something like "owin$bdry"
                                # which means anticlockwise vertex order with
                                # first vertex not repeated
    f <- match.fun(f)
    stopifnot(isScalar(nGQ),
              is.null(alpha) || (isScalar(alpha) && !is.na(alpha)))

    ## COMPUTE NODES AND WEIGHTS OF 1D GAUSS QUADRATURE RULE.
    ## DEGREE "N" (as requested) (ORDER GAUSS PRIMITIVE)
    nw_N <- gauss.quad(n = nGQ, kind = "legendre")
    ## DEGREE "M" = N+1 (ORDER GAUSS INTEGRATION)
    nw_M <- gauss.quad(n = nGQ + 1, kind = "legendre")

    ## Cubature of every single polygon of the "polys" list
    int1 <- function (poly) {
        nw <- polygauss(poly, nw_N, nw_M, alpha, rotation)
        fvals <- f(nw$nodes, ...)
        cubature_val <- sum(nw$weights * fvals)
        ## if (!isTRUE(all.equal(0, cubature_val))) {
        ## if ((1 - 2 * as.numeric(poly$hole)) * sign(cubature_val) == -1)
        ## warning("wrong sign if positive integral")
        ## }
        cubature_val
    }
    respolys <- sapply(polys, int1, simplify = TRUE, USE.NAMES = FALSE)
    int <- sum(respolys)

### ILLUSTRATION ###
    if (plot) {
        plotpolyf(polys, f, ..., use.lattice=FALSE)
        for (i in seq_along(polys)) {
            nw <- polygauss(polys[[i]], nw_N, nw_M, alpha, rotation)
            points(nw$nodes, cex=0.6, pch = i) #, col=1+(nw$weights<=0)
        }
    }
###################

    int
}


##' Calculate 2D Nodes and Weights of the Product Gauss Cubature
##'
##' @param xy list with elements \code{"x"} and \code{"y"} containing the
##' polygon vertices in \emph{anticlockwise} order (otherwise the result of the
##' cubature will have a negative sign) with first vertex not repeated at the
##' end (like \code{owin.object$bdry}).
##' @param nw_N,nw_M lists of nodes and weights of one-dimensional Gauss
##' quadrature rules of degrees \eqn{N} and \eqn{M=N+1} (as returned by
##' \code{\link[statmod]{gauss.quad}})
##' @param alpha base-line of the (rotated) polygon at \eqn{x = \alpha} (see
##' Sommariva and Vianello (2007) for an explication). If \code{NULL} (default),
##' the midpoint of the x-range of the polygon is chosen if no \code{rotation}
##' is performed, and otherwise the \eqn{x}-coordinate of the rotated point
##' \code{"P"}. If \code{f} has its maximum value at the origin \eqn{(0,0)},
##' e.g., the bivariate Gaussian density with zero mean, \code{alpha = 0} is a
##' reasonable choice.
##' @param rotation logical (default: \code{FALSE}) or a list of points
##' \code{"P"} and \code{"Q"} describing the preferred direction. If
##' \code{TRUE}, the polygon is rotated according to the vertices \code{"P"} and
##' \code{"Q"}, which are farthest apart (see Sommariva and Vianello, 2007). For
##' convex polygons, this rotation guarantees that all nodes fall inside the
##' polygon.
##' @references
##' A. Sommariva and M. Vianello (2007):
##' Product Gauss cubature over polygons based on Green's integration formula.
##' Bit Numerical Mathematics, 47 (2), 441-453.
##' @keywords internal

polygauss <- function (xy, nw_N, nw_M, alpha = NULL, rotation = FALSE)
{
    ## convert to coordinate matrix
    xy <- cbind(x=xy[["x"]], y=xy[["y"]], deparse.level=0)

    
    ## POLYGON ROTATION
    
    xyrot <- if (identical(FALSE, rotation)) {
        if (is.null(alpha)) { # choose midpoint of x-range
            xrange <- range(xy[,1L])
            alpha <- (xrange[1L] + xrange[2L]) / 2
        }
        angle <- 0
        xy
    } else {
        if (identical(TRUE, rotation)) { # automatic choice of rotation angle
            ## such that for a convex polygon all nodes fall inside the polygon
            QP <- vertexpairmaxdist(xy)
            Q <- QP[1L,,drop=TRUE]
            P <- QP[2L,,drop=TRUE]
        } else if (is.list(rotation)) {  # predefined rotation
            P <- rotation$P
            Q <- rotation$Q
            stopifnot(is.vector(P, mode="numeric") && length(P) == 2L,
                      is.vector(Q, mode="numeric") && length(Q) == 2L)
            stopifnot(any(P != Q))
            rotation <- TRUE
        } else {
            stop("'rotation' must be logical or a list of points \"P\" and \"Q\"")
        }
        rotmat <- rotmatPQ(P,Q)
        angle <- attr(rotmat, "angle")
        if (is.null(alpha)) {
            Prot <- rotmat %*% P
            alpha <- Prot[1]
        }
        xy %*% t(rotmat)   # = t(rotmat %*% t(xy))
    }

    
    ## COMPUTE 2D NODES AND WEIGHTS.

    xbd <- xyrot[,1L,drop=TRUE]
    ybd <- xyrot[,2L,drop=TRUE]
    nw <- .polygauss(c(xbd,xbd[1L]), c(ybd,ybd[1L]), alpha,
                     nw_N$nodes, nw_N$weights, nw_M$nodes, nw_M$weights)
    #nw <- .Call("polygauss",
    #            xyrot[,1L],xyrot[,2L],alpha,
    #            nw_N$nodes,nw_N$weights,nw_M$nodes,nw_M$weights,
    #            PACKAGE="surveillance")

    ## back-transform rotated nodes by t(t(rotmat) %*% t(nodes))
    ## (inverse of rotation matrix is its transpose)
    nodes <- if (rotation) nw$nodes %*% rotmat else nw$nodes
    
    ## Done.
    list(nodes=nodes, weights=nw$weights, angle = angle, alpha = alpha)
}


## The working horses .polygauss and .polygauss.side below are R translations
## of the original MATLAB implementation by Sommariva and Vianello (2007).
## TODO: efficient implementation of these functions in C
##       would increase the speed of the cubature
## Parameters:
## x, y: coordinates of the polygon's vertices in _anticlockwise_ order
##       (otherwise the result of the cubature will have a negative sign)
##       with _REPEATED FIRST VERTEX_ at the end
## alpha: "base-line"
## s/w_N/M: nodes and weights of univariate Gauss rules of orders N and M=N+1

.polygauss <- function (x, y, alpha, s_N, w_N, s_M, w_M)
{
    L <- length(x) - 1L                 # number of sides of the polygon
    N <- length(s_N)
    M <- length(s_M)
    maxNodes <- L*M*N
    nodes <- matrix(0, nrow=maxNodes, ncol=2L)
    weights <- numeric(maxNodes)
    K <- 0L

    for (side in seq_len(L))
    {
        x1 <- x[side];  x2 <- x[side+1L]
        y1 <- y[side];  y2 <- y[side+1L]

        if ((x1 == alpha && x2 == alpha) || (y2 == y1)) {
            ## skip: side lies on base-line or is orthogonal to it
            next
        }

        if (x2 == x1) { # side is parallel to base-line => degree N
            degree_loc <- N
            s_loc <- s_N
            w_loc <- w_N
        } else { # degree M=N+1
            degree_loc <- M
            s_loc <- s_M
            w_loc <- w_M
        }

        nw_loc <- .polygauss.side(x1, y1, x2, y2, s_loc, w_loc, s_N, w_N, alpha)

        ## add nodes and weights for this side of the polygon
        nNodes_loc <- degree_loc * N
        rowidx <- K + seq_len(nNodes_loc)
        nodes[rowidx,] <- nw_loc$nodes
        weights[rowidx] <- nw_loc$weights
        K <- K + nNodes_loc
    }
        
    # only the first K entries of 'weights' and rows of 'nodes_x' and 'nodes_y'
    # have been filled, the remainder until 'maxRows', contains the zeros
    # from initialisation.
    seqK <- seq_len(K)
    weights <- weights[seqK]
    nodes <- nodes[seqK,,drop=FALSE]

    ## Done
    ret <- list(nodes = nodes, weights = weights)
    ret
}

.polygauss.side <- function (x1, y1, x2, y2, s_loc, w_loc, s_N, w_N, alpha)
{
    degree_loc <- length(s_loc)
    N <- length(s_N)
    
    half_pt_x <- (x1+x2)/2;  half_length_x <- (x2-x1)/2;
    half_pt_y <- (y1+y2)/2;  half_length_y <- (y2-y1)/2;
    
    ## GAUSSIAN POINTS ON THE SIDE.
    x_gauss_side <- half_pt_x + half_length_x * s_loc
    y_gauss_side <- half_pt_y + half_length_y * s_loc

    ## construct weights
    scaling_fact_minus <- (x_gauss_side - alpha) / 2
    weights1 <- (half_length_y * scaling_fact_minus) * w_loc # length=degree_loc
    weights <- tcrossprod(weights1, w_N) # degree_loc x N
    #weights <- rep(w_N, each=degree_loc) * rep.int(weights1, N)  # lenth=degree_loc*N
    
    ## construct nodes: x and y coordinates ARE STORED IN MATRICES.
    ## A COUPLE WITH THE SAME INDEX IS A POINT, i.e. P_i=(x(k),y(k)).
    term_2 <- matrix(scaling_fact_minus, nrow = degree_loc, ncol = N)
    rep_n_Np1 <- matrix(s_N + 1, nrow = degree_loc, ncol = N, byrow = TRUE)
    nodes_x <- rep_n_Np1 * term_2 + alpha # degree_loc x N
    #nodes_y <- matrix(y_gauss_side, nrow = degree_loc, ncol = N)
    nodes_y <- rep.int(y_gauss_side, N)   # no matrix dimensions
    
    ## Done (return as 2-column matrix, and weights vector)
    list(nodes = cbind(x=c(nodes_x), y=nodes_y), weights = c(weights))
}

vertexpairmaxdist <- function (xy)
{
    ## compute euclidean distance matrix
    distances <- dist(xy)
    size <- attr(distances, "Size")
    
    ## select two points with maximum distance
    maxdistidx <- which.max(distances)
    lowertri <- seq_along(distances) == maxdistidx
    mat <- matrix(FALSE, size, size)
    mat[lower.tri(mat)] <- lowertri
    QPidx <- which(mat, arr.ind=TRUE, useNames=FALSE)[1L,]
    xy[QPidx,]    
}

rotmatPQ <- function (P, Q)
{
    direction_axis <- (Q-P) / sqrt(sum((Q-P)^2))
    
    ## determine rotation angle
    rot_angle_x <- acos(direction_axis[1L])
    rot_angle_y <- acos(direction_axis[2L])
    
    rot_angle <- if (rot_angle_y <= pi/2) {
        if (rot_angle_x <= pi/2) -rot_angle_y else rot_angle_y
    } else {
        if (rot_angle_x <= pi/2) pi-rot_angle_y else rot_angle_y
    }
    ## cat(sprintf(' [ANGLE CLOCKWISE (IN DEGREES)]: %5.5f\n', rot_angle*180/pi))

    ## rotation matrix
    rot_matrix <- diag(cos(rot_angle), nrow=2L)
    rot_matrix[2:3] <- c(-1,1) * sin(rot_angle) # clockwise rotation
    structure(rot_matrix, angle=rot_angle)
}
