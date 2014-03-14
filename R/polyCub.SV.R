################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2009-2014 Sebastian Meyer
### Time-stamp: <[polyCub.SV.R] by SM Fre 14/03/2014 14:49 (CET)>
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
#' Sommariva, A. and Vianello, M. (2007).
#' Product Gauss cubature over polygons based on Green's integration formula.
#' \emph{Bit Numerical Mathematics}, \bold{47} (2), 441-453.
#' @keywords math spatial
#' @family polyCub-methods
#' @importFrom statmod gauss.quad
#' @importFrom graphics points
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
    ## in one list
    nw_MN <- unname(c(nw_M, nw_N))
    
    ## Cubature of every single polygon of the "polys" list
    int1 <- function (poly) {
        nw <- polygauss(poly, nw_MN, alpha, rotation)
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
            nw <- polygauss(polys[[i]], nw_MN, alpha, rotation)
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
##' @param nw_MN unnamed list of nodes and weights of one-dimensional Gauss
##' quadrature rules of degrees \eqn{N} and \eqn{M=N+1} (as returned by
##' \code{\link[statmod]{gauss.quad}}): \code{list(s_M, w_M, s_N, w_N)}.
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
##' Sommariva, A. and Vianello, M. (2007):
##' Product Gauss cubature over polygons based on Green's integration formula.
##' \emph{Bit Numerical Mathematics}, \bold{47} (2), 441-453.
##' @keywords internal

polygauss <- function (xy, nw_MN, alpha = NULL, rotation = FALSE)
{
    ## convert to coordinate matrix
    xy <- cbind(xy[["x"]], xy[["y"]], deparse.level=0)

    
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

    sides <- cbind(xyrot,
                   xyrot[c(2:nrow(xyrot),1L),,drop=FALSE],
                   deparse.level=0) # (x1,y1,x2,y2)
    
    nwlist <- mapply(.polygauss.side,
                     sides[,1L], sides[,2L], sides[,3L], sides[,4L],
                     MoreArgs = c(nw_MN, alpha),
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)

    ## nodes <- c(subListExtract(nwlist, "x", use.names=FALSE),
    ##            subListExtract(nwlist, "y", use.names=FALSE),
    ##            recursive=TRUE)
    nodes <- c(lapply(nwlist, "[[", 1L),
               lapply(nwlist, "[[", 2L),
               recursive=TRUE)
    dim(nodes) <- c(length(nodes)/2, 2L)
    
    #nw <- .Call("polygauss",
    #            xyrot[,1L],xyrot[,2L],alpha,
    #            nw_N$nodes,nw_N$weights,nw_M$nodes,nw_M$weights,
    #            PACKAGE="surveillance")

    ## back-transform rotated nodes by t(t(rotmat) %*% t(nodes))
    ## (inverse of rotation matrix is its transpose)
    list(nodes = if (rotation) nodes %*% rotmat else nodes,
         #weights = unlist(subListExtract(nwlist, "w", use.names=FALSE), recursive=FALSE, use.names=FALSE),
         weights = unlist(lapply(nwlist, "[[", 3L), recursive=FALSE, use.names=FALSE),
         angle = angle, alpha = alpha)
}


## The working horse .polygauss.side below is an R translation
## of the original MATLAB implementation by Sommariva and Vianello (2007).
## TODO: efficient implementation of this function in C
##       might increase the speed of the cubature (although this is already
##       highly efficient R code)

.polygauss.side <- function (x1, y1, x2, y2, s_loc, w_loc, s_N, w_N, alpha)
{
    if ((x1 == alpha && x2 == alpha) || (y2 == y1))
        ## side lies on base-line or is orthogonal to it -> skip
        return(NULL)
    
    if (x2 == x1) { # side is parallel to base-line => degree N
        s_loc <- s_N
        w_loc <- w_N
    }
    
    half_pt_x <- (x1+x2)/2
    half_length_x <- (x2-x1)/2
    
    half_pt_y <- (y1+y2)/2
    half_length_y <- (y2-y1)/2
    
    ## GAUSSIAN POINTS ON THE SIDE.
    x_gauss_side <- half_pt_x + half_length_x * s_loc
    y_gauss_side <- half_pt_y + half_length_y * s_loc

    scaling_fact_minus <- (x_gauss_side - alpha) / 2

    ## construct nodes and weights: x and y coordinates ARE STORED IN MATRICES.
    ## A COUPLE WITH THE SAME INDEX IS A POINT, i.e. P_i=(x(k),y(k)).
    ## Return in an unnamed list of nodes_x, nodes_y, weights
    ## (there is no need for c(nodes_x) and c(weights))
    list(alpha + tcrossprod(scaling_fact_minus, s_N + 1), # degree_loc x N
         rep.int(y_gauss_side, length(s_N)),              # length: degree_loc*N
         tcrossprod(half_length_y*scaling_fact_minus*w_loc, w_N)) # degree_loc x N
}

##' @importFrom stats dist
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
