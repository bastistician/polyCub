################################################################################
### Package Setup
###
### Copyright (C) 2009-2014,2018-2021 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


#' Cubature over Polygonal Domains
#'
#' The \R package \pkg{polyCub} implements \emph{cubature}
#' (numerical integration) over \emph{polygonal} domains.
#' It solves the problem of integrating a continuously differentiable
#' function \eqn{f(x,y)} over simple closed polygons.
#'
#' \pkg{polyCub} provides the following cubature methods,
#' which can either be called explicitly or via the generic
#' \code{\link{polyCub}} function:
#' \describe{
#' \item{\code{\link{polyCub.SV}}:}{
#' General-purpose \emph{product Gauss cubature} (Sommariva and Vianello, 2007)
#' }
#' \item{\code{\link{polyCub.midpoint}}:}{
#' Simple \emph{two-dimensional midpoint rule} based on
#' \code{\link[spatstat.geom]{as.im.function}} from \pkg{spatstat.geom}
#' (Baddeley et al., 2015)
#' }
#' \item{\code{\link{polyCub.iso}}:}{
#' Adaptive cubature for \emph{radially symmetric functions}
#' via line \code{\link{integrate}()} along the polygon boundary
#' (Meyer and Held, 2014, Supplement B, Section 2.4).
#' }
#' \item{\code{\link{polyCub.exact.Gauss}}:}{
#' Accurate (but slow) integration of the \emph{bivariate Gaussian density}
#' based on polygon triangulation (via \code{\link[gpclib]{tristrip}} from
#' \pkg{gpclib}) and (numerous) evaluations of cumulative densities (via
#' \code{\link[mvtnorm]{pmvnorm}} from package \pkg{mvtnorm}).
#' Note that there is also a function \code{\link{circleCub.Gauss}}
#' to integrate the \emph{isotropic} Gaussian density over a
#' \emph{circular domain}.
#' }
#' }
#' A more detailed description and benchmark experiment of the above cubature
#' methods can be found in the \code{vignette("polyCub")} and in
#' Meyer (2010, Section 3.2).
#'
#' @references
#' Abramowitz, M. and Stegun, I. A. (1972).
#' \emph{Handbook of Mathematical Functions with Formulas, Graphs, and
#' Mathematical Tables}. New York: Dover Publications.
#'
#' Baddeley, A., Rubak, E. and Turner, R. (2015).
#' \emph{Spatial Point Patterns: Methodology and Applications with R}.
#' Chapman and Hall/CRC Press, London.
#'
#' Meyer, S. (2010).
#' \emph{Spatio-Temporal Infectious Disease Epidemiology based on Point
#' Processes}. Master's Thesis, LMU Munich.
#' Available as \url{https://epub.ub.uni-muenchen.de/11703/}.
#'
#' Meyer, S. and Held, L. (2014).
#' Power-law models for infectious disease spread.
#' \emph{The Annals of Applied Statistics}, \bold{8} (3), 1612-1639.
#' \doi{10.1214/14-AOAS743}
#'
#' Sommariva, A. and Vianello, M. (2007).
#' Product Gauss cubature over polygons based on Green's integration formula.
#' \emph{BIT Numerical Mathematics}, \bold{47} (2), 441-453.
#' \doi{10.1007/s10543-007-0131-2}
#' @docType package
#' @name polyCub-package
#' @seealso
#' \code{vignette("polyCub")}
#'
#' For the special case of a rectangular domain along the axes
#' (e.g., a bounding box), the \CRANpkg{cubature} package is more appropriate.
NULL

#' \pkg{gpclib} License Acceptance (OBSOLETE)
#'
#' Previous versions of package \CRANpkg{gpclib} had a restricted license
#' (commercial use prohibited) and these functions were used as a blocker.
#' They now always return \code{TRUE}.
#'
#' \pkg{gpclib} functionality is only required for
#' \code{\link{polyCub.exact.Gauss}}.
#' @keywords internal
#' @export
gpclibPermit <- function ()
{
    ##requireNamespace("gpclib")
    gpclibPermitStatus()
}
#' @rdname gpclibPermit
#' @export
gpclibPermitStatus <- function () TRUE
