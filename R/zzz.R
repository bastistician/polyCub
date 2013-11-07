################################################################################
### Part of the R package "polyCub".
### Free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at http://www.r-project.org/Licenses/.
###
### Copyright (C) 2009-2013 Sebastian Meyer
### Time-stamp: <[zzz.R] by SM Don 07/11/2013 20:54 (CET)>
###
### Package administration
################################################################################


#' Cubature over Polygonal Domains
#'
#' The \R package \pkg{polyCub} provides methods for \strong{cubature}
#' (numerical integration) \strong{over polygonal domains}.
#' The function \code{\link{polyCub}()} is the main entry point of the package. 
#' It is a wrapper around the specific cubature methods listed below.
#'
#' \describe{
#' \item{\code{\link{polyCub.midpoint}}:}{
#' Two-dimensional midpoint rule.
#' Polygons are converted to binary pixel images
#' using the \code{\link[spatstat]{as.im.function}} method from package
#' \pkg{spatstat}. The integral is then obtained as the sum over (pixel area *
#' f(pixel midpoint)).
#' }
#' \item{\code{\link{polyCub.SV}}:}{
#' Product Gauss cubature as proposed by Sommariva and Vianello (2007).
#' }
#' \item{\code{\link{polyCub.iso}}:}{
#' Efficient adaptive cubature for \emph{isotropic} functions via line
#' \code{\link{integrate}()} along the polygon boundary.
#' }
#' \item{\code{\link{polyCub.exact.Gauss}}:}{
#' Quasi-exact method specific to the integration of the \emph{bivariate Gaussian
#' density} over polygonal domains. It is based on formulae from Chapter 26 of
#' the Abramowitz and Stegun (1970) handbook, i.e. triangulation of the
#' polygonal domain (using \code{\link[gpclib]{tristrip}} of package
#' \pkg{gpclib}) and appropriate evaluations of
#' \code{\link[mvtnorm]{pmvnorm}} from package \pkg{mvtnorm}.
#' Note that there is also a function \code{\link{circleCub.Gauss}}
#' to perform integration of the \emph{isotropic} Gaussian density over
#' \emph{circular domains}.
#' }
#' }
#' See Section 3.2 of Meyer (2010) for a more detailed description and benchmark
#' experiment of some of the above cubature methods (and others).
#'
#' @references
#' M. Abramowitz and I. A. Stegun (1970).
#' Handbook of Mathematical Functions with Formulas, Graphs, and Mathematical
#' Tables (9th ed.). New York: Dover Publications.
#'
#' A. Baddeley and R. Turner (2005).
#' Spatstat: an R package for analyzing spatial point patterns.
#' Journal of Statistical Software 12 (6), 1-42.
#'
#' S. Meyer (2010).
#' Spatio-Temporal Infectious Disease Epidemiology based on Point Processes.
#' Master's Thesis, LMU Munich.
#' Available as \url{http://epub.ub.uni-muenchen.de/11703/}.
#' 
#' A. Sommariva and M. Vianello (2007).
#' Product Gauss cubature over polygons based on Green's integration formula.
#' Bit Numerical Mathematics, 47 (2), 441-453.
#' @docType package
#' @name polyCub-package
#' @seealso The packages \pkg{cubature} and \pkg{R2Cuba}, which are more
#' appropriate for cubature over simple hypercubes.
NULL


.Options <- new.env()

.onLoad <- function (libname, pkgname)
{
    .Options$gpclib <- FALSE
}

gpclibCheck <- function (fatal = TRUE)
{
    gpclibOK <- .Options$gpclib
    if (!gpclibOK && fatal) {
        message("Note: The gpclib license is accepted by ",
                sQuote("gpclibPermit()"), ".")
        stop("acceptance of the gpclib license is required")
    }
    gpclibOK
}

##' \pkg{gpclib} Licence Acceptance
##'
##' Similar to the handling in package \pkg{maptools}, these functions
##' explicitly accept the restricted \pkg{gpclib} licence (commercial use
##' prohibited) and report its acceptance status, respectively.
##' \pkg{gpclib} functionality is only required for
##' \code{\link{polyCub.exact.Gauss}}.
##' @export
gpclibPermit <- function ()
{
    if (requireNamespace("gpclib")) .Options$gpclib <- TRUE
    gpclibPermitStatus()
}
##' @rdname gpclibPermit
##' @export
gpclibPermitStatus <- function () gpclibCheck(fatal=FALSE)
