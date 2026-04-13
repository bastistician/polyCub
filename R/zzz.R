################################################################################
### Package Setup
###
### Copyright (C) 2009-2014,2018-2021,2023,2026 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


#' Cubature over Polygonal Domains
#'
#' The \R package \pkg{polyCub} implements
#' \emph{cubature} (numerical integration) over \emph{polygonal} domains.
#' It solves the problem of integrating a continuously differentiable
#' function \eqn{f(x,y)} over simple closed polygons.
#'
#' \pkg{polyCub} provides the following cubature methods:
#' \describe{
#' \item{\code{\link{polyCub.SV}}:}{
#' General-purpose \emph{product Gauss cubature}
#' \bibcitep{sommariva.vianello2007}
#' }
#' \item{\code{\link{polyCub.midpoint}}:}{
#' Simple \emph{two-dimensional midpoint rule} based on
#' \code{\link[spatstat.geom]{as.im.function}} from \CRANpkg{spatstat.geom}
#' \bibcitep{R:spatstat.geom}
#' }
#' \item{\code{\link{polyCub.iso}}:}{
#' Adaptive cubature for \emph{radially symmetric functions}
#' via line \code{\link{integrate}()} along the polygon boundary
#' \bibcitep{|meyer.held2014|Supplement B, Section 2.4}
#' }
#' }
#' A brief description and benchmark experiment of the above cubature
#' methods can be found in the \code{vignette("polyCub")}.
#'
#' There is also \code{\link{polyCub.exact.Gauss}}, intended to
#' accurately (but slowly) integrate the \emph{bivariate Gaussian density};
#' however, this implementation is disabled as of \pkg{polyCub} 0.9.0:
#' it needs a reliable implementation of polygon triangulation.
#'
#' \bibcitet{|meyer2010|Section 3.2}
#' discusses and compares some of these methods.
#'
#' @note To cite package \pkg{polyCub} in publications,
#' please use \code{citation("polyCub")}:
#'
#' \Sexpr[results=rd,stage=build]{tools::toRd(citation("polyCub"))}
#'
#' @author Sebastian Meyer
#' @references \bibshow{*}
#' @seealso
#' \code{vignette("polyCub")}
#'
#' For the special case of a rectangular domain along the axes
#' (e.g., a bounding box), the \CRANpkg{cubature} package is more appropriate.
"_PACKAGE"
