################################################################################
### polyCub: Wrapper Function for the Various Cubature Methods
###
### Copyright (C) 2009-2013,2019 Sebastian Meyer
###
### This file is part of the R package "polyCub",
### free software under the terms of the GNU General Public License, version 2,
### a copy of which is available at https://www.R-project.org/Licenses/.
################################################################################


#' Wrapper Function for the Various Cubature Methods
#'
#' The wrapper function \code{polyCub} can be used to call specific cubature
#' methods via its \code{method} argument. It calls \code{\link{polyCub.SV}}
#' by default, which implements general-purpose product Gauss cubature.
#'
#' @inheritParams plotpolyf
#' @param f a two-dimensional real-valued function to be integrated over
#' \code{polyregion}. As its first argument it must take a coordinate matrix,
#' i.e., a numeric matrix with two columns, and it must return a numeric vector
#' of length the number of coordinates.\cr
#' For the \code{"exact.Gauss"} \code{method},
#' \code{f} is ignored since it is specific to the bivariate normal density.
#' @param method choose one of the implemented cubature methods (partial
#' argument matching is applied), see \code{help("\link{polyCub-package}")}
#' for an overview. Defaults to using product Gauss cubature
#' implemented in \code{\link{polyCub.SV}}.
#' @param ... arguments of \code{f} or of the specific \code{method}.
#' @param plot logical indicating if an illustrative plot of the numerical
#' integration should be produced.
#' @return The approximated integral of \code{f} over \code{polyregion}.
#' @seealso Details and examples in the \code{vignette("polyCub")}
#' and on the method-specific help pages.
#' @family polyCub-methods
#' @keywords math spatial
#' @export

polyCub <- function (polyregion, f,
                     method = c("SV", "midpoint", "iso", "exact.Gauss"), ...,
                     plot = FALSE)
{
    method <- match.arg(method)
    cl <- match.call()
    cl$method <- NULL
    cl[[1]] <- call("::", as.name("polyCub"),
                    as.name(paste("polyCub", method, sep=".")))
    if (method == "exact.Gauss") cl$f <- NULL
    int <- eval(cl, parent.frame())
    int
}
