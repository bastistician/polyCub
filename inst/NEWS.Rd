\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
%% some pre-defined commands: \R, \code, \acronym, \url, \file, \pkg

\name{NEWS}
\title{News for Package 'polyCub'}


\section{Changes in polyCub version 0.3-1 (2013-08-22)}{

  \itemize{
    \item This version solely fixes a few typos and a technical note
    from \command{R CMD check} in the current R development version
    (also import packages into the \file{NAMESPACE} which are listed
    in the \dQuote{Depends:} field).
  }

}



\section{Changes in polyCub version 0.3-0 (2013-07-06)}{

  \itemize{
    \item New cubature method \code{polyCub.iso()} specific to isotropic
    functions (thanks to Emil Hedevang for the basic idea).
    \item New function \code{plotpolyf()} to plot a polygonal domain on
    top of an image of a bivariate function.
    \item The package now depends on \R >= 2.15.0 (for \code{.rowSums()}).
    \item The package no longer registers \code{"owin"} as an \acronym{S4}-class
    since we depend on the \pkg{sp} package which does the job. This
    avoids a spurious warning (in \code{.simpleDuplicateClass()}) upon
    package installation.
    \item In \code{discpoly()}, the argument \code{r} has been renamed
    to \code{radius}. This is backward compatible by partial argument
    matching in old code.
  }

}



\section{Changes in polyCub version 0.2-0 (2013-05-09)}{

  \itemize{
    \item This is the initial version of the \pkg{polyCub} package
    mainly built on functions previously maintained within the
    \CRANpkg{surveillance} package. These methods for cubature of
    polygonal domains have been outsourced into this separate
    \pkg{polyCub} package since they are of general use for other
    packages as well.
    
    \item The \pkg{polyCub} package has more documentation and tests,
    avoids the use of \CRANpkg{gpclib} as far as possible (using
    \CRANpkg{rgeos} instead), and solves a compatibility issue with
    package \CRANpkg{maptools} (use \code{setClass("owin")} instead
    of \code{setOldClass("owin")}).
  }

}
