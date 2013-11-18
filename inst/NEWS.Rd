\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
%% some pre-defined commands: \R, \code, \acronym, \url, \file, \pkg

\name{NEWS}
\title{News for Package 'polyCub'}


\section{Changes in polyCub version 0.4-0 (2013-11-19)}{

  \subsection{INFRASTRUCTURE}{
    \itemize{
      \item \CRANpkg{rgeos} (and therefore the GEOS library) is no longer
      strictly required (moved from Imports to Suggests).
      
      \item Added \code{coerce}-methods from \code{"Polygons"} (or
      \code{"SpatialPolygons"} or \code{"Polygon"}) to \code{"owin"}
      (\code{as(..., "owin")}).
      
      \item \acronym{S4}-style \code{coerce}-methods between
      \code{"gpc.poly"} and \code{"Polygons"}/\code{"owin"} have been
      removed from the package (since we no longer import the formal class
      \code{"gpc.poly"} from \pkg{gpclib} or \pkg{rgeos}).
      However, there are two new functions \code{gpc2owin} and
      \code{owin2gpc} similar to those dropped from \CRANpkg{spatstat}
      since version 1.34-0.
      
      \item Moved \code{discpoly()} back to \CRANpkg{surveillance} since
      it is only used there.

      \item The latter two changes cause \CRANpkg{surveillance} version
      1.6-0 to be incompatible with this new version of \pkg{polyCub}.
      Appropriate modifications have been made in the new version
      1.7-0 of \pkg{surveillance}.
    }
  }

  \subsection{SPEED-UP \code{polyCub.SV()}}{
    \itemize{
      \item thorough optimization of \code{polyCub.SV()}-related code
      resulted in about 27\% speed-up:
      \itemize{
	\item use \code{mapply()} instead of a \code{for}-loop
	\item avoid \code{cbind()}
	\item use \code{tcrossprod()}
	\item less object copying
      }
    }
  }

  \subsection{MINOR CHANGES}{
    \itemize{
      \item \code{xylist()} is now exported. It simply extracts
      polygon coordinates from various spatial classes (with same unifying
      intention as \code{xy.coords()}).
      
      \item A \code{polyregion} of class \code{"SpatialPolygons"}
      of length more than 1 now works in \code{polyCub}-methods.
      
      \item Use aspect ratio of 1 in \code{plotpolyf()}.
    }
  }

}



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
