\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
%% some pre-defined commands: \R, \code, \acronym, \url, \file, \pkg

\name{NEWS}
\title{News for Package 'polyCub'}


\section{Changes in polyCub version 0.5-1 (2014-10-24)}{

  \itemize{
    \item Nodes and weights for \code{polyCub.SV()} were only cached
    up to \code{nGQ=59}, not 60 as announced in version 0.5-0. Fixed
    that which also makes examples truly run without \pkg{statmod}.

    \item In \code{polyCub.SV()}, the new special setting
    \code{f=NULL} means to only compute nodes and weights.

    \item Internal changes to the \code{"gpc.poly"} converters
    to accommodate \CRANpkg{spatstat} 1.39-0.
  }
  
}



\section{Changes in polyCub version 0.5-0 (2014-05-07)}{

  \itemize{
    \item \code{polyCub.SV()} gained an argument \code{engine} to choose
    among available implementations. The new and faster C-implementation
    is the default. There should not be any numerical differences in the
    result of the cubature.

    \item Package \CRANpkg{statmod} is no longer strictly required
    (imported). Nodes and weights for Gauss-Legendre quadrature in
    \code{polyCub.SV()} are now cached in the \pkg{polyCub} package
    up to \code{nGQ=60}. \pkg{statmod}\code{::gauss.quad} is only
    queried for a higher number of nodes.
  }
  
}



\section{Changes in polyCub version 0.4-3 (2014-03-14)}{

  \itemize{
    \item \code{polyCub.iso()} ...
    \itemize{
      \item could not handle additional arguments for
      \code{integrate()} given in the \code{control} list.
    
      \item applies the \code{control} arguments also
      to the numerical approximation of \code{intrfr}.
    }
    
    \item The \code{checkintrfr()} function is exported and documented.

    \item Added a \file{CITATION} file.
  }
  
}



\section{Changes in polyCub version 0.4-2 (2014-02-12)}{

  \itemize{
    \item \code{plotpolyf()} ...
    
    \itemize{
      \item gained an additional argument
      \code{print.args}, an optional list of arguments passed to
      \code{print.trellis()} if \code{use.lattice=TRUE}.

      \item passed a \emph{data frame} of coordinates
      to \code{f} instead of a matrix as documented.
    }
  }
  
}



\section{Changes in polyCub version 0.4-1 (2013-12-05)}{

  \itemize{
    \item This version solely fixes a missing \file{NAMESPACE} import to
    make package \pkg{polyCub} again compatible with older versions of
    \CRANpkg{spatstat} (< 1.33-0).
  }
  
}



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
