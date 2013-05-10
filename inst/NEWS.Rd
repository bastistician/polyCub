\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
%% some pre-defined commands: \R, \code, \acronym, \url, \file, \pkg

\name{NEWS}
\title{News for Package 'polyCub'}


\section{Changes in polyCub version 0.2-0 (2013-05-09)}{

\subsection{SYNOPSIS}{
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

}
