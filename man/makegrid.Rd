\name{makegrid}
\alias{makegrid}
\title{Constructs Equally-Spaced Grid}
\usage{
makegrid(range, n)
}
\arguments{
  \item{range}{numeric vector of length 2.}

  \item{n}{length of the desired grid, i.e. number of bins
  + 1.}
}
\value{
the desired grid, a numeric vector of length \code{n}
covering \code{range}.
}
\description{
Construct an equally-spaced grid given a range and the
number of cut points (one more than the number of resulting
bins). This is nothing else than \code{seq(range[1],
range[2], length.out=n)}.
}
\keyword{internal}

