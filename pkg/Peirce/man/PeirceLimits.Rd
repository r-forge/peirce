\name{PeirceLimits}
\alias{PeirceLimits}
\title{
Plot limits of Peirce's function
}
\description{
Produces 3d plots using \pkg{rgl} (rotatable) and \pkg{scatterplot3d} which gives values of \emph{R} for various values of \emph{N} and \emph{m}.
\dfn{R} is the maximum allowable absolute error for a value in a vector of observations.
\dfn{N} is the number of observations (length of vector).
\dfn{m}, the \sQuote{number of unknown quantities}, may be treated as the number of independent processes giving rise to errors in the vector.
}
\usage{
PeirceLimits(N=1000L, plots=TRUE, noPoints=100)
}
\arguments{
  \item{N}{
    is the number of observations (length of vector)
  }
  \item{plots}{
    if \code{TRUE} then will display 3d plots
  }
  \item{noPoints}{
    no of intervals into which to divide \emph{k} and \emph{n}. Defaults
    to 100, corresponding to 100\% of \emph{N}. Larger values will give
    a more detailed resolution for the plotted image. Set to the same
    value as \emph{N} to generate results for \emph{every} combination
    of \emph{k} and \emph{m} up to \emph{N}
  }
  \item{asPercent}{
    if \code{TRUE} then will display returned values of \emph{k} and \emph{m}
  as percentages of \emph{N}
  }
}
\value{
  3d plots with \pkg{rgl} and \pkg{scatterplot3d}.
  A dataframe with value of \emph{R} for a given combination of \emph{k}
  and \emph{m}.
}
\note{
This can take some time to run for large values of \emph{N} e.g. >10000.
}
\seealso{
\code{\link{Peirce}}
}
\examples{
\dontrun{PeirceLimits()}
}