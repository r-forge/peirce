\name{PeirceGould}
\alias{PeirceGould}
\title{
Peirces function to remove outliers
}
\description{
Remove outliers from a vector based on Peirces criterion.
Method follows the original technique in the paper by Gould below.
}
\usage{
PeirceGould(v, m = 1)
}

\arguments{
  \item{v}{
	\code{v} vector of numeric values
	}
  \item{m}{
	\code{m} number of unknown quantities
	}
}
\details{
Calculates \dfn{R}, the maximum allowable absolute error for a value in the vector, and removes those values exceeding this value. 
\dfn{m}, the \sQuote{number of unknown quantities}, may be treated as degrees of freedom of the system of observations, or the number of independent processes giving rise to errors. 
For practical purposes m should generally be restricted to 1.
For efficiency, \code{Peirce} is preferred.
}
\value{
A numeric vector, with outliers removed; ordered by value of absolute error. 
}
\references{
\href{http://articles.adsabs.harvard.edu/cgi-bin/nph-iarticle_query?1855AJ......4...81G&amp;data_type=PDF_HIGH&amp;whole_paper=YES&amp;type=PRINTER&amp;filetype=.pdf}{Gould BA (1855). On Peirce's Criterion for the Rejection of Doubtful Observations, with tables for facilitating its application. Astronomical Journal, 4(83), 81(87)}
}
\seealso{
\code{\link{Peirce}}
}
\examples{
v1 <- c(101.2, 90.0, 99.0, 102.0, 103.0, 100.2, 89.0, 98.1, 101.5, 102.0)
Peirce(v1) # 2x outliers removed
}