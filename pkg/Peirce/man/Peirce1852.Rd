\name{Peirce1852}
\alias{Peirce1852}
\title{
Plots from Peirces 1852 paper
}
\description{
Produce plots of functions from Peirces 1852 paper.
Initial two plots are with \code{Hmisc::labcurve}. 
Subsequent plots are with \code{ggplot2}. 
Produces plots corresponding to his functions for \ifelse{html}{\out{&phi;}}{\eqn{\phi}{phi}} and \ifelse{html}{\out{&psi;}}{\eqn{\psi}{psi}} and compares the latter with \dfn{erfc}, the complementary error function.
}
\usage{
Peirce1852(width = 1366, height = 768)
}
\arguments{
  \item{width}{
	\code{width} width of graphical display (screen) in pixels
	}
  \item{height}{
	\code{height} height of graphical display (screen) in pixels
	}
}
\value{
Plots two curves with \code{Hmisc::labcurve} and returns a list of 4 plots from \code{ggplot2}.
}
\references{
\href{http://articles.adsabs.harvard.edu/cgi-bin/nph-iarticle_query?1852AJ......2..161P;data_type=PDF_HIGHhttp://adsabs.harvard.edu/full/1852AJ......2..161P.}{Peirce B (1852). Criterion for the rejection of doubtful observations. The Astronomical Journal, 2(45), 161-163.}
}
\note{
The tail end of function contains attempts to generate probability functions from Peirces paper.
These values are not returned.
}
\seealso{
\code{\link{Peirce}}
}
\examples{
\dontrun{Peirce1852()}
}
