\name{PeirceVsChauvenet}
\alias{PeirceVsChauvenet}
\title{
Produce plots and table (matrix) comparing Peirce to Chauvenet
}
\description{
Compares Peirces function with Chauvenets for four datasets.
Plots these datsets using base graphics and \pkg{ggplot2}.
Returns a matrix giving number of observations removed by each method.
}
\usage{
PeirceVsChauvenet(width = 1366, height = 768)
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
A matrix giving number of observations removed by each method.
}
\seealso{
\code{\link{Peirce}}
}
\examples{
\dontrun{PeirceVsChauvenet()}
}