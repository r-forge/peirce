\name{PeirceVsLibor}
\alias{PeirceVsLibor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Produce plots and table (data frame) comparing Peirce to Chauvenet
}
\description{
Compares Peirces function with bbalibor for an example dataset.
Peirces and libor function are used to remove outliers, then the mean is calculated from the remaining values.
Plots the number of observations removed by each method for each date with base graphics.
Plots the datset, including averaged values by each method, using \pkg{ggplot2}.
Returns a data frame giving number of observations removed by Peirces method for each date.
}
\usage{
PeirceVsLibor(width = 1366, height = 768)
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
A data frame giving number of observations removed by Peirces method for each date.
}
\author{
Christopher Dardis
}
\seealso{
\code{\link{liborUSD3M}}
}
\examples{
\dontrun{PeirceVsLibor()}
}