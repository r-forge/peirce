\name{libor}
\alias{libor}
\alias{Libor}
\alias{LIBOR}
\alias{BBAlibor}
\alias{BBALibor}
\title{
bbalibor function to remove outliers then return mean of those remaining.
}
\description{
Remove outliers from a vector based on bbalibor criterion, then calculate mean of those remaining.
Number removed is based on length of vector. 
\itemize{
\item If 15-18, 4 highest and lowest removed.
\item If 11-14, 3 highest and lowest removed.
\item If 8-10, 2 highest and lowest removed.
\item If 6-7, 1 highest and lowest removed.
	}
}
\usage{
libor(v)
}
\arguments{
  \item{v}{
	\code{v} vector of numeric values, length 6-18
}
}
\value{
Numeric. The mean of the vector with outliers removed.
}
\references{
\href{http://www.bbalibor.com/bbalibor-explained/the-basics}{bbalibor explained}
}
\seealso{
\code{\link{PeirceVsLibor}}
}
\examples{
libor(runif(n=16, min=0, max=10))
}