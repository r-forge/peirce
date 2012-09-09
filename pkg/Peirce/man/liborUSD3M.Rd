\name{liborUSD3M}
\alias{liborUSD3M}
\docType{data}
\title{
LIBOR rate in US Dollars for 3 month loans
}
\description{
LIBOR rate submissions for 16 banks during a 3 month period in 2008.
Each submission reflects the rate of interest the bank would expect to pay on a 3 month loan in US Dollars.
}
\usage{data(liborUSD3M)}
\format{
  A data frame with 496 observations on the following 3 variables.
  \describe{
    \item{\code{Bank}}{a character vector}
    \item{\code{Date}}{a character vector}
    \item{\code{X3M}}{a character vector}
  }
}

\source{
\href{https://docs.google.com/spreadsheet/ccc?key=0AonYZs4MzlZbdEtRNnA4SWx1djhTSHpyYVliQ1pFb2c\#gid=0}{Data on google docs spreasheet}
}
\references{
\href{http://www.guardian.co.uk/news/datablog/2012/jul/03/libor-rates-set-banks}{Guardian newspaper 2012}
}
\examples{
data(liborUSD3M)
str(liborUSD3M) 
\dontrun{ 
liborUSD3M <- transform(liborUSD3M, Date = as.Date(Date, format="\%m/\%d/\%Y")) # convert to date format
liborUSD3M <- transform(liborUSD3M, X3M = as.numeric(X3M)) # convert to numeric
liborUSD3M <- transform(liborUSD3M, Bank = as.factor(Bank)) # convert to factor
	}
}
\keyword{datasets}
