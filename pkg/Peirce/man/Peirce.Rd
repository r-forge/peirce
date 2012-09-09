\name{Peirce}
\alias{Peirce}
\title{
Peirces function to remove outliers
}
\description{
Remove outliers from a vector based on Peirces criterion.
}
\usage{
Peirce(v, m = 1)
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
\dfn{m}, the \sQuote{number of unknown quantities}, may be treated as the number of independent processes giving rise to errors. 
For practical purposes m should generally be restricted to 1.
}
\value{
A numeric vector, with outliers removed; ordered by value of absolute error. 
}
\seealso{
See Also \code{\link{PeirceGould}, \link{PeirceVsChauvenet}}
}
\examples{
\dontrun{
v1 <- c(101.2, 90.0, 99.0, 102.0, 103.0, 100.2, 89.0, 98.1, 101.5, 102.0)
Peirce(v1) # 2 outliers removed
	}
}