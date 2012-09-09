\name{Chauvenet}
\alias{Chauvenet}
\title{
Chauvenets function to remove outliers
}
\description{
Remove outliers from a vector based on Chauvenets criterion. 
}
\usage{
Chauvenet(v, loop = FALSE)
}

\arguments{
  \item{v}{
	\code{v} vector of numeric values
	}
  \item{loop}{
	\code{loop} logical value; if TRUE, process will be repeated until no more values can be removed
	}
}
\details{
Calculates z value for each value in the vector (from mean and standard deviation). 
Then removes elements where the \code{erfc} of the z value is >0.5. (\dfn{erfc} = complementary error function). 
If \code{loop = TRUE}, this process will be repeated until no more values can be removed.
}
\value{
A numeric vector, with outliers removed; preserves order of original vector.	 
The vector will contain at least two values.
}
\references{
William Chauvenet, A Manual of Spherical and Practical Astronomy V.II , Lippincott, Philadelphia, 1st Ed (1863); Reprint of 1891 5th Ed: Dover, NY (1960).
}
\examples{
v1 <- c( 101.2, 90.0, 99.0, 102.0, 103.0, 100.2, 89.0, 98.1, 101.5, 102.0)
Chauvenet(v1) # 2 outliers removed
Chauvenet(v1, loop=TRUE) # 8 outliers removed (the maximum)
}