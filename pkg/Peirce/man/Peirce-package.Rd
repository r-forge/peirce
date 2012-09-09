\name{Peirce-package}
\alias{Peirce-package}
\docType{package}
\title{
Peirce
}
\description{
\code{Peirce} and \code{Chauvenet} functions for removal of outliers. 
Examples and illustrations from the original papers.
}
\details{
\tabular{ll}{
Package: \tab Peirce \cr
Type: \tab Package \cr
Version: \tab 0.5 \cr
Date: \tab 2012-08-26 \cr
License: \tab GPL (>= 2) \cr
LazyLoad: \tab yes \cr
}
Functions for removing outliers: \code{Peirce}, \code{Chauvenet}.
Other functions are provided as examples and illustrations.
}
\author{
Christopher Dardis
Maintainer: chrisopherdardis@gmail.com
}
\references{
\href{http://classes.engineering.wustl.edu/2009/fall/che473/handouts/OutlierRejection.pdf}{Ross S (2003). Peirce's criterion for the elimination of suspect experimental data. Journal of Engineering Technology, 20(2), 1-12.}
}
\keyword{ package }
\examples{
v1 <- c(101.2, 90.0, 99.0, 102.0, 103.0, 100.2, 89.0, 98.1, 101.5, 102.0)
Peirce(v1) # 2 outliers removed
Chauvenet(v1) # 2 outliers removed
}