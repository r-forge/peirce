\name{NIST}
\alias{NIST}
\docType{data}
\title{
NIST outliers dataset
}
\description{
Outliers dataset from NIST. 90 ordered observations.
}
\usage{data(NIST)}
\format{
  The format is:
 num [1:90] 30 171 184 201 212 250 265 270 272 289 ...
}
\source{
\href{http://www.itl.nist.gov/div898/handbook/prc/section1/prc16.htm}{NIST outliers example}
}
\examples{
data(NIST)
str(NIST)
hist(NIST)
}
\keyword{datasets}
