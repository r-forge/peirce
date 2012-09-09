\name{Ross}
\alias{Ross}
\docType{data}
\title{
Outliers dataset from Ross paper
}
\description{
Outliers dataset from Ross paper. 
10 observations from a pressure gage, taken in an experiment at one setting.
}
\usage{data(Ross)}
\format{
  The format is:
 num [1:10] 101 90 99 102 103 ...
}
\source{
\href{http://classes.engineering.wustl.edu/2009/fall/che473/handouts/OutlierRejection.pdf}{Ross S (2003). Peirce's criterion for the elimination of suspect experimental data. Journal of Engineering Technology, 20(2), 1-12}
}

\examples{
data(Ross)
hist(Ross)
}
\keyword{datasets}
