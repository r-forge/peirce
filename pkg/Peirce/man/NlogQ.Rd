\name{NlogQ}
\alias{NlogQ}
\title{
NlogQ
}
\description{
Calculate the value of NlogQ.
}
\usage{
NlogQ(N, k)
}
\arguments{
  \item{N}{
\code{N} Number of observations in dataset.
	}
  \item{k}{
Number of observations proposed to be rejected in dataset.
	}
}
\details{
Calculates \dfn{NlogQ}, as given in Table III of Goulds paper below. 
This value is used in the calcuation of \dfn{R}, the maximum allowable absolute error for a value in a vector of observations.
}
\value{
Numeric.
}
\references{
\href{http://articles.adsabs.harvard.edu/cgi-bin/nph-iarticle_query?1855AJ......4...81G&amp;data_type=PDF_HIGH&amp;whole_paper=YES&amp;type=PRINTER&amp;filetype=.pdf}{Gould BA (1855). On Peirce's Criterion for the Rejection of Doubtful Observations, with tables for facilitating its application. Astronomical Journal, 4(83), 81(87)}
}
\examples{
NlogQ(N=10, k=1)  # = 8.58818...
}
