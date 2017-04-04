\name{PROBA_INTERVAL}
\alias{PROBA_INTERVAL}
\title{
Interval of probability of exceedance.
}
\description{
Function for summarizing the uncertainty propagation's results in the form of an interval of probability of exceedance for a given threshold.
}
\usage{
PROBA_INTERVAL(Z0, threshold)
}
\arguments{
  \item{Z0}{
	Output of the uncertainty propagation function \emph{PROPAG()}.
}
  \item{threshold}{
	Scalar value specifying the threshold for which the probability of exceedance should be calculated.
}
}
\value{
	List with arguments \emph{Plow} and \emph{Pupp} corresponding to lower and upper probability values.
}

\details{
Details of the theory and the example in Dubois & Guyonnet (2011) 
Available at:
\url{https://hal-brgm.archives-ouvertes.fr/file/index/docid/578821/filename/Uncertainties_RA_09_l_dg.pdf}
}

\seealso{
\code{\link{PROPAG}}
\code{\link{QUAN_INTERVAL}}
\code{\link{UNCERTAINTY}}
}
\examples{

#See example described for \emph{UNCERTAINTY}.
}

