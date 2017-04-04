\name{PINCHING_fun}
\alias{PINCHING_fun}
\title{
Function to perform pinching.
}
\description{
Function to pinch a imprecise variable to a fixed value following Ferson and Tucker (2006) and perform the propagation.
Note that it only handles imprecise parametes. 
In this case of an imprecise probability distributions, only the imprecise parameters are handled.
}
\usage{
PINCHING_fun(which, value, N, input, FUN, 
	     choice_opt="L-BFGS-B", param_opt=NULL, 
	     corr = 0.01, NL = 10, mode = "IRS")
}
\arguments{
  \item{which}{
	Integer to specify the rank of the input variable as specifed in \emph{CREATE_INPUT()}, i.e. parameter 1, 2,..., etc.
}
  \item{value}{
	Scalar value to which the imprecise variable is pinched.
}
  \item{N}{
	Integer corresponding to the number of random samples.
}
  \item{input}{
	List of inputs as provided by \emph{CREATE_INPUT()}.
}
  \item{FUN}{
	Model assessment function.
}
  \item{choice_opt}{
	Option of constrainted optimization algorithm, see \code{\link{PROPAG}}.
}
  \item{param_opt}{
	Parameters necessary for conducting the optimization algorithm, see \code{\link{PROPAG}}.
}
  \item{corr}{
	Tolerance to avoid empty alpha-cuts. By default, \emph{corr}=0.01.
}
  \item{NL}{
	Integer to specify the number of alpha-cuts needed for hybrid propagation described by Baudrit et al. (2006). By default, \emph{NL}=10.
}
  \item{mode}{
	String to specify the mode of propagation: "IRS" (Baudrit et al. 2007) or "HYBRID" (Baudrit et al. 2006), see \code{\link{PROPAG}}.
}
}

\references{
	\itemize{
        	\item Baudrit, C., Dubois, D., & Guyonnet, D. 2006. Joint propagation and exploitation of probabilistic and possibilistic information in risk assessment. IEEE transactions on fuzzy systems, 14(5), 593-608.
		\item Baudrit, C., Guyonnet, D., Dubois, D. 2007. Joint propagation of variability and partial ignorance in a groundwater risk assessment. Journal of Contaminant Hydrology, 93: 72-84.
		\item Ferson, S., & Tucker, W. T. (2006). Sensitivity analysis using probability bounding. Reliability Engineering & System Safety, 91(10), 1435-1442.
        	}
}

\seealso{
\code{\link{SENSI_PINCHING}}
\code{\link{PROPAG}}
}
\examples{

#See example described for \emph{SENSI_PINCHING}.

}
