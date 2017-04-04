\name{SUMMARY_1CDF}
\alias{SUMMARY_1CDF}
\title{
Function for summarizing the uncertainty propagation's results in the form of a unique CDF.
}
\description{
Function for summarizing the uncertainty propagation's results in the form of a unique CDF via the weighting average approach of Dubois and Guyonnet (2011).
}
\usage{
SUMMARY_1CDF(Z0, aversion = 0.5)
}
\arguments{
  \item{Z0}{
	Output of the uncertainty propagation function \emph{PROPAG()}
}
  \item{aversion}{
	Weight value representing the decision-maker risk aversion i.e. the balance between the lower and upper CDFs. by default, \emph{alpha}=0.5.
}
}

\value{
	Vector of the same size as the number of columns of \emph{Z0}. 
}

\details{
Details of the theory and the example in Dubois & Guyonnet (2011) 
Available at:
\url{https://hal-brgm.archives-ouvertes.fr/file/index/docid/578821/filename/Uncertainties_RA_09_l_dg.pdf}
}

\references{
Dubois D., Guyonnet D. 2011. Risk-informed decision-making under epistemic uncertainty. International Journal of General Systems, 40(2), 145-167.
}
\seealso{
\code{\link{PROPAG}}
}
\examples{
\dontrun{
#################################################
#### EXAMPLE 1 of Dubois & Guyonnet (2011)
#### Probability and Possibility distributions
#################################################

#### Model function
FUN<-function(X){
UER=X[1]
EF=X[2]
I=X[3]
C=X[4]
ED=X[5]
return(UER*I*C*EF*ED/(70*70*365))
}

ninput<-5 #Number of input parameters
input<-vector(mode="list", length=ninput) # Initialisation

input[[1]]=CREATE_INPUT(
		name="UER",
		type="possi",
		distr="triangle",
		param=c(2.e-2, 5.7e-2, 1.e-1),
		monoton="incr"
		)
input[[2]]=CREATE_INPUT(
		name="EF",
		type="possi",
		distr="triangle",
		param=c(200,250,350),
		monoton="incr"
		)
input[[3]]=CREATE_INPUT(
		name="I",
		type="possi",
		distr="triangle",
		param=c(1,1.5,2.5),
		monoton="incr"
		)
input[[4]]=CREATE_INPUT(
		name="C",
		type="proba",
		distr="triangle",
		param=c(5e-3,20e-3,10e-3)
		)
input[[5]]=CREATE_INPUT(
		name="ED",
		type="proba",
		distr="triangle",
		param=c(10,50,30)
		)

####CREATION OF THE DISTRIBUTIONS ASSOCIATED TO THE PARAMETERS
input=CREATE_DISTR(input)

####VISU INPUT
PLOT_INPUT(input)

#################################################
#### PROPAGATION

#OPTIMZATION CHOICES
choice_opt=NULL #no optimization needed
param_opt=NULL

#PROPAGATION RUN
Z0_IRS<-PROPAG(N=1000,input,FUN,choice_opt,param_opt,mode="IRS")

#################################################
#### POST-PROCESSING

# VISU - PROPAGATION
PLOT_CDF(Z0_IRS,xlab="Z",ylab="CDF",main="EX 1",lwd=1.5)

# One CDF with risk aversion of 1/3
Z<-SUMMARY_1CDF(Z0_IRS,aversion=1/3)
lines(ecdf(Z),col=5,lwd=1.5)

}
}
