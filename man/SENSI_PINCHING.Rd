\name{SENSI_PINCHING}
\alias{SENSI_PINCHING}
\title{
Sensitivity analysis using the pinching approach.
}
\description{
Function for conducting a sensitivity analysis using the pinching approach of Ferson and Tucker (2006). 
}
\usage{
SENSI_PINCHING(Z0, Z0p, mode = "global", 
	       threshold = NULL, level = NULL, disc=0.01)
}
\arguments{
  \item{Z0}{
	Output of the uncertainty propagation function \emph{PROPAG()}.
}
  \item{Z0p}{
	Output of the pinching function \emph{PINCHING_fun()}.
}
  \item{mode}{
	String to specify the mode to represent the epistemic uncertainty: 
	\itemize{
		\item If "proba": interval of exceedance probability given the threshold.
		\item If "quantile": interval of quantile given the level.
		\item If "global": global indicator of uncertainty corresponding to the area between the upper and lower CDFs.
	}
}
  \item{threshold}{
	Threshold value to compute the interval of exceedance probabilities.
}
  \item{level}{
	Level value to compute the interval of quantiles.
}

  \item{disc}{
	discretisation value to compute the global indicator
}

}
\value{
A number between 0 and 100% representing the decrease of uncertainty. By default, emph{disc}=100.
}

\references{
Ferson, S., & Tucker, W. T. (2006). Sensitivity analysis using probability bounding. Reliability Engineering & System Safety, 91(10), 1435-1442.
}

\seealso{
\code{\link{PINCHING_fun}}
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
#### PINCHING
Z0p<-PINCHING_fun(
	which=1,##first input variable
	value=5.7e-2, ##pinched at the scalar value of 5.7e-2
	N=1000,
	input,
	FUN,
	choice_opt,
	param_opt,
	mode="IRS"
	)

# VISU - PROPAGATION
PLOT_CDF(Z0_IRS,xlab="Z",ylab="CDF",main="EX PINCHING",lwd=1.5)
PLOT_CDF(Z0p,color1=3,color2=4,new=FALSE,lwd=1.5)

## quantile mode
sensi.quan<-SENSI_PINCHING(Z0_IRS,Z0p,mode="quantile",level=0.75)
print(paste("Quantile-based sensitivity measure: ",sensi.quan,sep=""))

## proba mode
sensi.proba<-SENSI_PINCHING(Z0_IRS,Z0p,mode="proba",threshold=2e-6)
print(paste("Probability-based sensitivity measure: ",sensi.proba,sep=""))

## global mode
sensi.global<-SENSI_PINCHING(Z0_IRS,Z0p,mode="global",disc=0.01)
print(paste("global sensitivity measure: ",sensi.global,sep=""))

}
}
