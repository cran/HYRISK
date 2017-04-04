\name{UNCERTAINTY}
\alias{UNCERTAINTY}
\title{
Global indicator for summarizing the epistemic uncertainty.
}
\description{
Function for summarizing the uncertainty propagation's results in the form of a global indicator corresponding the area between the upper and lower CDFs.
}
\usage{
UNCERTAINTY(Z0, disc=0.01)
}
\arguments{
  \item{Z0}{
Output of the uncertainty propagation function \emph{PROPAG()}.
}
  \item{disc}{
	Integer to specify number of the uniform discretisation of the pair of CDFs. By default, \emph{disc}=0.01
}
}
\value{
	Value of the area between both CDFs.
}

\details{
Details of the theory and the example in Dubois & Guyonnet (2011) 
Available at:
\url{https://hal-brgm.archives-ouvertes.fr/file/index/docid/578821/filename/Uncertainties_RA_09_l_dg.pdf}
}

\seealso{
\code{\link{PROPAG}}
\code{\link{PROBA_INTERVAL}}
\code{\link{QUAN_INTERVAL}}
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

# interval of quantiles
level=0.95
quant<-QUAN_INTERVAL(Z0_IRS,level)
Qlw<-quant$Qlow
Qup<-quant$Qupp
print(paste("interval of quantiles at level:",level," : ",
	"Qlow:",round(Qlw/10^floor(log10(Qlw)),
	abs(floor(log10((Qup-Qlw)/10^ceiling(log10(Qlw))))))*10^floor(log10(Qlw)),
	" & Qup:",round(Qup/10^floor(log10(Qup)),
	abs(floor(log10((Qup-Qlw)/10^ceiling(log10(Qup))))))*10^floor(log10(Qup)),sep="")
	)

# interval of probabilities
thres=1e-5
prob<-PROBA_INTERVAL(Z0_IRS,thres)
print(paste("interval of probabilities at threshold:",thres," : ",
	"Plow:",prob$Plow,
	" & Pup:",round(prob$Pupp,3),sep="")
	)

# Global indicator of uncertainty
unc<-UNCERTAINTY(Z0_IRS)
print(paste("Epistemic uncertainty : ",unc,sep=""))

}
}
