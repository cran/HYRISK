\name{PROPAG}
\alias{PROPAG}
\title{Function for conducting joint propagation of uncertainty}
\description{
Function for conducting joint propagation of probability, imprecise probability and possibilisty distributions (or intervals) using IRS (Baudrit et al., 2006) or hybrid method (Guyonnet et al., 2003).
}
\usage{
PROPAG(N, input, FUN, choice_opt="L-BFGS-B", param_opt = NULL, 
       mode = "IRS", corr = 0.01, NL = 10)
}
\arguments{
  \item{N}{
	Integer corresponding to the number of random samples.
}
  \item{input}{
	List of inputs as provided by the function \emph{CREATE_INPUT()}.
}
  \item{FUN}{
	Model assessment function.
}
  \item{choice_opt}{
	Option for the constrainted optimization algorithm: 
  	\itemize{
  		\item "L-BFGS-B": Limited-memory BFGS based on \emph{optimr} package.
  		\item "L-BFGS-B_MULTI": Limited-memory BFGS with multiple starts.
  		\item "GENOUD": genetic algorithm plus derivative optimizer based on \emph{rgenoud} package, see Mebane & Sekhon (2011). 
  	}
}
  \item{param_opt}{
	Parameters needed by the optimization algorithm:
	\itemize{
		\item If "L-BFGS-B": param_opt=NULL.
		\item If "L-BFGS-B_MULTI": \emph{param_opt} specifies the number of multi starts (e.g. 10).
		\item If "GENOUD": \emph{param_opt} specifies vector of paramters (population size, maxixmum of generations, solution tolerance), see Mebane & Sekhon (2011). 
	}
}
  \item{mode}{
	Type of hybrid uncertainty propagation:
	\itemize{
		\item "IRS" Independent Random Sampling of Baudrit et al. (2007).
		\item "HYBRID" Hybrid propagation described by Baudrit et al. (2006).
	}
}
  \item{corr}{
	Tolerance to avoid empty alpha-cuts. By default, \emph{corr}=0.01.
}
  \item{NL}{
	Integer to specify the number of alpha-cuts needed for hybrid propagation described by Baudrit et al. (2006). By default, \emph{NL}=10.
}
}

\details{
\itemize{
	\item Details of the theory and example 1 in Dubois & Guyonnet (2011), available at:
		\url{https://hal-brgm.archives-ouvertes.fr/file/index/docid/578821/filename/Uncertainties_RA_09_l_dg.pdf}
	\item Details on the representation via imprecise probability distributions in Pedroni et al. (2013).
	\item Details on example 2 in Sch\"obi & Sudret (2016), available at:
		\url{https://arxiv.org/pdf/1608.05565.pdf}
		
}
}

\value{
	Matrix Z0 of 2 rows and \emph{N} columns (for IRS mode) or \emph{N}x\emph{NL} (for HYBRID mode). 
	This corresponds to the set of random intervals (row Number 1: lower bound; row Number 2: upper bound), which can be summarized in different forms, see Baudrit et al. (2006).
}
\references{
	\itemize{
		\item Baudrit, C., Dubois, D., & Guyonnet, D. 2006. Joint propagation and exploitation of probabilistic and possibilistic information in risk assessment. IEEE transactions on fuzzy systems, 14(5), 593-608.
		\item Baudrit, C., Guyonnet, D., Dubois, D. 2007. Joint propagation of variability and partial ignorance in a groundwater risk assessment. Journal of Contaminant Hydrology, 93: 72-84.
		\item Mebane, W., Jr. and  Sekhon, J. S. 2011. Genetic Optimization Using Derivatives: The rgenoud package for R. Journal of Statistical Software, 42(11): 1-26. 
		\item Pedroni, N., Zio, E., Ferrario, E., Pasanisi, A., & Couplet, M. 2013. Hierarchical propagation of probabilistic and non-probabilistic uncertainty in the parameters of a risk model. Computers & Structures, 126, 199-213.
		\item Sch\"obi, R., Sudret, B. 2016. Uncertainty propagation of p-boxes using sparse polynomial chaos expansions, https://arxiv.org/pdf/1608.05565.pdf
}
}
\seealso{
\code{\link{CREATE_INPUT}}
\code{\link{CREATE_DISTR}}
\code{\link{PLOT_CDF}}
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
Z0_HYBRID<-PROPAG(N=250,input,FUN,choice_opt,param_opt,mode="HYBRID")

#################################################
#### POST-PROCESSING

# VISU - PROPAGATION
PLOT_CDF(Z0_IRS,xlab="Z",ylab="CDF",main="EX 1",lwd=1.5)
PLOT_CDF(Z0_HYBRID,new=FALSE,color1=3,color2=4,lwd=1.5)

#################################################
#### EXAMPLE 2 of Schobi & Sudret (2016)
#### Imprecise Probability distributions
#################################################

#### Model function
FUN<-function(X){
A=X[1]
B=X[2]
return(100*(B-A^2)^2+(1-A)^2)
}

ninput<-6 #Number of input parameters
input<-vector(mode="list", length=ninput) # Initialisation

# Imprecise normal probability 
# whose parameters are described by the 3rd and 5th parameters
input[[1]]=CREATE_INPUT(
		name="A",
		type="impr proba",
		distr="normal",
		param=c(3,5),
		monoton="dunno"
		)

# Imprecise normal probability
# whose parameters are described by the 4th and 6th parameters
input[[2]]=CREATE_INPUT(
		name="B",
		type="impr proba",
		distr="normal",
		param=c(4,6),
		monoton="dunno"
		)

# imprecise paramters of afore-described probability distribution
# mean of input number 1 as an interval
input[[3]]=CREATE_INPUT(
		name="mu_A",
		type="possi",
		distr="interval",
		param=c(-0.5,0.5)
		)

# mean of input number 2 as an interval
input[[4]]=CREATE_INPUT(
		name="mu_B",
		type="possi",
		distr="interval",
		param=c(-0.5,0.5)
		)

# standard deviation of input number 1  as an interval
input[[5]]=CREATE_INPUT(
		name="s_A",
		type="possi",
		distr="interval",
		param=c(0.7,1)
		)

# standard deviation of input number 2  as an interval
input[[6]]=CREATE_INPUT(
		name="s_B",
		type="possi",
		distr="interval",
		param=c(0.7,1)
		)


####CREATION OF THE DISTRIBUTIONS ASSOCIATED TO THE PARAMETERS
input=CREATE_DISTR(input)

####VISU INPUT (needs propagation parameters to plot impr proba distributions)
PLOT_INPUT(input)

#################################################
### PROPAGATION
# OPTIMZATION CHOICES (could take some time)
choice_opt="GENOUD"
param_opt=c(50,3,1.e-1)

#PROPAGATION RUN
Z0_IRS<-PROPAG(N=1000,input,FUN,choice_opt,param_opt,mode="IRS")

#################################################
### VISU - PROPAGATION
PLOT_CDF(Z0_IRS,xlab="Z",ylab="CDF",main="EX 2",lwd=1.5)

}
}
