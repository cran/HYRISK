\name{HYRISK-package}
\alias{HYRISK-package}
\alias{HYRISK}
\docType{package}
\title{
Hybrid Methods For Addressing Uncertainty In RISK Assessments
}
\description{
Methods for addressing uncertainty in risk assessments using hybrid representations of uncertainty (probability distributions, fuzzy numbers, intervals, probability distributions with imprecise parameters). The uncertainty propagation procedure combines random sampling using Monte Carlo method with fuzzy interval analysis of Baudrit et al. (2007) <doi:10.1109/TFUZZ.2006.876720>. The sensitivity analysis is based on the pinching method of Ferson and Tucker (2006) <doi:10.1016/j.ress.2005.11.052>.
}
\details{

This package provides tools for uncertainty analysis:
\itemize{
    \item Create input uncertain variables represented by an interval, a possibility distribution (trapezoidal or triangular), a probability distribution (normal, lognormal, beta, triangle, Gumbel or user-defined), or an imprecise probability distribution.
    \item Perform joint uncertainty propagation using IRS of Baudrit et al. (2007) or Hybrid algorithm described by Baudrit et al. (2006).
    \item Perform uncertainty propagation when the random variables are represented by imprecise probabilities, i.e. probability distribution with ill-known parameters (Pedroni et al., 2013).
    \item Summarize the uncertan results in the form of a pair of lower and upper cumulative distribution functions CDFs. 
    \item Summarize the uncertainty in the form of an interval of exceedance probabilities, an interval of quantiles, or a global indicator corresponding to the area between the lower and upper CDFs.
    \item Perform sensitivity analysis using a pinching approach (Ferson and Tucker, 2006).
}

}
\author{
Author: Jeremy Rohmer, Jean-Charles Manceau, Faiza Boulahya
Maintainer: Jeremy Rohmer <j.rohmer@brgm.fr>
}

\references{
	\itemize{
		\item Baudrit, C., Dubois, D., & Guyonnet, D. 2006. Joint propagation and exploitation of probabilistic and possibilistic information in risk assessment. IEEE transactions on fuzzy systems, 14(5), 593-608.
		\item Baudrit, C., Guyonnet, D., Dubois, D. 2007. Joint propagation of variability and partial ignorance in a groundwater risk assessment. Journal of Contaminant Hydrology, 93: 72-84.
		\item Ferson, S., & Tucker, W. T. (2006). Sensitivity analysis using probability bounding. Reliability Engineering & System Safety, 91(10), 1435-1442.
		\item Pedroni, N., Zio, E., Ferrario, E., Pasanisi, A., & Couplet, M. 2013. Hierarchical propagation of probabilistic and non-probabilistic uncertainty in the parameters of a risk model. Computers & Structures, 126, 199-213.
}
}

\keyword{ package }

