\name{PLOT_INPUT}
\alias{PLOT_INPUT}
\title{
Function for plotting the inputs.
}
\description{
Plot the input variables. Regarding imprecise probability distribution, it plots the p-box of the imprecise probability distribution itself as well as its parameters.
Note that plotting the imprecise probability distribution requires solving an optimisation problem using the "L-BFGS-B" algorithm by assuming independdance of dependence among the alpha-cuts of imprecise parameters.
}
\usage{
PLOT_INPUT(input,N=1000,mode="IND")
}
\arguments{
  \item{input}{
	List of inputs with arguments as specified in \emph{CREATE_INPUT()}.
}
  \item{N}{
	Integer corresponding to the number of random samples (to be used for plotting imprecise probability distributions only). By default N=1000.
}
  \item{mode}{
	String to specify the dependence or the independence among the imprecise parameters (to be used for plotting imprecise probability distributions only): "IND" (independence) or "DEP" (dependence), see \code{\link{PROPAG}}.
}
}
\value{
	It provides a plot of the input variables. 
}

\seealso{
\code{\link{CREATE_INPUT}}
}

