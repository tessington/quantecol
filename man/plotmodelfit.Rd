% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotmodelfit.R
\name{plotmodelfit}
\alias{plotmodelfit}
\title{Plot data and fitted model}
\usage{
plotmodelfit(
  x,
  r,
  N0 = NULL,
  N1 = NULL,
  K = NULL,
  type = "Process",
  add.to.plot = F
)
}
\arguments{
\item{x:}{the data frame containing year and harbor seal abundances}

\item{r:}{the fitted parameter r}

\item{N0:}{the fitted parameter N0 (only used for process error models)}

\item{N1:}{the fitted parameter N1 (only used for observation error models)}

\item{K:}{the fitted parameer K (only used for density dependent models}

\item{type:}{must be either "Process" or "Observation". Density independent models plotted appears as a blue line, Density dependent models plotted appears as red line}

\item{add.to.plot:}{if TRUE, then adds fitted model to an existing plot.}
}
\description{
Makes plot of data and one fitted dynamic model.  Can also add an additional fitted model to an existing plot
}
\examples{

#Plot best fitting process, density independent model with data

di.mle.pars <- proc.di.fit$par
plotmodelfun(x = harborseals,
r = di.mle.pars[1],
N0= = di.mle.pars[2])
type = "Process")

# Add density dependent model fit to existing plot:

dd.mle.pars <- proc.dd.fit$par
plotmodelfun(x = harborseals,
r = dd.mle.pars[1],
K = dd.mle.pars[2],
N0 = dd.mle.pars[3]
type = "Process",
add.to.plot = TRUE)
}
