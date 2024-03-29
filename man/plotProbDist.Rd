% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotProbDist.R
\name{plotProbDist}
\alias{plotProbDist}
\title{View Probability Distributions}
\usage{
plotProbDist(plottype = "binomial", viewcode = FALSE)
}
\arguments{
\item{plottype}{a character string that is either
\itemize{
\item 'binomial'
\item 'poisson'
\item 'negativebinomial'
\item 'normal'
\item 'gamma'
\item 'studentst'
}}

\item{viewcode}{TRUE or FALSE (default) indicating whether to print the function code}
}
\description{
Compare probability-mass and -density functions, as illustrated in Figures 7.1 - 7.6.
}
\examples{
# Generate figure 7.1
plotProbDist("binomial")
# View all plotting code
plotProbDist(viewcode = TRUE)
}
