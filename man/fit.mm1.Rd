% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.mm1.R
\name{fit.mm1}
\alias{fit.mm1}
\title{performance measures}
\usage{
fit.mm1(data, n, alpha)
}
\arguments{
\item{data}{data set.}

\item{n}{number of simulations interest you want to perform.}

\item{alpha}{significance level for the Kolmogorov-Smirnov test.}
}
\value{
a list of all performance measures used.
}
\description{
It allows to evaluate the efficiency of a system by analyzing its characteristics using performance measures.
}
\examples{
fit.mm1(data,5,0.05)
}
\author{
Mayara Almeida
}
