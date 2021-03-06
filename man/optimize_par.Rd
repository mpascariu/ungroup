% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pclm_optim.R
\name{optimize_par}
\alias{optimize_par}
\title{Optimize Smoothing Parameters
This function optimize searches of \code{lambda, kr} and \code{deg}. 
See \code{\link{control.pclm}} to see what is their meaning. 
The optimization process works in steps. Simultaneous optimization was 
tested and found inefficient.}
\usage{
optimize_par(I, type)
}
\arguments{
\item{I}{Input object from pclm function}

\item{type}{Type of PCLM model. Options: \code{"1D", "2D"} for 
univariate and two-dimensional model respectively.}
}
\description{
Optimize Smoothing Parameters
This function optimize searches of \code{lambda, kr} and \code{deg}. 
See \code{\link{control.pclm}} to see what is their meaning. 
The optimization process works in steps. Simultaneous optimization was 
tested and found inefficient.
}
\keyword{internal}
