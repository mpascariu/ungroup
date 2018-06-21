
#' Imports
#' 
#' @importFrom stats optimise qnorm quantile fitted aggregate nlminb
#' @importFrom utils tail
#' @importFrom MortalitySmooth MortSmooth_bbase
#' @importFrom graphics axis barplot legend lines plot abline par
#' @import pbapply
#' @import rgl
#' @importFrom Rcpp sourceCpp
#' @import RcppEigen
#' @useDynLib ungroup
#' @name foo_imports
#' @keywords internal
#' 
NULL

#' Test Dataset in the Package
#' 
#' Dataset containing death counts (Dx) and exposures (Ex)  by age for a 
#' certain population between 1980 and 2014. The data-set is provided for 
#' testing purposes only and might be altered and outdated. Download actual 
#' demographic data free of charge from HMD. Once a username and a password is 
#' created on the \href{http://www.mortality.org}{website} the 
#' \href{https://CRAN.R-project.org/package=MortalityLaws}{MortalityLaws} 
#' R package can be used to extract data in R format.
#' @source \href{http://www.mortality.org}{Human Mortality Database}
#' @name ungroup.data 
#' 
"ungroup.data"


#' Print function for \code{ungroup.data}
#' 
#' @param x An \code{ungroup.data} object
#' @inheritParams base::print
#' @keywords internal
#' @export
print.ungroup.data <- function(x, ...) {
  cat("\nungroup Test Dataset\n")
  cat(" Series   : Death counts and Exposures by age (Dx & Ex)\n")
  cat(" Years    : 1980 - 2014\n")
  cat(" Ages     : 0 - 110\n")
  cat(" Format   : List containg 2 data frames\n")
  cat(" Source   : Human Mortality Database\n")
  cat(" Download : January 17, 2018")
}
