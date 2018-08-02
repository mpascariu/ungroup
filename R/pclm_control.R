
#' Auxiliary for Controlling \code{pclm} Fitting
#' 
#' @usage 
#' pclm.control(lambda     = NA,
#'              kr         = 2,
#'              deg        = 3,
#'              int.lambda = c(0.1, 1e+5),
#'              diff       = 2,
#'              opt.method = "BIC",
#'              max.iter   = 1e+3,
#'              tol        = 1e-3)
#' @param lambda Smoothing parameter to be used in pclm estimation.
#' If \code{lambda = NA} an algorithm will find the optimal values.
#' @param kr Knot ratio. Number of internal intervals used for defining 1 knot in 
#' B-spline basis construction. See \code{\link{MortSmooth_bbase}}.
#' @param deg Degree of the splines needed to create equally-spaced B-splines 
#' basis over an abscissa of data.
#' @param int.lambda If \code{lambda} is optimized an interval to be searched 
#' needs to be specified. Format: vector containing the end-points.
#' @param diff An integer indicating the order of differences of the components 
#' of PCLM coefficients. Default value: 2.
#' @param opt.method Selection criterion of the model.
#' Possible values are \code{"AIC"} and \code{"BIC"}. Default: \code{"BIC"}.
#' @param max.iter Maximal number of iterations used in fitting procedure.
#' @param tol Relative tolerance in PCLM fitting procedure. Default: 0.1\% i.e. 
#' the estimated aggregate bins should be in the 0.1\% error margin.
#' @return List with control parameters.
#' @seealso \code{\link{pclm}}
#' @export
pclm.control <- function(lambda = NA,
                         kr = 2,
                         deg = 3,
                         int.lambda = c(0.1, 1e+5),
                         diff = 2,
                         opt.method = "BIC",
                         max.iter = 1e+3, 
                         tol = 1e-3){
  out <- c(as.list(environment()))
  return(out)
}


#' Auxiliary for Controlling \code{pclm2D} Fitting
#' 
#' @usage 
#' pclm2D.control(lambda     = c(1, 1),
#'                kr         = 7,
#'                deg        = 3,
#'                int.lambda = c(0.1, 1e+3),
#'                diff       = 2,
#'                opt.method = "BIC",
#'                max.iter   = 1e+3,
#'                tol        = 1e-3)
#' @inheritParams pclm.control
#' @seealso \code{\link{pclm2D}}
#' @export
pclm2D.control <- function(lambda = c(1, 1),
                           kr = 7,
                           deg = 3,
                           int.lambda = c(0.1, 1e+3),
                           diff = 2,
                           opt.method = "BIC",
                           max.iter = 1e+3, 
                           tol = 1e-3){
  out <- c(as.list(environment()))
  return(out)
}



