
#' Auxiliary for Controlling \code{pclm} and \code{pclm2D} Fitting
#' 
#' @usage 
#' pclm.control(lambda     = 1,
#'              kr         = 7,
#'              deg        = 3,
#'              int.lambda = c(1, 100),
#'              int.kr     = c(4, 10),
#'              int.deg    = c(2, 5),
#'              diff       = 3,
#'              opt.method = "BIC",
#'              max.iter   = 100,
#'              tol        = 1e-5)
#' @param lambda Smoothing parameter to be used in pclm estimation. Default: 1.
#' If \code{lambda = NA} an algorithm will find the optimal values.
#' @param kr Knot ratio. Number of internal intervals used for defining 1 knot in 
#' B-spline basis construction. See \code{\link{MortSmooth_bbase}}. Default: 7.
#' If \code{ndx = NA} an algorithm will find the optimal values.
#' @param deg Degree of the splines needed to create equally-spaced B-splines 
#' basis over an abscissa of data. Default: 3. If \code{deg = NA} an algorithm 
#' will find the optimal values.
#' @param int.lambda If \code{lambda} is optimized an interval to be searched 
#' needs to be specified. Format: vector containing the end-points.
#' Default: \code{c(1, 100)}.
#' @param int.kr If \code{kr} is optimized an interval to be searched 
#' needs to be specified. Format: vector containing the end-points.
#' Default: \code{c(4, 10)}. \code{kr < 5} might slow down considerably
#' the algorithm in the case of \code{pclm2D} or 'overfit' in the case 
#' of \code{pclm}. Fell free to play around with it.
#' @param int.deg If \code{deg} is optimized an interval to be searched 
#' needs to be specified. Format: vector containing the end-points.
#' Default: \code{c(3, 5)}.
#' @param diff An integer indicating the order of differences of the components 
#' of PCLM coefficients. If not sure what it means leave untouched. 
#' Default value: 3.
#' @param opt.method Selection criterion of the model.
#' Possible values are \code{"AIC"} and \code{"BIC"}. Default: \code{"BIC"}.
#' @param max.iter Maximal number of iterations used in fitting procedure.
#' Default: 100.
#' @param tol Tolerance in PCLM fitting procedure.
#' @inherit pclm references
#' @return List with control parameters.
#' @seealso \code{\link{pclm}} \code{\link{pclm2D}}
#' @export
pclm.control <- function(lambda = 1,
                         kr = 7,
                         deg = 3,
                         int.lambda = c(1, 100),
                         int.kr = c(4, 10),
                         int.deg = c(2, 5),
                         diff = 3,
                         opt.method = "BIC",
                         max.iter = 100, 
                         tol = 1e-5){
  out <- c(as.list(environment()))
  return(out)
}
