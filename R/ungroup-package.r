# -------------------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last Update: Mon Jan 29 15:09:21 2024
# -------------------------------------------------------------- #

#' @details 
#' To learn more about the package, start with the vignettes:
#' \code{browseVignettes(package = "ungroup")}
#' \insertNoCite{*}{ungroup}
#' @references \insertAllCited{}
#' @importFrom Rcpp sourceCpp
#' @importFrom stats optimise qnorm quantile fitted aggregate nlminb AIC BIC
#' @importFrom utils tail
#' @importFrom graphics axis barplot legend lines abline par plot.default persp
#' @importFrom pbapply startpb setpb closepb
#' @importFrom grDevices colorRampPalette
#' @import Rdpack
#' @importClassesFrom Matrix dgCMatrix
#' @name ungroup
#' @useDynLib ungroup
#' @docType package
"_PACKAGE"
