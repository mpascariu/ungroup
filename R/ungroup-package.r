# -------------------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last Update: Fri Oct 02 17:06:29 2026
# -------------------------------------------------------------- #

#' @details 
#' To learn more about the package, start with the vignettes:
#' \code{browseVignettes(package = "ungroup")}
#' \insertNoCite{*}{ungroup}
#' @references \insertAllCited{}
#' @importFrom Rcpp evalCpp
#' @importFrom stats optimise qnorm fitted aggregate nlminb AIC BIC
#' @importFrom utils tail
#' @importFrom graphics axis barplot legend lines plot.default persp
#' @importFrom pbapply startpb setpb closepb
#' @importFrom grDevices colorRampPalette
#' @import Rdpack
#' @importClassesFrom Matrix dgCMatrix
#' @name ungroup
#' @useDynLib ungroup
#' @docType package
"_PACKAGE"
