
#' ungroup: Penalized Composite Link Model for Efficient Estimation of 
#' Smooth Distributions from Coarsely Binned Data
#' 
#' @description 
#' The \code{ungroup} R package introduces a versatile method for ungrouping 
#' histograms (binned count data) assuming that counts are Poisson distributed 
#' and that the underlying sequence on a fine grid to be estimated is smooth. 
#' The method is based on the composite link model and estimation is achieved 
#' by maximizing a penalized likelihood. Smooth detailed sequences of counts 
#' and rates are so estimated from the binned counts. Ungrouping binned data 
#' can be desirable for many reasons: Bins can be too coarse to allow for 
#' accurate analysis; comparisons can be hindered when different grouping 
#' approaches are used in different histograms; and the last interval is often 
#' wide and open-ended and, thus, covers a lot of information in the tail area. 
#' Age-at-death distributions grouped in age classes and abridged life tables 
#' are examples of binned data. Because of modest assumptions, the approach is 
#' suitable for many demographic and epidemiological applications. For a detailed 
#' description of the method and applications see \insertCite{rizzi2015;textual}{ungroup}.
#' 
#' To learn more about the package, start with the vignettes:
#' \code{browseVignettes(package = "ungroup")}
#' \insertNoCite{*}{ungroup}
#' @references \insertAllCited{}
#' @author \itemize{
#' \item {Marius D. Pascariu <rpascariu@@outlook.com>}
#' \item {Silvia Rizzi <srizzi@@health.sdu.dk>}
#' \item {Jonas Schoeley}
#' \item {Maciej J. Danko}
#' }
#' @useDynLib ungroup
#' @import pbapply Rdpack
#' @importFrom Rcpp sourceCpp
#' @importFrom stats optimise qnorm quantile fitted aggregate nlminb AIC BIC
#' @importFrom utils tail
#' @importFrom MortalitySmooth MortSmooth_bbase
#' @importFrom graphics axis barplot legend lines plot abline par
#' @importFrom rgl axes3d box3d open3d surface3d title3d
#' @name ungroup-package
#' @docType package
NULL
