
#' Univariate Penalized Composite Link Model (PCLM)
#' 
#' Fit univariate penalized composite link model (PCLM) to ungroup binned 
#' count data, e.g. age-at-death distributions grouped in age classes.
#' 
#' @details The PCLM method is based on the composite link model, which extends 
#' standard generalized linear models. It implements the idea that the observed 
#' counts, interpreted as realizations from Poisson distributions, are indirect 
#' observations of a finer (ungrouped) but latent sequence. This latent sequence 
#' represents the distribution of expected means on a fine resolution and has 
#' to be estimated from the aggregated data. Estimates are obtained by 
#' maximizing a penalized likelihood. This maximization is performed efficiently 
#' by a version of the iteratively reweighted least-squares algorithm. Optimal 
#' values of the smoothing parameter are chosen by minimizing Bayesian or 
#' Akaike's Information Criterion.
#' @param x Vector containing the starting values of the input intervals/bins.
#' For example: if we have 3 bins \code{[0,5), [5,10) and [10, 15)},
#' \code{x} will be defined by the vector: \code{c(0, 5, 10)}.
#' @param y Vector with counts to be ungrouped. It must have the same dimension as \code{x}.
#' @param nlast Length of the last interval. In the example above \code{nlast} would be 5.
#' @param offset Optional offset term to calculate smooth mortality rates. 
#' A vector of the same length as x and y. See \emph{Rizzi et al.(2015)} for 
#' further details.
#' @param show Logical value. Indicates whether a progress bar should be shown or not.
#' Default: \code{TRUE}.
#' @param ci.level Level of significance for computing confidence intervals. 
#' Default: \code{0.05}.
#' @param out.step Length of estimated intervals in output. 
#' Values between 0.1 and 1 are accepted. Default: 1.
#' @param control List with additional parameters. See \code{\link{pclm.control}}.
#' @return The output is a list with the following components:
#' @return \item{input}{ A list with arguments provided in input. Saved for convenience.}
#' @return \item{fitted}{ The fitted values of the PCLM model.}
#' @return \item{ci}{ Confidence intervals around fitted values.}
#' @return \item{goodness.of.fit}{ A list containing goodness of fit measures: 
#' standard errors, AIC and BIC.} 
#' @return \item{smoothPar}{ Estimated smoothing parameters: \code{lambda, kr} 
#' and \code{deg}.}
#' @return \item{bins.definition}{ Additional values to identify the bins limits and 
#' location in input and output objects.}
#' @return \item{call}{ An unevaluated function call, that is, an unevaluated 
#' expression which consists of the named function applied to the given arguments.}
#' @seealso \code{\link{pclm2D}}
#' @references
#' \enumerate{
#' \item{Rizzi S, Gampe J, Eilers PHC. \href{https://doi.org/10.1093/aje/kwv020}{
#' Efficient estimation of smooth distributions from coarsely grouped data.} 
#' American Journal of Epidemiology, Volume 182, Issue 2, 15 July 2015, Pages 138-147.}
#' \item{Rizzi S, Thinggaard M, Engholm G, et al. \href{https://doi.org/10.1186/s12874-016-0157-8}{
#' Comparison of non-parametric methods for ungrouping coarsely aggregated data.} 
#' BMC Medical Research Methodology. 2016;16:59.}
#' \item{Eilers PHC. \href{https://doi.org/10.1177/1471082X0700700302}{
#' Ill-posed problems with counts, the composite link model and penalized 
#' likelihood.} Statistical Modelling, Volume 7, Issue 3, 1 October 2007, Pages 239-254.}
#' }
#' @examples
#' # Data  
#' x <- c(0, 1, seq(5, 85, by = 5))
#' y <- c(294, 66, 32, 44, 170, 284, 287, 293, 361, 600, 998, 
#'        1572, 2529, 4637, 6161, 7369, 10481, 15293, 39016)
#' offset <- c(114, 440, 509, 492, 628, 618, 576, 580, 634, 657, 
#'             631, 584, 573, 619, 530, 384, 303, 245, 249) * 1000
#' nlast <- 26 # the size of the last interval
#' 
#' # Example 1 ----------------------
#' M1 <- pclm(x, y, nlast)
#' ls(M1)
#' summary(M1)
#' fitted(M1)
#' plot(M1)
#' 
#' # Example 2 ----------------------
#' # ungroup even in smaller intervals
#' M2 <- pclm(x, y, nlast, out.step = 0.5)
#' plot(M2)
#' 
#' # Example 3 ----------------------
#' # Do not optimise smoothing parameters; choose your own. Faster.
#' M3 <- pclm(x, y, nlast, out.step = 0.5, 
#'            control = list(lambda = 100, kr = 10, deg = 10))
#' plot(M3)
#' 
#' summary(M2)
#' summary(M3) # not the smallest BIC here, but sometimes is not important.
#' 
#' # Example 4 -----------------------
#' # Grouped x & grouped offset (estimate death rates)
#' M4 <- pclm(x, y, nlast, offset)
#' plot(M4, type = "s")
#' 
#' @export
pclm <- function(x, y, nlast, offset = NULL, show = TRUE,
                 ci.level = 0.05, out.step = 1, control = list()){
  # Check input
  control     <- do.call("pclm.control", control)
  input       <- c(as.list(environment())) # save all the input for later use
                 pclm.input.check(input, "1D")
  input$nlast <- validate.nlast(x, nlast, out.step)
  
  # Preliminary; start the clock
  if (show) {pb = startpb(0, 100); on.exit(closepb(pb)); setpb(pb, 1)}
  I   <- create.artificial.bin(input) # ***
  Par <- with(control, c(lambda = lambda, kr = kr, deg = deg))
  
  # Deal with offset term
  if (!is.null(offset)) {
    if (show) { setpb(pb, 5); cat("   Ungrouping offset")}
    pclmEx <- pclm(x = I$x, y = I$offset, I$nlast, offset = NULL, 
                   show = F, ci.level, out.step, control)
    I$offset <- fitted(pclmEx)
  }
  
  # If smoothing parameters are not provided in input, find them automatically.
  if (any(is.na(Par))) {
    Par <- optimize_par1D(I$x, I$y, I$nlast, I$offset, show, out.step, control)
  }
  
  # solve the PCLM 
  M <- with(control, pclm.fit(I$x, I$y, I$nlast, I$offset, out.step, show,
                              lambda = Par[1], kr = Par[2], deg = Par[3],
                              diff, max.iter, tol, pclm.type = "1D"))
  cn    <- c("fit", "lower", "upper", "s.e.")
  M[cn] <- pclm.confidence(M, ci.level, pclm.type = "1D")
  M     <- delete.artificial.bin(M) # ***
  G     <- map.bins(x, nlast, out.step)
  dn    <- G$output$names
  names(M$fit) = names(M$lower) = names(M$upper) = names(M$s.e.) <- dn
  
  # Output
  gof <- list(AIC = M$AIC, BIC = M$BIC, standard.errors = M$s.e.)
  ci  <- list(upper = M$upper, lower = M$lower)
  out <- list(input = input, fitted = M$fit, ci = ci, goodness.of.fit = gof,
              smoothPar = Par, bin.definition = G)
  out      <- structure(class = "pclm", out)
  out$call <- match.call()
  if (show) setpb(pb, 100)
  return(out)  
}


# ----------------------------------------------

#' Print for pclm method
#' @param x An object of class \code{"pclm"}
#' @inheritParams base::print
#' @keywords internal
#' @export
print.pclm <- function(x, ...){
  cat("\nPenalized Composite Link Model (PCLM)")
  cat("\nPCLM Type               : Univariate")
  cat("\nNumber of input groups  :", length(x$input$x))
  cat("\nNumber of fitted values :", length(x$fitted))
  cat("\nLength of estimate bins :", x$input$out.step, "\n")
}

#' Summary for pclm method
#' @inheritParams base::summary
#' @keywords internal
#' @export
summary.pclm <- function(object, ...) {
  cl    <- object$call
  AIC   <- round(object$goodness.of.fit$AIC, 2)
  BIC   <- round(object$goodness.of.fit$BIC, 2)
  L     <- round(object$smoothPar)
  dim.y <- length(object$input$y)
  dim.f <- length(fitted(object))
  out.step <- object$input$out.step
  out <- structure(class = "summary.pclm", as.list(environment()))
  return(out)
}

#' Print for summary.pclm method
#' @param x An object of class \code{"summary.pclm"}
#' @inheritParams print.pclm
#' @keywords internal
#' @export
print.summary.pclm <- function(x, ...) {
  with(x, {
    cat("\nPenalized Composite Link Model (PCLM)")
    cat("\n\nCall:\n"); print(cl)
    cat("\nPCLM Type                  : Univariate")
    cat("\nNumber of input groups     :", dim.y)
    cat("\nNumber of fitted values    :", dim.f)
    cat("\nLength of estimate bins    :", out.step)
    cat("\nSmoothing parameter lambda :", L[1])
    cat("\nB-splines intervals/knot   :", L[2])
    cat("\nB-splines degree           :", L[3])
    cat("\nAIC                        :", AIC)
    cat("\nBIC                        :", BIC)
  })
}



