
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
#' A vector of the same length as x and y. See \insertCite{rizzi2015;textual}{ungroup} 
#' for further details.
#' @param out.step Length of estimated intervals in output. 
#' Values between 0.1 and 1 are accepted. Default: 1.
#' @param ci.level Level of significance for computing confidence intervals. 
#' Default: \code{95}.
#' @param verbose Logical value. Indicates whether a progress bar should be shown or not.
#' Default: \code{FALSE}.
#' @param control List with additional parameters: \itemize{
#'   \item{\code{lambda}} -- Smoothing parameter to be used in pclm estimation.
#'   If \code{lambda = NA} an algorithm will find the optimal values.
#'   \item{kr} -- Knot ratio. Number of internal intervals used for defining 
#'   1 knot in B-spline basis construction. See \code{\link{MortSmooth_bbase}}.
#'   \item{\code{deg}} -- Degree of the splines needed to create equally-spaced 
#'   B-splines basis over an abscissa of data.
#'   \item{\code{int.lambda}} -- If \code{lambda} is optimized an interval to be 
#'   searched needs to be specified. Format: vector containing the end-points.
#'   \item{\code{diff}} -- An integer indicating the order of differences of the 
#'   components of PCLM coefficients.
#'   \item{\code{opt.method}} -- Selection criterion of the model.
#'   Possible values are \code{"AIC"} and \code{"BIC"}.
#'   \item{\code{max.iter}} -- Maximal number of iterations used in fitting procedure.
#'   \item{\code{tol}} -- Relative tolerance in PCLM fitting procedure.}
#' 
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
#' @return \item{deep}{ A list of objects created in the fitting process. Useful 
#' in diagnosis of possible issues.}
#' @return \item{call}{ An unevaluated function call, that is, an unevaluated 
#' expression which consists of the named function applied to the given arguments.}
#' @seealso \code{\link{control.pclm}}, \code{\link{plot.pclm}}.
#' @references \insertAllCited{}
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
#' # Example 5 -----------------------
#' # Grouped x & ungrouped offset (estimate death rates)
#' 
#' ungroupped_Ex <- pclm(x, y = offset, nlast, offset = NULL)$fitted # ungroupped offset data
#' 
#' M5 <- pclm(x, y, nlast, offset = ungroupped_Ex)
#' @export
pclm <- function(x, y, nlast, offset = NULL, out.step = 1, ci.level = 95, 
                 verbose = FALSE, control = list()){
  # Check input
  control <- do.call("control.pclm", control)
  input   <- I <- as.list(environment()) # save all the input for later use
  I$nlast <- validate.nlast(x, nlast, out.step)
  type    <- "1D"
  pclm.input.check(input, type)
  
  # Preliminary; start the clock
  if (verbose) {pb = startpb(0, 100); on.exit(closepb(pb)); setpb(pb, 1)}
  I[c("x", "y", "nlast", "offset")] <- create.artificial.bin(I) # ***
  
  # Deal with offset term
  if (!is.null(offset)) {
    if (length(offset) == length(y)) {
      if (verbose) { setpb(pb, 5); cat("   Ungrouping offset")}
      I$offset <- pclm(x = I$x, y = I$offset, I$nlast, offset = NULL, 
                       out.step, ci.level, verbose = F, control)$fitted
    } 
  }
  
  # Find lambda
  L <- I$control$lambda
  if (any(is.na(L))) L <- optimize_par(I, type)
  
  # solve the PCLM 
  M <- with(control, pclm.fit(I$x, I$y, I$nlast, I$offset, out.step, verbose,
                              lambda = L, kr, deg, diff, max.iter, tol, type))
  SE <- with(M, compute_standard_errors(B, QmQ, QmQP))
  R  <- with(M, pclm.confidence(fit, out.step, y, SE, ci.level, type, offset))
  R  <- delete.artificial.bin(R) # ***
  G  <- map.bins(x, nlast, out.step)
  dn <- G$output$names
  names(R$fit) = names(R$lower) = names(R$upper) = names(R$SE) <- dn
  
  # Output
  Fcall <- match.call()
  Par <- with(control, c(lambda = L, kr = kr, deg = deg))
  gof <- list(AIC = AIC.pclm(M), BIC = BIC.pclm(M), standard.errors = R$SE)
  ci  <- list(upper = R$upper, lower = R$lower)
  out <- list(input = input, fitted = R$fit, ci = ci, goodness.of.fit = gof,
              smoothPar = Par, bin.definition = G, deep = M, call = Fcall)
  out <- structure(class = "pclm", out)
  if (verbose) setpb(pb, 100)
  return(out)  
}

# ----------------------------------------------

#' Extract PCLM Deviance Residuals
#' 
#' @inherit stats::residuals params return
#' @examples 
#' x <- c(0, 1, seq(5, 85, by = 5))
#' y <- c(294, 66, 32, 44, 170, 284, 287, 293, 361, 600, 998, 
#'        1572, 2529, 4637, 6161, 7369, 10481, 15293, 39016)
#' M1 <- pclm(x, y, nlast = 26)
#' 
#' residuals(M1)
#' @export
residuals.pclm <- function(object, ...) {
  if (!is.null(object$input$offset)) {
    stop("residuals method not implemented for hazard rates", call. = F)
  }
  C <- object$deep$C
  C <- C[-nrow(C), -ncol(C)]
  
  y.obs <- object$input$y
  y.hat <- as.numeric(C %*% fitted(object))
  res <- y.obs - y.hat
  names(res) <- object$bin.definition$input$names
  return(res)
}


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
  cat("\n")
}

#' Summary for pclm method
#' @inheritParams base::summary
#' @keywords internal
#' @export
summary.pclm <- function(object, ...) {
  cl    <- object$call
  AIC   <- round(object$goodness.of.fit$AIC, 2)
  BIC   <- round(object$goodness.of.fit$BIC, 2)
  sPar  <- round(object$smoothPar)
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
    cat("\nPCLM Type                    : Univariate")
    cat("\nNumber of input groups       :", dim.y)
    cat("\nNumber of fitted values      :", dim.f)
    cat("\nLength of estimate bins      :", out.step)
    cat("\nSmoothing parameter lambda   :", sPar[1])
    cat("\nB-splines intervals/knot (kr):", sPar[2])
    cat("\nB-splines degree (deg)       :", sPar[3])
    cat("\nAIC                          :", AIC)
    cat("\nBIC                          :", BIC)
    cat("\n")
  })
}



