
#' Two-Dimensional Penalized Composite Link Model (PCLM-2D)
#' 
#' Fit two-dimensional penalized composite link model (PCLM-2D), 
#' e.g. simultaneous ungrouping of age-at-death distributions grouped in age 
#' classes for adjacent years. The PCLM can be extended to a two-dimensional 
#' regression problem. This is particularly suitable for mortality analysis 
#' when mortality surfaces are to be estimated to capture both age-specific 
#' trajectories of coarsely grouped distributions and time trends.
#' @param y data.frame with counts to be ungrouped. The number of rows 
#' should be equal with the length of \code{x}.
#' @param verbose Logical value. Indicates whether a progress bar should be shown or not.
#' Default: \code{TRUE}.
#' @inheritParams pclm
#' @inherit pclm return
#' @seealso \code{\link{control.pclm2D}}, \code{\link{plot.pclm2D}}.
#' @examples 
#' # Input data
#' Dx <- ungroup.data$Dx
#' Ex <- ungroup.data$Ex
#' 
#' # Aggregate data to ungroup it in the examples below
#' x      <- c(0, 1, seq(5, 85, by = 5))
#' nlast  <- 26
#' n      <- c(diff(x), nlast)
#' group  <- rep(x, n)
#' y      <- aggregate(Dx, by = list(group), FUN = "sum")[, -1]
#' offset <- aggregate(Ex, by = list(group), FUN = "sum")[, -1]
#' 
#' # Example 1 ---------------------- 
#' # Fit model and ungroup data using PCLM-2D
#' P1 <- pclm2D(x, y, nlast)
#' summary(P1)
#' # plot(P1)
#' 
#' \dontrun{
#' # NOTE: pclm2D does not search for optimal smoothing parameters by default
#' # (like pclm) because it is more time consuming. If optimization is required
#' # set lambda = c(NA, NA):
#' 
#' P1 <- pclm2D(x, y, nlast, control = list(lambda = c(NA, NA)))
#' 
#' # Example 2 ---------------------- 
#' # Ungroup and build a mortality surface
#' P2 <- pclm2D(x, y, nlast, offset)
#' summary(P2)
#' 
#' plot(P2)                      # plot
#' library(rgl)
#' snapshot3d("plotP2.jpeg")     # save the plot in jpeg format
#' aspect3d(x = 1, y = 2, z = 1) # modify the aspect ratio
#' }
#' 
#' @export
pclm2D <- function(x, y, nlast, offset = NULL, verbose = TRUE, 
                   ci.level = 95, out.step = 1, control = list()) {
  # Check input
  control <- do.call("control.pclm2D", control)
  input   <- I <- as.list(environment()) # save all the input for later use
  I$nlast <- validate.nlast(x, nlast, out.step)
  type    <- "2D"
  pclm.input.check(input, type)
  
  # Preliminary; start the clock
  if (verbose) {pb = startpb(0, 100); on.exit(closepb(pb)); setpb(pb, 1)}
  I[c("x", "y", "nlast", "offset")] <- create.artificial.bin(I) # ***
  
  # Deal with offset term
  if (!is.null(offset)) {
    if (all(dim(offset) == dim(y))) {
      if (verbose) { setpb(pb, 5); cat("   Ungrouping offset")}
      I$offset <- pclm2D(x = I$x, y = I$offset, I$nlast, offset = NULL, 
                         verbose = F, ci.level, out.step, control)$fitted
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
  dn <- list(G$output$names, colnames(y))
  dimnames(R$fit) = dimnames(R$lower) = dimnames(R$upper) = dimnames(R$SE) <- dn
  
  # Output
  Fcall <- match.call()
  Par <- with(control, c(lambda.x = L[1], lambda.y = L[2], kr = kr, deg = deg))
  gof <- list(AIC = AIC.pclm(M), BIC = BIC.pclm(M), standard.errors = R$SE)
  ci  <- list(upper = R$upper, lower = R$lower)
  out <- list(input = input, fitted = R$fit, ci = ci, goodness.of.fit = gof,
              smoothPar = Par, bin.definition = G, deep = M, call = Fcall)
  out <- structure(class = "pclm2D", out)
  if (verbose) setpb(pb, 100)
  return(out)
}


# ----------------------------------------------

#' Extract PCLM-2D Deviance Residuals
#' 
#' @inherit stats::residuals params return
#' @examples 
#' 
#' Dx <- ungroup.data$Dx
#' Ex <- ungroup.data$Ex
#' 
#' # Aggregate data to ungroup it in the example below
#' x      <- c(0, 1, seq(5, 85, by = 5))
#' nlast  <- 26
#' n      <- c(diff(x), nlast)
#' group  <- rep(x, n)
#' y      <- aggregate(Dx, by = list(group), FUN = "sum")[, -1]
#' 
#' # Example
#' P1 <- pclm2D(x, y, nlast)
#' 
#' residuals(P1)
#' @export
residuals.pclm2D <- function(object, ...) {
  if (!is.null(object$input$offset)) {
    stop("residuals method not implemented for hazard rates", call. = F)
  }
  C  <- object$deep$C
  x  <- object$input$x
  y  <- object$input$y
  nr <- 1:length(x)
  nc <- 1:(ncol(C) / length(y) - 1)
  y.hat <- C[nr, nc] %*% fitted(object)
  res <- y - y.hat
  rownames(res) <- object$bin.definition$input$names
  
  return(res)
}


#' Print method for pclm2D
#' @param x An object of class \code{"pclm2D"}
#' @inheritParams base::print
#' @keywords internal
#' @export
print.pclm2D <- function(x, ...){
  cat("\nPenalized Composite Link Model (PCLM)")
  cat("\nPCLM Type                  : Two-Dimensional")
  cat("\nNumber of input  groups    :", nrow(x$input$y), "x", ncol(x$input$y))
  cat("\nNumber of fitted values    :", nrow(x$fitted), "x", ncol(x$fitted))
  cat("\nDimension of estimate bins :", x$input$out.step, "x 1\n")
  cat("\n")
}

#' Summary method for pclm2D
#' Generic function used to produce result summaries of the results produced 
#' by \code{\link{pclm2D}}.
#' @inheritParams base::summary
#' @keywords internal
#' @export
summary.pclm2D <- function(object, ...) {
  cl    <- object$call
  AIC   <- round(object$goodness.of.fit$AIC, 2)
  BIC   <- round(object$goodness.of.fit$BIC, 2)
  sPar  <- round(object$smoothPar, 2)
  dim.y <- dim(object$input$y)
  dim.f <- dim(fitted(object))
  out.step <- object$input$out.step
  out = structure(class = "summary.pclm2D", as.list(environment()))
  return(out)
}

#' Print method for summary.pclm2D
#' @param x An object of class \code{"summary.pclm2D"}
#' @inheritParams base::print
#' @keywords internal
#' @export
print.summary.pclm2D <- function(x, ...) {
  with(x, {
    cat("\nPenalized Composite Link Model (PCLM)")
    cat("\n\nCall:\n"); print(cl)
    cat("\nPCLM Type                    : Two-Dimensional")
    cat("\nNumber of input groups       :", dim.y[1], "x", dim.y[2])
    cat("\nNumber of fitted values      :", dim.f[1], "x", dim.f[2])
    cat("\nDimension of estimate bins   :", out.step, "x 1")
    cat("\nSmoothing parameter lambda   :", sPar[1], "x", sPar[2])
    cat("\nB-splines intervals/knot (kr):", sPar[3])
    cat("\nB-splines degree (deg)       :", sPar[4])
    cat("\nAIC                          :", AIC)
    cat("\nBIC                          :", BIC)
    cat("\n")
  })
}



