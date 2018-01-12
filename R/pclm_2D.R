
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
#' @inheritParams pclm
#' @inherit pclm return
#' @seealso \code{\link{plot.pclm2D}}
#' @examples 
#' # Input data
#' Dx <- ungroup.data$Dx
#' Ex <- ungroup.data$Ex
#' 
#' # Aggregate data
#' x      <- c(0, 1, seq(5, 85, by = 5))
#' nlast  <- 26
#' n      <- c(diff(x), nlast)
#' Ex$gr  <- Dx$gr <- rep(x, n)
#' y      <- aggregate(Dx[, 1:35], by = list(Dx$gr), FUN = "sum")[, -1]
#' offset <- aggregate(Ex[, 1:35], by = list(Ex$gr), FUN = "sum")[, -1]
#' 
#' # Example 1 ---------------------- 
#' # Fit model and ungroup data using PCLM-2D
#' P1 <- pclm2D(x, y, nlast)
#' summary(P1)
#' # plot(P1)
#' 
#' # Example 2 ---------------------- 
#' # Ungroup and build a mortality surface
#' P2 <- pclm2D(x, y, nlast, offset)
#' summary(P2)
#' 
#' \dontrun{
#' plot(P2)                      # plot
#' library(rgl)
#' snapshot3d("plotP2.jpeg")     # save the plot in jpeg format
#' aspect3d(x = 1, y = 2, z = 1) # modify the aspect ratio
#' }
#' 
#' @export
pclm2D <- function(x, y, nlast, offset = NULL, show = TRUE, ci.level = 0.05,
                   out.step = 1, control = list()) {
  # Check input
  control     <- do.call("pclm2D.control", control)
  input       <- as.list(environment())
                 pclm.input.check(input, "2D")
  input$nlast <- validate.nlast(x, nlast, out.step)
  
  # Preliminary; start the clock
  if (show) {pb = startpb(0, 100); on.exit(closepb(pb)); setpb(pb, 1)}
  I   <- create.artificial.bin(input)
  Par <- with(control, c(lambda = lambda, kr = kr, deg = deg))
  
  # Deal with offset term
  if (!is.null(offset)) {
    if (show) { setpb(pb, 5); cat("   Ungrouping offset")}
    pclmEx <- pclm2D(x = I$x, y = I$offset, I$nlast, offset = NULL, 
                     show = F, ci.level, out.step, control)
    I$offset <- fitted(pclmEx)
  }
  
  # If smoothing parameters are not provided in input, find them automatically.
  if (any(is.na(Par))) { 
    # IF 'out.step < 1' the algorithm can becomes slow (several minutes slow).
    Par <- optimize_par(I$x, I$y, I$nlast, I$offset, show,
                         out.step, control, pclm.type = "2D")
  }
  
  # solve the PCLM 
  M <- with(control, pclm.fit(I$x, I$y, I$nlast, I$offset, out.step, show,
                              lambda = Par[1], kr = Par[2], deg = Par[3], 
                              diff, max.iter, tol, pclm.type = "2D"))
  cn    <- c("fit", "lower", "upper", "s.e.")
  M[cn] <- pclm.confidence(M, ci.level, pclm.type = "2D")
  M     <- delete.artificial.bin(M) # ***
  G     <- map.bins(x, nlast, out.step)
  dn    <- list(G$output$names, colnames(y))
  dimnames(M$fit) = dimnames(M$lower) = dimnames(M$upper) = dimnames(M$s.e.) <- dn
  
  # Output
  gof <- list(AIC = M$AIC, BIC = M$BIC, standard.errors = M$s.e.)
  ci  <- list(upper = M$upper, lower = M$lower)
  out <- list(input = input, fitted = M$fit, ci = ci, goodness.of.fit = gof,
              smoothPar = Par, bin.definition = G)
  out      <- structure(class = "pclm2D", out)
  out$call <- match.call()
  if (show) setpb(pb, 100)
  return(out)
}


# ----------------------------------------------

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
  L     <- object$smoothPar
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
    cat("\nPCLM Type                  : Two-Dimensional")
    cat("\nNumber of input groups     :", dim.y[1], "x", dim.y[2])
    cat("\nNumber of fitted values    :", dim.f[1], "x", dim.f[2])
    cat("\nDimension of estimate bins :", out.step, "x 1")
    cat("\nSmoothing parameter lambda :", L[1])
    cat("\nB-splines intervals/knot   :", L[2])
    cat("\nB-splines degree           :", L[3])
    cat("\nAIC                        :", AIC)
    cat("\nBIC                        :", BIC)
  })
}



