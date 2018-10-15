
#' Fit PCLM Models
#' 
#' This is an internal function used to estimate PCLM model. It is used by 
#' \code{\link{pclm}} and \code{\link{pclm2D}} functions
#' @inheritParams pclm
#' @inheritParams control.pclm
#' @param type Type of PCLM model. Options: \code{"1D", "2D"} for 
#' univariate and two-dimensional model respectively.
#' @keywords internal
pclm.fit <- function(x, y, nlast, offset, out.step, verbose,
                     lambda, kr, deg, diff, max.iter, tol, type){
  
  if (verbose) { # Setup progress bar
    pb = startpb(0, 100)
    setpb(pb, 50)
    cat("   Ungrouping data      ")
  }
  
  # Some preparations
  CM   <- build_C_matrix(x, y, nlast, offset, out.step, type) # composition matrix
  BM   <- build_B_spline_basis(CM$gx, CM$gy, kr, deg, diff, type) # B-spline
  P    <- build_P_matrix(BM$BA, BM$BY, lambda, type) # penalty
  C    <- CM$C
  B    <- BM$B
  y_   <- as.vector(unlist(y))
  ny_  <- length(y_)
  K    <- pclm_loop(asSparseMat(C), P, B, y_, max.iter, tol)
  QmQ  <- K$QmQ
  QmQP <- K$QmQP
  fit  <- as.numeric(K$mu)
  
  # Regression diagnostics
  H     <- solve(QmQP, QmQ)
  trace <- sum(diag(H))
  y_[y_ == 0] <- 10^-4
  dev   <- 2 * sum(y_ * log(y_ / K$muA), na.rm = TRUE)
  out   <- as.list(environment())
  return(out)
}


#' Build Composition Matrices
#' @inheritParams pclm.fit
#' @keywords internal
build_C_matrix <- function(x, y, nlast, offset, out.step, type) {
  # Build C matrix in the age direction
  nx <- length(x)
  gx <- seq(min(x), max(x) + nlast - out.step, by = out.step)
  gu <- c(diff(x), nlast)/out.step
  CA <- matrix(0, nrow = nx, ncol = sum(gu), dimnames = list(x, gx))
  xr <- c(x[-1], max(x) + nlast)
  for (j in 1:nx) CA[j, which(gx >= x[j] & gx < xr[j])] <- 1
  
  # Build C matrix in the year direction
  if (type == "1D") {
    ny <- length(y)
    CY <- NULL
    C  <- CA
  } else {
    ny <- ncol(y)
    CY <- diag(1, ncol = ny, nrow = ny) 
    C  <- CY %x% CA # Kronecker product
  }
  gy <- 1:ny
  if (!is.null(offset)) C <- C %*% diag(as.vector(unlist(offset)))
  
  # Output
  out <- as.list(environment())
  return(out)
}


#' Construct B-spline basis
#' This is an internal function which constructs B-spline basis to be used in 
#' pclm estimation
#' @param X vector with ages
#' @param Y vector with years
#' @inheritParams pclm.fit
#' @seealso \code{\link{MortSmooth_bbase}}
#' @keywords internal
build_B_spline_basis <- function(X, Y, kr, deg, diff, type) {
  # B-spline basis 
  bsb <- function(Z, kr, deg, diff) {
    zl   <- min(Z)
    zr   <- max(Z)
    zmin <- zl - 0.01 * (zr - zl)
    zmax <- zr + 0.01 * (zr - zl)
    ndx  <- trunc(length(Z)/kr) # number of internal knots
    B    <- MortSmooth_bbase(x = Z, zmin, zmax, ndx, deg) 
    dg   <- diag(ncol(B))
    D    <- diff(dg, diff = diff)
    tD   <- t(D) %*% D
    list(B = B, tD = tD, dg = dg)
  }
  
  BA  <- bsb(X, kr, deg, diff) # for ages
  BY  <- bsb(Y, kr, deg, diff) # for years
  B   <- if (type == "1D") BA$B else BY$B %x% BA$B
  out <- as.list(environment())
  return(out)
} 


#' Construct Penalty Matrix
#' @param BA B-spline basis object for age axis
#' @param BY B-spline basis object for year axis
#' @inheritParams pclm.fit
#' @keywords internal
build_P_matrix <- function(BA, BY, lambda, type){
  L  <- sqrt(lambda)
  if (type == "1D") {
    P <- L * BA$tD
  } else {
    Px <- BY$dg %x% BA$tD
    Py <- BY$tD %x% BA$dg
    P  <- L[1] * Px + L[2] * Py 
  }
  return(P)
}


#' Create an additional bin with a small value at the end. 
#' Improves convergence.
#' @param i A list of input values corresponding to pclm or pclm2D;
#' @param vy Numerical value of the bin created for \code{y} input;
#' @param vo Numerical values of the bin created for \code{offset} input (if the case).
#' @keywords internal
create.artificial.bin <- function(i, vy = 1, vo = 1.01){
  with(i, {
    x     <- c(x, max(x) + nlast)
    nlast <- out.step
    fn    <- if (is.vector(y)) c else rbind
    y     <- fn(y, vy)
    if (!is.null(offset)) offset <- fn(offset, vo)
    out   <- list(x = x, y = y, nlast = nlast, offset = offset)
    return(out)
  })
}


#' Delete from results the last group added artificially in pclm and pclm2D 
#' @param M A pclm.fit object
#' @keywords internal
delete.artificial.bin <- function(M){
  n <- 1
  N <- 1:n
  
  f1 <- function(x) { # method 1 - delete groups and reallocate values
    A <- rev(rev(x)[-N])
    B <- sum(rev(x)[N] - n)
    B * (A/sum(A)) + A
  }
  f2 <- function(x) { # method 2 - delete groups
    rev(rev(x)[-N])
  }
  L <- class(M$fit) == "numeric"
  M$fit   <- with(M, if (L) f1(fit)   else apply(fit, 2, FUN = f1))
  M$lower <- with(M, if (L) f1(lower) else apply(lower, 2, FUN = f1))
  M$upper <- with(M, if (L) f1(upper) else apply(upper, 2, FUN = f1))
  M$SE    <- with(M, if (L) f1(SE)  else apply(SE, 2, FUN = f2))
  return(M)
}


#' Map groups and borders
#' 
#' We assume no missing values between the bins
#' @inheritParams pclm
#' @keywords internal
map.bins <- function(x, nlast, out.step) {
  step  <- c(diff(x), nlast)
  xl    <- rev(rev(c(0, cumsum(step)))[-1]) + 1
  xr    <- xl + step - 1
  N     <- length(xl)
  delta <- x[1] - xl[1]
  bl    <- round(xl + delta, 3)
  br    <- c(bl[-1], xr[N] + 1 + delta)
  
  dnames <- list(c("left", "right"), rep("", N))
  breaks <- matrix(c(bl, br), nrow = 2, byrow = T, dimnames = dnames)
  loc    <- matrix(c(xl, xr), nrow = 2, byrow = T, dimnames = dnames)
  input  <- list(n = N, 
                 length = xr - xl + 1, 
                 names  = paste0("[", bl,",", br, ")"), 
                 breaks = breaks, 
                 location = loc)
  output <- NULL
  if (!is.null(out.step)) {
    X <- range(breaks)
    X <- seqlast(X[1], X[2], by = out.step)
    output <- map.bins(X, NULL, NULL)$input
  }
  
  out <- list(input = input, output = output)
  return(out)
}

