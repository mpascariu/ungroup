# --------------------------------------------------- #
# Author: Marius D. Pascariu
# License: GPL-3
# Last update: Sat Aug 03 17:23:49 2019
# --------------------------------------------------- #

#' Validate input values
#' 
#' @param X A list with input arguments provided in \code{\link{pclm}} function
#' @inheritParams pclm.fit
#' @keywords internal
pclm.input.check <- function(X, pclm.type) {
  # Validate the other arguments
  with(X, {
    if (!is.numeric(x)) {
      stop("'x' must be a vector of class numeric", call. = FALSE)
    }
    if (any(is.na(x))) {
      stop("'x' contains NA values", call. = FALSE)
    }
    if (any(is.na(y))) {
      stop("'y' contains NA values", call. = FALSE)
    }
    if (any(y < 0)) {
      stop("'y' contains negative values. The counts are always positive.", 
           call. = FALSE)
    }
    if (ci.level <= 50.1 || ci.level >= 99.9) {
      stop("'ci.level' must take values in the [50.1, 99.9] interval", 
           call. = FALSE)
    }
    if (out.step > 1 || out.step < 0.1) {
      stop("'out.step' must be between 0.1 and 1", call. = FALSE)
    }
    if (pclm.type == "1D") {
      if (length(x) != length(y)) {
        stop("length of 'x' and 'y' must be equal", call. = FALSE)
      }
    }
    if (pclm.type == "2D") {
      if (!(is.data.frame(y) || is.matrix(y))) {
        stop("'y' must be a data.frame or a matrix", call. = FALSE)
      }
      if (length(x) != nrow(y)) {
        stop("length(x) must be equal to nrow(y)", call. = FALSE)
      }
    }
    
  })
  
  # Validate input in pclm.control
  with(X$control, {
    if (any(!is.na(lambda)) && any(lambda < 0)) {
      stop("'lambda' must be a positive scalar", call. = FALSE)
    }
    if (!is.na(kr)) {
      if (kr <= 0 || frac(kr) != 0) stop("'kr' must be a positive integer", 
                                         call. = FALSE)
    }
    if (!is.na(deg)) {
      if (deg < 2 || frac(deg) != 0) 
        stop("'deg' must be a positive integer greater or equal than 2", 
             call. = FALSE)
    }
    if (!(opt.method[1] %in% c('BIC','AIC'))) {
      stop("'AIC' or 'BIC' should be used as opt.method", call. = FALSE)
    }
    if (max.iter < 10) {
      stop("'max.iter' should be at least 10 for a decent run", call. = FALSE)
    }
    if (tol <= 0) {
      stop("'tol' must be greater than 0", call. = FALSE)
    }
  })
}


#' Check if \code{nlast} needs to be adjusted in order to accommodate 
#' \code{out.step}
#' @inheritParams pclm
#' @keywords internal
validate.nlast <- function(x, nlast, out.step) {
  if (length(nlast) != 1) {
    stop("'nlast' has to be a scalar. length(nlast) must be equal to 1.", 
         call. = FALSE)
  }
  if (nlast <= 0) {
    stop("'nlast' must be greater than 0", call. = FALSE)
  }
  
  len <- max(x) - min(x) + nlast
  N   <- len/out.step
  if (frac(N) != 0) {
    n.bins <- round(N, 0)
    new.nlast <- n.bins * out.step - len + nlast
    vos <- suggest.valid.out.step(len)
    warning("'nlast' has been adjusted in order to obtain ", n.bins, 
            " bins of equal length as specified in 'out.step = ", out.step,
            "'. Now 'nlast = ", new.nlast, "'. The impact in results should be",
            " insignificant. However, if the adjustment is not acceptable",
            " try out one of the following 'out.step' values: ", 
            paste(vos, collapse = ", "), ".", call. = FALSE)
  } else {
    new.nlast <- nlast
  }
  return(new.nlast)
}


#' Sequence function with last value
#' 
#' @inheritParams base::seq
#' @keywords internal 
seqlast <- function(from, to, by) 
{
  vec <- do.call(what = seq, args = list(from, to, by))
  if ( tail(vec, 1) != to ) {
    return(c(vec, to))
  } else {
    return(vec)
  }
}


#' Extract Fractional Part of a Number
#' @param x A numeric value, vector or matrix
#' @keywords internal
frac <- function(x) {
  x - trunc(x)
}


#' Suggest values of \code{out.step} that do not 
#' require an adjustment of \code{nlast}
#' @param len Interval length
#' @param increment Increment
#' @keywords internal
suggest.valid.out.step <- function(len, increment = 0.01) {
  o  <- seq(0.1, 1, by = increment)
  v  <- len/o
  tv <- trunc(v)
  o[v == tv]
}
