# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Fri Oct 02 17:06:29 2026
# --------------------------------------------------- #

#' Validate input values
#' 
#' @param X A list with input arguments provided in \code{\link{pclm}} function
#' @param pclm.type Either \code{"1D"} or \code{"2D"}. Selects which shape
#'   rules apply to \code{y} and how many smoothing parameters are expected.
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
    if (any(!is.finite(x))) {
      stop("'x' contains non-finite values", call. = FALSE)
    }
    # unlist() so a data.frame response in the 2D model is covered too.
    yv <- unlist(y)
    # NA means "not observed". Under na.action = "omit" those rows are dropped
    # and the penalty bridges the gap; anything else non-finite is still wrong.
    if (identical(na.action, "fail") && any(is.na(yv))) {
      stop("'y' contains NA values. Use na.action = \"omit\" to smooth over them.",
           call. = FALSE)
    }
    if (any(!is.finite(yv) & !is.na(yv))) {
      stop("'y' contains non-finite values", call. = FALSE)
    }
    if (any(yv < 0, na.rm = TRUE)) {
      stop("'y' contains negative values. The counts are always positive.",
           call. = FALSE)
    }
    if (any(yv == 0, na.rm = TRUE)) {
      message(
        "Input data contains zeros. ",
        "Replace zero values with a very small number to avoid erroneous results. ",
        "If the input data contains small values, you might also want to ",
        "transform it for the purpose of ungrouping. E.g. Multiplication by 100."
      )
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
    if (is.unsorted(x, strictly = TRUE)) {
      stop("'x' must be strictly increasing", call. = FALSE)
    }
    if (is.array(y)) {
      stop("'y' argument should be a numeric vector or a data.frame.", 
           call. = FALSE)
    }
  })
  
  # Validate input in pclm.control
  with(X$control, {
    # Shape before values. The 1D model has one smoothing parameter and the 2D
    # model has two, and a wrong length used to be recycled into the penalty
    # matrix and return an all-NaN fit without a word.
    n.lambda <- if (pclm.type == "1D") 1L else 2L
    if (length(lambda) != n.lambda) {
      stop("'lambda' must have length ", n.lambda, " in the ", pclm.type,
           " model", call. = FALSE)
    }
    # NA is a legal lambda: it marks one to be found by optimisation, so a
    # partial vector such as c(5, NA) is meaningful in the 2D model. NaN is
    # not. Mask what is to be optimised before comparing, otherwise the test
    # itself evaluates to NA and aborts with "missing value where TRUE/FALSE
    # needed".
    to_optimise <- is.na(lambda) & !is.nan(lambda)
    if (any(!to_optimise & (!is.finite(lambda) | lambda <= 0))) {
      stop("'lambda' must be NA or a positive, finite value", call. = FALSE)
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


#' Resolve the width of the last interval
#'
#' @description
#' A caller may state either the width of the last interval or the closing age
#' of the distribution. This turns the second into the first.
#' @param x Vector containing the starting values of the input intervals/bins.
#' @param nlast The width of the last interval. See \code{\link{pclm}}.
#' @param omega The closing age of the distribution, an alternative to
#'   \code{nlast}.
#' @keywords internal
resolve_nlast <- function(x, nlast, omega) {
  if (is.null(nlast) && is.null(omega)) {
    stop("supply either 'nlast' or 'omega'", call. = FALSE)
  }
  if (!is.null(nlast) && !is.null(omega)) {
    stop("supply 'nlast' or 'omega', not both", call. = FALSE)
  }
  if (is.null(nlast)) {
    if (length(omega) != 1 || !is.finite(omega)) {
      stop("'omega' must be a single finite value", call. = FALSE)
    }
    nlast <- omega - max(x)
    if (nlast <= 0) {
      stop("'omega' must be greater than max(x)", call. = FALSE)
    }
  }

  return(nlast)
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
#' @export
suggest.valid.out.step <- function(len, increment = 0.01) {
  o  <- seq(0.1, 1, by = increment)
  v  <- len/o
  tv <- trunc(v)
  o[v == tv]
}
