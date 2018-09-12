
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
    if (length(nlast) != 1) {
      stop("length(nlast) must be 1", call. = FALSE)
    }
    if (nlast <= 0) {
      stop("'nlast' must be greater than 0", call. = FALSE)
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
    if (!is.na(lambda) && lambda < 0) {
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

