
#' onAttach
#' @param lib lib
#' @param pkg pkg
#' @name onAttach
#' @keywords internal
".onAttach" <- function(lib, pkg){
  packageStartupMessage("\nungroup    : PCLM for Estimation of Smooth Distributions from Coarsely Binned Data",
                        "\nLast Update: June 19, 2018\n")
}


#' Validate input values
#' 
#' @param X A list with input arguments provided in \code{\link{pclm}} function
#' @inheritParams pclm.fit
#' @keywords internal
pclm.input.check <- function(X, pclm.type) {
  # Validate the other arguments
  with(X, {
    if (!is.numeric(x)) {
      stop("'x' must be a vector of class numeric", call. = F)
    }
    if (any(is.na(x))) {
      stop("'x' contains NA values", call. = F)
    }
    if (any(is.na(y))) {
      stop("'y' contains NA values", call. = F)
    }
    if (any(y < 0)) {
      stop("'y' contains negative values. The counts are always positive.", call. = F)
    }
    if (length(nlast) != 1) {
      stop("length(nlast) must be 1", call. = F)
    }
    if (nlast <= 0) {
      stop("'nlast' must be greater than 0", call. = F)
    }
    if (ci.level <= 0 || ci.level >= 1) {
      stop("'ci.level' must take values in the (0, 1) interval", call. = F)
    }
    if (out.step > 1 || out.step < 0.1) {
      stop("'out.step' must be between 0.1 and 1", call. = F)
    }
    if (pclm.type == "1D") {
      if (length(x) != length(y)) {
        stop("length of 'x' and 'y' must be equal", call. = F)
      }
      if (!is.null(offset) & length(offset) != length(y)) {
        stop(paste("'offset' must have the same length as 'y'"), call. = F)
      }
    }
    if (pclm.type == "2D") {
      if (!(is.data.frame(y) || is.matrix(y))) {
        stop("'y' must be a data.frame or a matrix", call. = F)
      }
      if (length(x) != nrow(y)) {
        stop("length(x) must be equal to nrow(y)", call. = F)
      }
      if (!is.null(offset) & !all(dim(offset) == dim(y))) {
        stop(paste("'offset' must have the same dimension as 'y'"), call. = F)
      }
    }
    
  })
  
  # Validate input in pclm.control
  with(X$control, {
    # if (!(length(lambda) == 1)) {
    #   stop("'lambda' must be of length 1", call. = F)
    # }
    if (!is.na(lambda) && lambda < 0) {
      stop("'lambda' must be a positive scalar", call. = F)
    }
    if (!is.na(kr)) {
      if (kr <= 0 || frac(kr) != 0) stop("'kr' must be a positive integer", call. = F)
    }
    if (!is.na(deg)) {
      if (deg < 2 || frac(deg) != 0) 
        stop("'deg' must be a positive integer greater or equal than 2", call. = F)
    }
    if (!(opt.method[1] %in% c('BIC','AIC'))) {
      stop("'AIC' or 'BIC' should be used as opt.method", call. = F)
    }
    if (max.iter < 10) {
      stop("'max.iter' should be at least 10 for a decent run", call. = F)
    }
    if (tol <= 0) {
      stop("'tol' must be greater than 0", call. = F)
    }
  })
}

