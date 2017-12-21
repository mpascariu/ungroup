
#' onAttach
#' @param lib lib
#' @param pkg pkg
#' @name onAttach
#' @keywords internal
".onAttach" <- function(lib, pkg){
  packageStartupMessage("\nR Package  : pclm",
                        "\nName       : Penalized Composite Link Model for Efficient Estimation",
                        "\n             of Smooth Distributions from Coarsely Binned Data",
                        "\nAuthors    : M.D. Pascariu, S. Rizzi, and M.J. Danko",
                        "\nLast Update: December 21, 2017")
}


#' Validate input values
#' 
#' @param X A list with input arguments provided in \code{\link{pclm}} function
#' @inheritParams pclm.fit
#' @keywords internal
pclm.input.check <- function(X, pclm.type) {
  with(X, {
    if (!is.numeric(x)) stop("'x' must be a vector of class numeric", call. = F)
    
    if (pclm.type == "1D" ) {
      if (length(x) != length(y)) {
        stop("lenght of 'x' and 'y' is not the same!", call. = F)
      }
      L2 <- length(offset) == length(y)
      L3 <- length(offset) == (length(x[1]:max((x + nlast))) - 1)
    } else {
      if (length(x) != nrow(y)) {
        stop("length(x) != nrow(y)", call. = F)
      }
      L2 <- all(dim(offset) == dim(y))
      L3 <- nrow(offset) == (length(x[1]:max((x + nlast))) - 1)
    }
    
    if (!is.null(offset)) {
      if (L2 & L3) stop(paste("\n'offset' should have the same dimension as 'y'"), call. = F)
    }
  })
}
