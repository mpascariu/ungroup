
#' onAttach
#' @param lib lib
#' @param pkg pkg
#' @name onAttach
#' @keywords internal
".onAttach" <- function(lib, pkg){
  packageStartupMessage("\nungroup    : PCLM for Estimation of Smooth Distributions from Coarsely Binned Data",
                        "\nLast Update: September 1, 2018\n")
}
