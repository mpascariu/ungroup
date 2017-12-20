
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
                        "\nLast Update: December 15, 2017")
}
