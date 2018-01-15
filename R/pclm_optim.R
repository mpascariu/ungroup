
#' Optimize Smoothing Parameters
#' This function optimize searches of \code{lambda, kr} and \code{deg}. 
#' See \code{\link{pclm.control}} to see what is their meaning. 
#' The optimization process works in steps. Simultaneous optimization was 
#' tested and found inefficient. Methods tested: "Nelder-Mead" (optim) 
#' and "PORT routines" (nlminb).
#' @inheritParams pclm
#' @keywords internal
optimize_par2D <- function(x, y, nlast, offset, show, out.step, control) {
  if (show) pb = startpb(0, 100)
  with(control, {
    # Objective functions
    Par <- c(lambda, kr, deg)
    fn <- function(L) {
      L = round(exp(L), 6)
      out = pclm.fit(x, y, nlast, offset, out.step, show = F, 
                     lambda = L, kr, deg, diff, max.iter, tol, 
                     pclm.type = "2D")[[paste(opt.method)]]
      # print(round(c(L = L, AIC = out), 3))
      return(out)
    }

    # Find lambda (continuos)
    if (any(is.na(lambda))) { 
      if (show) {setpb(pb, 40); cat("   Optimizing lambda  ")}
      ipar = log(int.lambda + .000001)
      opt <- nlminb(start = log(c(50, 50)), objective = fn,
                    lower = ipar[1], upper = ipar[2])
      Par[1:2] <- round(exp(opt$par), 6)
    }
    if (Par[1] == int.lambda[2]) {
      warning(paste0("'lambda' has reached the upper limit of ", int.lambda[2],
                    ". Maybe it is a good idea to extend interval. ",
                    "See 'int.lambda' argument in 'pclm2D.control'."), call. = F)
    } 
    return(Par)
  })
}

#' Optimize Smoothing Parameters
#' @inheritParams pclm
#' @keywords internal 
optimize_par1D <- function(x, y, nlast, offset, show, out.step, control) {
  if (show) pb = startpb(0, 100)
  with(control, {
    Par <- c(lambda, kr, deg)
    fn <- function(L) {
      L = exp(round(L, 2))
      pclm.fit(x, y, nlast, offset, out.step, show = F, 
               lambda = L, kr, deg, diff, max.iter, tol, 
               pclm.type = "1D")[[paste(opt.method)]]
      # print(c(L = L, opt = opt))
    }
 
    # Find lambda (continuos)
    if (is.na(lambda)) { 
      if (show) {setpb(pb, 40); cat("   Optimizing lambda  ")}
      opt    <- optimise(f = fn, interval = log(int.lambda + .00001), tol = 1e-05)
      Par[1] <- round(exp(opt$minimum), 2)
    }
    if (Par[1] == int.lambda[2]) {
      warning(paste0("'lambda' has reached the upper limit of ", int.lambda[2],
                     ". Maybe it is a good idea to extend interval. ",
                     "See 'int.lambda' argument in 'pclm1D.control'."), call. = F)
    } 
    return(Par)
  })
}








