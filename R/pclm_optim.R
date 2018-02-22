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
    Par <- c(lambda, kr, deg)
    # Find lambda (continuos)
    if (any(is.na(lambda))) { 
      if (show) {setpb(pb, 40); cat("   Optimizing lambda  ")}
      # The following two 'if's will help me define constraints in the 
      # optimization algorithm. If one of the lambdas is specified, the 
      # algorithm will search only for the missing one.
      L1_lw = L1_up = L1 <- log(lambda[1]) 
      L2_lw = L2_up = L2 <- log(lambda[2]) 
      if (is.na(lambda[1])) {
        L1_lw <- log(int.lambda)[1]
        L1_up <- log(int.lambda)[2]
        L1    <- log(mean(int.lambda))
      }
      if (is.na(lambda[2])) {
        L2_lw <- log(int.lambda)[1]
        L2_up <- log(int.lambda)[2]
        L2    <- log(mean(int.lambda))
      } 
      opt <- nlminb(start = c(L1, L2), objective = objective_fn,
                    lower = c(L1_lw, L2_lw), upper = c(L1_up, L2_up), 
                    x = x, y = y, nlast = nlast, offset = offset, 
                    out.step = out.step, kr = kr, deg = deg, diff = diff, 
                    max.iter = max.iter, pclm.type = "2D", opt.method = opt.method)
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
    # Find lambda (continuos)
    if (is.na(lambda)) { 
      if (show) {setpb(pb, 40); cat("   Optimizing lambda  ")}
      opt <- optimise(f = objective_fn, interval = log(int.lambda), 
                      x = x, y = y, nlast = nlast, offset = offset, 
                      out.step = out.step, kr = kr, deg = deg, diff = diff, 
                      max.iter = max.iter, pclm.type = "1D", opt.method = opt.method)
      Par[1] <- round(exp(opt$minimum), 6)
    }
    if (Par[1] == int.lambda[2]) {
      warning(paste0("'lambda' has reached the upper limit of ", int.lambda[2],
                     ". Maybe it is a good idea to extend interval. ",
                     "See 'int.lambda' argument in 'pclm1D.control'."), call. = F)
    } 
    return(Par)
  })
}


#' Objective function
#' @param L lambda
#' @param ... all the other pclm.fit arguments
#' @inheritParams pclm.control
#' @keywords internal 
objective_fn <- function(L, opt.method, tol = 1e-05, ...) {
  L   <- round(exp(L), 6)
  out <- pclm.fit(lambda = L, show = F, tol = tol, ...)[[paste(opt.method)]]
  # print(round(c(L = L, AIC = out), 3))
  return(out)
}

