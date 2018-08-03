
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
      
      opt <- nlminb(start = c(L1, L2), objective = fn, 
                    lower = c(L1_lw, L2_lw), 
                    upper = c(L1_up, L2_up))
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
      L = exp(round(L, 6))
      pclm.fit(x, y, nlast, offset, out.step, show = F, 
               lambda = L, kr, deg, diff, max.iter, tol, 
               pclm.type = "1D")[[paste(opt.method)]]
      # print(c(L = L, opt = opt))
    }
    
    # Find lambda (continuos)
    if (is.na(lambda)) { 
      if (show) {setpb(pb, 40); cat("   Optimizing lambda  ")}
      opt    <- optimise(f = fn, interval = log(int.lambda), tol = 1e-05)
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
