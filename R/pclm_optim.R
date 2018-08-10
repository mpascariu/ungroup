
#' Objective function
#' @param L lambda.hat
#' @inheritParams optimize_par
#' @keywords internal
#' 
ofun <- function(L, I, type) {
  L <- round(exp(L), 6)
  with(I$control, {
    M <- pclm.fit(I$x, I$y, I$nlast, I$offset, I$out.step, verbose = FALSE, 
                  lambda = L, kr, deg, diff, max.iter, tol, type)
    fn <- paste0(opt.method, ".pclm")
    aic_bic <- get(fn)
    out <- aic_bic(M)
    # print(round(c(L = L, AIC = out), 3))
    return(out)
  })
}


#' Optimize Smoothing Parameters
#' This function optimize searches of \code{lambda, kr} and \code{deg}. 
#' See \code{\link{control.pclm}} to see what is their meaning. 
#' The optimization process works in steps. Simultaneous optimization was 
#' tested and found inefficient.
#' @param I Input object from pclm function
#' @inheritParams pclm.fit
#' @keywords internal
optimize_par <- function(I, type) {
  
  with(I$control, {
    if (I$verbose) pb = startpb(0, 100)
    
    # Find lambda (continuos)
    if (any(is.na(lambda))) { 
      if (I$verbose) {setpb(pb, 40); cat("   Optimizing lambda  ")}
      
      if (type == "1D") {
        opt <- optimise(f = ofun, interval = log(int.lambda), I = I, 
                        type = type, tol = 1e-05)
        lambda.hat <- round(exp(opt$minimum), 6)
        
      } else {
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
        
        opt <- nlminb(start = c(L1, L2), objective = ofun, I = I, type = type,
                      lower = c(L1_lw, L2_lw), 
                      upper = c(L1_up, L2_up))
        lambda.hat <- round(exp(opt$par), 6)
      }
    }
    
    if (lambda.hat[1] == int.lambda[2]) {
      warning(paste0("'lambda' has reached the upper limit of ", int.lambda[2],
                     ". Maybe it is a good idea to extend interval. ",
                     "See 'int.lambda' argument in 'pclm2D.control'."), call. = F)
    } 
    return(lambda.hat)
  })
}



