
#' Optimize Smoothing Parameters
#' This function optimize searches of \code{lambda, kr} and \code{deg}. 
#' See \code{\link{pclm.control}} to see what is their meaning. 
#' The optimization process works in steps. Simultaneous optimization was 
#' tested and found inefficient. Methods tested: "Nelder-Mead" (optim) 
#' and "PORT routines" (nlminb).
#' @inheritParams pclm
#' @keywords internal
optimize_par <- function(x, y, nlast, offset, show, 
                          out.step, control, pclm.type) {
  with(control, {
    # Objective functions
    Par <- c(lambda, kr, deg)
    FN <- function(L, K, D) {
      L <- round(L, 2)
      # print(c(L = L, K = K, D = D))
      pclm.fit(x, y, nlast, offset, out.step, show = F, 
               lambda = L, kr = K, deg = D, 
               diff, max.iter, tol, pclm.type)[[paste(opt.method)]]
    }
    F_D <- function(x) FN(L = ifelse(is.na(Par[1]), int.lambda[1], Par[1]), 
                          K = ifelse(is.na(Par[2]), int.kr[1], Par[2]), 
                          D = x)
    F_K <- function(x) FN(L = ifelse(is.na(Par[1]), int.lambda[1], Par[1]), 
                          K = x, 
                          D = Par[3])
    F_L <- function(x) FN(L = exp(x), K = Par[2], D = Par[3])
    if (show) pb = startpb(0, 100)
    # Step 1. Find deg over integer values
    if (is.na(deg)) { 
      if (show) {setpb(pb, 20); cat("   Optimizing deg    ")}
      res    <- apply(matrix(int.deg), 1, F_D)
      Par[3] <- int.deg[res == min(res)][1]
    }
    # Step 2. Find kr over integer values
    if (is.na(kr)) { 
      if (show) {setpb(pb, 30); cat("   Optimizing kr     ")}
      res    <- apply(matrix(int.kr), 1, F_K)
      Par[2] <- int.kr[res == min(res)][1]
    }
    # Step 3. Find lambda (continuos)
    if (is.na(lambda)) { 
      if (show) {setpb(pb, 40); cat("   Optimizing lambda  ")}
      tol    <- if (pclm.type == "1D") 1e-05 else 1
      opt    <- optimise(f = F_L, interval = log(int.lambda + .01), tol = tol)
      Par[1] <- round(exp(opt$minimum), 2)
    }
    if (Par[1] == int.lambda[2]) {
      warning(paste0("'lambda' has reached the upper limit of ", int.lambda[2],
                    ". Maybe it is a good idea to extend interval. ",
                    "See 'int.lambda' argument in 'pclm.control'."), call. = F)
    } 
    return(Par)
  })
}

#' Optimize Smoothing Parameters - 2nd method
#' @inheritParams pclm
#' @keywords internal 
optimize_par1D <- function(x, y, nlast, offset, show, 
                           out.step, control, pclm.type) {
  if (show) pb = startpb(0, 100)
  with(control, {
    Par <- c(lambda, kr, deg)
    FN <- function(L, K, D) {
      L   <- round(L, 2)
      opt <- pclm.fit(x, y, nlast, offset, out.step, show = F, 
               lambda = L, kr = K, deg = D, 
               diff, max.iter, tol, pclm.type)[[paste(opt.method)]]
      # print(c(L = L, K = K, D = D, opt = opt))
      return(opt)
    }
    
    # Step 1. Find kr and deg over integer values
    if (is.na(kr) || is.na(deg)) { 
      if (show) {setpb(pb, 20); cat("   Optimizing kr and deg  ")}
      if ( is.na(kr) &  is.na(deg)) grd <- expand.grid(int.kr, int.deg)
      if ( is.na(kr) & !is.na(deg)) grd <- expand.grid(int.kr, deg)
      if (!is.na(kr) &  is.na(deg)) grd <- expand.grid(kr, int.deg)
      fn <- function(X) FN(L = ifelse(is.na(lambda), 1, lambda), X[1], X[2])
      res      <- apply(grd, 1, fn)
      Par[2:3] <- as.numeric(grd[res == min(res), ][1, ])
    }
    # Step 2. Find lambda (continuos)
    if (is.na(lambda)) { 
      if (show) {setpb(pb, 40); cat("   Optimizing lambda  ")}
      tol    <- if (pclm.type == "1D") 1e-05 else 1
      fn     <- function(X) FN(L = exp(X), K = Par[2], D = Par[3])
      opt    <- optimise(f = fn, interval = log(int.lambda + .01), tol = tol)
      Par[1] <- round(exp(opt$minimum), 2)
    }
    if (Par[1] == int.lambda[2]) {
      warning(paste0("'lambda' has reached the upper limit of ", int.lambda[2],
                     ". Maybe it is a good idea to extend interval. ",
                     "See 'int.lambda' argument in 'pclm.control'."), call. = F)
    } 
    return(Par)
  })
}








