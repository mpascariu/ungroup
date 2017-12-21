
#' Generic Plot for pclm Class
#' 
#' @inheritParams graphics::plot.default
#' @inheritParams graphics::legend
#' @param x An object of class \code{\link{pclm}}
#' @param lwd Line width, a positive number, defaulting to 2. 
#' @param col Three colours to be used in the plot for observed values, 
#' fitted values and confidence intervals.
#' @param legend.position Legend position, or the x and y co-ordinates to be 
#' used to position the legend. 
#' @seealso \code{\link{pclm}}
#' @examples 
#' # See complete examples in pclm help page
#' @export
plot.pclm <- function(x,
                      xlab, ylab, ylim, type, 
                      lwd, col, legend, legend.position, ...) {
  # input data
  X    <- x$input$x
  Y    <- x$input$y
  fv   <- fitted(x)
  lw   <- x$ci$lower
  up   <- x$ci$upper
  Ex   <- x$input$offset
  if (length(Y) == length(Ex)) mx <- Y/Ex
  BI <- x$bin.definition$input
  BO <- x$bin.definition$output
  n1 <- BI$length
  n2 <- BO$length
  N  <- BO$n
  b1 <- BI$breaks[1,1]
  t1 <- with(BI, c(b1, breaks[2, ]))
  t2 <- with(BO, c(b1, breaks[2, ]))
  
  # Graphical parameters
  if (missing(xlab)) xlab = "(x)"
  if (missing(type)) type = "s"
  if (missing(lwd))  lwd = 2
  if (missing(legend)) legend = c("Input values", "Fitted values", "Conf. intervals")
  
  if (is.null(Ex)) { # Histogram
    if (missing(ylab)) ylab = "Counts (y)"
    if (missing(ylim)) ylim = c(0, max(Y/n1, fv/n2) * 1.3)
    if (missing(col))  col  = c("gold2", 2, 4)
    L2 = max(c(Y/n1)[1:3]) >= max(rev(c(Y/n1))[1:3])
    if (missing(legend.position)) legend.position = ifelse(L2, "topright", "topleft")
    
    barplot(height = Y/n1, width = n1, space = 0, 
            border = 'white', col = col[1],
            xlab = xlab, ylab = ylab, ylim = ylim)
    lines(x = t1 - b1, y = c(Y/n1, 0), type = type)
    lines(x = t2 - b1, y = c(lw/n2, 0), type = type, col = col[3])  
    lines(x = t2 - b1, y = c(up/n2, 0), type = type, col = col[3])  
    lines(x = t2 - b1, y = c(fv/n2, 0), type = type, col = col[2], lwd = lwd)
    legend(legend.position, legend = legend,
           bty = 'n', pch = c(15, NA, NA), 
           lty = c(NA, 1, 1), lwd = c(NA, lwd, lwd),
           col = col, text.col = "grey40", pt.cex = 2.3)
    axis(1, labels = t1, at = t1 - b1)
  } else {# mx plot
    if (missing(ylab)) ylab = "y / offset   (Log scale)"
    if (missing(ylim)) ylim = c(min(fv) * 0.50, max(fv) * 2)
    if (missing(col))  col = c(1, 2, 4)
    if (missing(legend.position)) legend.position = "topleft"
    
    plot(t2, c(fv, fv[N]), type = type, log = 'y', col = col[2],
         xlab = xlab, ylab = ylab, ylim = ylim,
         axes = F)
    if (length(Y) == length(Ex)) {
      lines(c(X, max(t1)), c(mx, max(mx)), type = "s", lwd = lwd + 1, col = col[1])
    }
    abline(v = c(X, max(t1)), col = "white", lwd = lwd)
    lines(x = t2, y = c(lw, lw[N]), type = type, col = col[3])
    lines(x = t2, y = c(up, up[N]), type = type, col = col[3])
    lines(x = t2, y = c(fv, fv[N]), type = type, col = col[2], lwd = lwd)
    legend(legend.position, legend = legend,
           bty = 'n', lty = 1, lwd = lwd,
           col = col, text.col = "grey40")
    axis(1)
    axis(2)
  }
}



#' Generic Plot for pclm2D Class
#' 
#' The generic plot for a \code{pclm2D} object is constructed using 
#' \code{\link{rgl}} package. And can be modified/improved using the \code{rgl} tools 
#' implemented in the package like: \code{\link{surface3d}}, \code{\link{axes3d}},
#' \code{\link{aspect3d}}, \code{\link{title3d}} or \code{\link{snapshot3d}}. 
#' For A complete guide to 3D visualization using \code{rgl} see 
#' \href{http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization}{this tutorial}.
#' @param x an object of class \code{\link{pclm2D}}
#' @inheritParams rgl::surface3d
#' @inheritParams rgl::rgl.material
#' @inheritParams rgl::axes3d
#' @param axes add axes to the plot. Logical. Default: \code{TRUE}. 
#' @param box draw a box aroud the plot. Logical. Default: \code{TRUE}. 
#' @seealso \code{\link{pclm2D}} \code{\link{surface3d}} \code{\link{axes3d}}
#' \code{\link{aspect3d}} \code{\link{title3d}} \code{\link{snapshot3d}}
#' @examples 
#' # See complete examples in pclm2D help page
#' @export
plot.pclm2D <- function(x, color = c(1, 2), alpha = c(1, .5), 
                        axes = TRUE, box = TRUE, 
                        xlab = "x-axis", ylab = "y-axis", zlab = "z-axis", 
                        main = "", sub = "", ...) {
  # Prepare input values
  y    <- x$input$y
  Ex   <- x$input$offset
  cond <- is.null(Ex)
  Z    <- if (cond) y else y/Ex
  Z    <- as.data.frame(Z)
  n    <- ncol(Z)
  Z$ID <- 1:nrow(Z)
  Z    <- rbind(Z, Z)
  Z    <- as.matrix(Z[sort(Z$ID), 1:n])
  bi   <- x$bin.definition$input
  if (cond) {
    len <- sort(rep(bi$length, 2))
    Z   <- sweep(Z, 1, len, FUN = "/")
  } else {
    Z <- log(Z)
  }
  loc  <- bi$location
  X    <- sort(c(loc[1,], loc[2,]))
  Y    <- n:1
  
  # Prepare fitted values
  out.step <- x$input$out.step
  Z_  <- as.matrix(fitted(x))
  len_ <- x$bin.definition$output$length
  if (cond) {
    Z_   <- sweep(Z_, 1, len_, FUN = "/")
  } else {
    Z_ <- log(Z_)
  }
  X_  <- 1:nrow(Z_) * out.step
  Y_  <- ncol(Z_):1
  
  # Plot
  if (!par('new')) open3d()
  surface3d(X, Y, Z, front = "lines", back = "lines", 
            color = color[1], alpha = alpha[1], ...)
  aspect3d(1, 1, 1)
  surface3d(X_, Y_, Z_, color = color[2], alpha = alpha[2])
  title3d(main, sub, xlab, ylab, zlab)
  if (box) box3d() 
  if (axes) axes3d()
}


