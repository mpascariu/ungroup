# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Jun 23 16:38:17 2021
# --------------------------------------------------- #

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
#' @param type 1-character string giving the type of plot desired. 
#' The following values are possible, for details, see plot: "p" for points, 
#' "l" for lines, "b" for both points and lines, "c" for empty points joined 
#' by lines, "o" for overplotted points and lines, "s" and "S" for stair 
#' steps and "h" for histogram-like vertical lines. Finally, "n" does not 
#' produce any points or lines.
#' @param ... other graphical parameters (see \link{par} for more details).
#' @seealso \code{\link{pclm}}
#' @examples 
#' # See complete examples in pclm help page
#' @export
plot.pclm <- function(x,
                      xlab, ylab, ylim, type, 
                      lwd, col, legend, legend.position, ...) {
  # input data
  X  <- x$input$x
  Y  <- x$input$y
  Ex <- x$input$offset
  if (length(Y) == length(Ex)) mx <- Y/Ex
  
  fv <- fitted(x)
  lw <- x$ci$lower
  up <- x$ci$upper
  BI <- x$bin.definition$input
  BO <- x$bin.definition$output
  n1 <- BI$length
  n2 <- BO$length
  N  <- BO$n
  b1 <- BI$breaks[1, 1]
  t1 <- with(BI, c(b1, breaks[2, ]))
  t2 <- with(BO, c(b1, breaks[2, ]))
  
  # Graphical parameters
  if (missing(xlab)) xlab <- "(x)"
  if (missing(type)) type <- "l"
  if (missing(lwd))  lwd <- 2
  if (missing(legend)) {
    legend <- c("Input values", "Fitted values", "Conf. intervals")
  }
  
  if (is.null(Ex)) { # Histogram
    if (missing(ylab)) ylab <- "Counts (y)"
    if (missing(ylim)) ylim <- c(0, max(Y/n1, fv/n2) * 1.3)
    if (missing(col))  col  <- c("gold2", 2, 4)
    L2 <- max(c(Y/n1)[1:3]) >= max(rev(c(Y/n1))[1:3])
    if (missing(legend.position)) {
      legend.position <- ifelse(L2, "topright", "topleft")
    }
    
    f <- function(x) c(x, x[length(x)])
    barplot(height = Y/n1, width = n1, space = 0, 
            border = 'white', col = col[1],
            xlab = xlab, ylab = ylab, ylim = ylim, ...)
    lines(x = t1 - b1, y = f(Y/n1), type = "s")
    lines(x = t2 - b1, y = f(lw/n2), type = type, col = col[3])  
    lines(x = t2 - b1, y = f(up/n2), type = type, col = col[3])  
    lines(x = t2 - b1, y = f(fv/n2), type = type, col = col[2], lwd = lwd)
    legend(legend.position, legend = legend,
           bty = 'n', pch = c(15, NA, NA), 
           lty = c(NA, 1, 1), lwd = c(NA, lwd, lwd),
           col = col, text.col = "grey40", pt.cex = 2.3)
    axis(1, labels = t1, at = t1 - b1)
    
  } else {# mx plot
    if (missing(ylab)) ylab <- "y / offset   (Log scale)"
    if (missing(ylim)) ylim <- c(min(fv) * 0.50, max(fv) * 2)
    if (missing(col))  col <- c(1, 2, 4)
    if (missing(legend.position)) legend.position <- "topleft"
    
    plot(t2, c(fv, fv[N]), type = type, log = 'y', col = col[2],
         xlab = xlab, ylab = ylab, ylim = ylim, axes = FALSE, ...)
    if (length(Y) == length(Ex)) {
      lines(x = c(X, max(t1)), y = c(mx, max(mx)), 
            type = "s", lwd = lwd + 1, col = col[1])
    }
    # abline(v = c(X, max(t1)), col = "white", lwd = lwd)
    lines(x = t2, y = c(lw, lw[N]), type = type, col = col[3])
    lines(x = t2, y = c(up, up[N]), type = type, col = col[3])
    lines(x = t2, y = c(fv, fv[N]), type = type, col = col[2], lwd = lwd)
    legend(legend.position, legend = legend,
           bty = 'n', lty = 1, lwd = 3,
           col = col, text.col = "grey40")
    axis(1)
    axis(2)
  }
}



#' Generic Plot for pclm2D Class
#' 
#' The generic plot for a \code{pclm2D} object is constructed using 
#' \code{\link[graphics]{persp}} method. 
#' 
#' @param x an object of class \code{\link{pclm2D}}.
#' @param nbcol dimension of the color palette. Number of colors. Default: 25.
#' @param type chart type. Defines which data are plotted, \code{"fitted"} 
#' values or \code{"observed"} input data. Default: \code{"fitted"}.
#' @inheritParams graphics::persp
#' @inheritParams grDevices::colorRampPalette
#' @param ... any other argument to be passed to 
#' \code{\link[graphics]{persp}}.
#' @seealso \code{\link{pclm2D}}
#' @examples 
#' # See complete examples in pclm2D help page
#' @export
plot.pclm2D <- function(x, 
                        type = c("fitted", "observed"),
                        colors = c("#b6e3db", "#e5d9c2", "#b5ba61", "#725428"),
                        nbcol = 25,
                        xlab = "x",
                        ylab = "y",
                        zlab = "values",
                        phi = 30,
                        theta = 210,
                        border = "grey50",
                        ticktype = "simple",
                        ...) {
  
  type   <- match.arg(type)
  object <- x
  Ex     <- x$input$offset
  ok     <- TRUE
  vsn    <- 0.000000001 # very small number
  
  if (type == "fitted") {
    out.step <- x$input$out.step
    len <- x$bin.definition$output$length
    Z   <- as.matrix(fitted(x))
    # If data if offset take logs
    Z   <- if (is.null(Ex)) sweep(Z, 1, len, FUN = "/") else log(Z)
    X   <- seq_len(nrow(Z)) * out.step
    Y   <- seq_len(ncol(Z))
  } 
  
  if (type == "observed") {
    
    len  <- sort(rep(x$bin.definition$input$length, 2))
    loc  <- x$bin.definition$input$location
    y    <- x$input$y
    n    <- ncol(y)
    Z    <- if (is.null(Ex)) y else y/Ex
    Z    <- as.data.frame(Z)
    Z$ID <- seq_len(nrow(Z))
    
    # We are doing all this to in order to get a stepwise type of surface
    Z    <- rbind(Z, Z + vsn)
    Z    <- as.matrix(Z[sort(Z$ID), 1:n])
    Z    <- if (is.null(Ex)) sweep(Z, 1, len, FUN = "/") else log(Z)
    X    <- sort(c(loc[1,], loc[2,] + vsn))
    Y    <- 1:n
  } 
  
  # Check point
  if (!is.null(Ex)) {
    ok <- all(dim(Y) != dim(Ex))
    
    if (!ok) {
      warning("Observed surface cannot be plotted because `y` and `offset`", 
              "have different dimensions.")
    }
  }
  
  # if all ok plot!
  if(ok) {
    
    # Figure out colors.
    # Compute the z-value at the facet centres
    ncz <- ncol(Z)
    nrz <- nrow(Z)
    zfacet   <- Z[-1, -1] + Z[-1, -ncz] + Z[-nrz, -1] + Z[-nrz, -ncz]
    # Recode facet z-values into color indices
    colpal   <- colorRampPalette(colors)(nbcol)
    facetcol <- cut(zfacet, nbcol)
    
    # Perspective Plot
    persp(X, Y, Z, 
          col = colpal[facetcol], 
          phi = phi, 
          theta = theta,
          xlab = xlab,
          ylab = ylab,
          zlab = zlab,
          border = border,
          ticktype = ticktype,
          ...)
    
  } else {
    return(NULL)
    
  }
}


