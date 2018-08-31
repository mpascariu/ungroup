
# Fri Aug 31 11:39:54 2018 --------- Marius D. Pascariu ---
remove(list = ls())
library(ungroup)
library(hexSticker)


# ----------------------------------------------
plot_pclm <- function(x, lwd, col, ...) {
  # input data
  Y  <- x$input$y
  fv <- fitted(x)
  BI <- x$bin.definition$input
  BO <- x$bin.definition$output
  n1 <- BI$length
  n2 <- BO$length
  b1 <- BI$breaks[1,1]
  t1 <- with(BI, c(b1, breaks[2, ]))
  t2 <- with(BO, c(b1, breaks[2, ]))
  
  # Graphical parameters
  if (missing(lwd))  lwd = 1
  
  ylim = c(0, max(Y/n1, fv/n2) * 1.3)
  if (missing(col))  col  = c("grey", 2, 4)
    
  f <- function(x) c(x, x[length(x)])
  barplot(height = Y/n1, width = n1, space = 0, 
          border = 'white', col = col[1],
          ylim = ylim, axes = F)
  lines(x = t1 - b1, y = f(Y/n1), type = "s")
  lines(x = t2 - b1, y = f(fv/n2), type = "l", col = col[2], lwd = lwd)
  axis(1, at = t1 - b1, labels = F, tick = T, lty = 1)
}
# ----------------------------------------------
# Data  
x <- c(0, 2, seq(5, 50, by = 5), 56)
y <- c(2000, 293, 361, 600, 998, 
       1572, 2529, 4637, 6161, 7369, 10481, 15293, 39016)
nlast <- 26 # the size of the last interval

M1 <- pclm(x, y, nlast, control = list(kr = 2))
plot_pclm(M1)

fn <- paste0(getwd(), "/inst/figures/ungroup_logo.png")
sticker(expression(plot_pclm(M1)), s_width = 2, s_height = 1.5,
        package = "ungroup", p_color = 1,
        h_fill = "white", h_color = 1,
        p_size = 8, s_x = 0.7, s_y = 0.7,
        filename = fn)





