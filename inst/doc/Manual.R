## ----setup, include=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(collapse = TRUE)

## ------------------------------------------------------------------------
# Load the package
library(ungroup)

## ------------------------------------------------------------------------
# Input data  
# x: Age groups
x <- c(0, 1, seq(5, 85, by = 5))
x
# y: Death counts in the age group
y <- c(294, 66, 32, 44, 170, 284, 287, 293, 361, 600, 998,  
       1572, 2529, 4637, 6161, 7369, 10481, 15293, 39016)
# offset: Population exposed to risk in the age group
offset <- c(114, 440, 509, 492, 628, 618, 576, 580, 634, 657, 
            631, 584, 573, 619, 530, 384, 303, 245, 249) * 1000
# nlast: the size of the last age interval (usually open)
nlast <- 26
# This results in the last group being [85, 110).

## ---- message=FALSE, results='hide'--------------------------------------
M1 <- pclm(x, y, nlast)

## ------------------------------------------------------------------------
ls(M1)

## ------------------------------------------------------------------------
summary(M1)

## ---- fig.align='center', fig.asp=0.8, out.width = '60%'-----------------
plot(M1)

# Print first 6 fitted values
fitted(M1)[1:6]

## ---- message=FALSE, results='hide', fig.align='center', fig.asp=0.8, out.width = '60%'----
M2 <- pclm(x, y, nlast, out.step = 0.5)
plot(M2)

## ------------------------------------------------------------------------
# Print first 6 fitted values
fitted(M2)[1:6]
# Number of fitted values
length(fitted(M2))

## ---- eval=FALSE---------------------------------------------------------
#  # Optimise smoothing parameter: lambda, kr and deg
#  M3 <- pclm(x, y, nlast,
#             control = list(lambda = NA, opt.method = "AIC"))

## ---- message=FALSE, results='hide'--------------------------------------
M5 <- pclm(x, y, nlast, offset)

## ---- fig.align='center', fig.asp=0.8, out.width = '60%'-----------------
plot(M5, type = "s")

