# Build the figure embedded in README.md.
# Uses the Swedish data shipped with the package, so the chart is reproducible
# and shows real data rather than a toy vector.
#
# Marius D. Pascariu

rm(list = ls())

library(ungroup)

out_file <- "man/figures/README-ungrouping.png"

# Coarse bins: single years under 5, then five-year groups, open at 85+.
x     <- c(0, 1, seq(5, 85, by = 5))
nlast <- 26
grp   <- rep(x, c(diff(x), nlast))

Dx <- as.numeric(ungroup.data$Dx[, 1])   # Sweden, 1980
Ex <- as.numeric(ungroup.data$Ex[, 1])

y  <- as.numeric(tapply(Dx, grp, sum))
Nx <- as.numeric(tapply(Ex, grp, sum))

M  <- pclm(x = x, y = y, nlast = nlast)

png(out_file, width = 1800, height = 800, res = 150)
op <- par(mfrow = c(1, 2), mar = c(4.2, 4.4, 2.6, 0.8), las = 1)

# ---------------------------------------------------------------- panel 1
# The observed coarse bins against the smooth estimate.
fv <- fitted(M)
bi <- M$bin.definition$input
bo <- M$bin.definition$output
n1 <- bi$length          # width of each input bin
n2 <- bo$length          # width of each output bin
b1 <- bi$breaks[1, 1]
t1 <- c(b1, bi$breaks[2, ])
t2 <- c(b1, bo$breaks[2, ])
step <- function(v) c(v, v[length(v)])

ylim <- c(0, max(c(y / n1, fv / n2)) * 1.28)

barplot(y / n1, width = n1, space = 0, border = "white", col = "grey85",
        xlab = "Age, x", ylab = "Deaths per year of age",
        ylim = ylim, axes = FALSE, names.arg = FALSE,
        main = "Ungrouping the age-at-death distribution")
axis(1, at = t1 - b1, labels = t1)
axis(2)
box()

lines(t2 - b1, step(fv / n2), col = "firebrick", lwd = 2.2)

legend("topright", bty = "n", inset = 0.02,
       legend = c("Coarse input", "PCLM estimate"),
       fill = c("grey85", NA), border = c("white", NA),
       lty = c(NA, 1), lwd = c(NA, 2.2), col = c(NA, "firebrick"),
       text.col = "grey30")

# ---------------------------------------------------------------- panel 2
# The age-specific death rates the same fit implies.
Mr <- pclm(x = x, y = y, nlast = nlast, offset = Nx)
plot(Mr, type = "s",
     xlab = "Age, x", ylab = expression(m(x) ~~ "(log scale)"),
     main = "Implied age-specific death rates",
     bty = "n", lwd = 2.2)

par(op)
dev.off()

message("wrote ", out_file)
