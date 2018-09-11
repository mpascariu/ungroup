# Wed Sep  5 22:31:22 2018 --------- Marius D. Pascariu ---
remove(list = ls())
library(ungroup)
library(rgl)

# Input data  
x <- c(0, 1, seq(5, 85, by = 5))
y <- c(294, 66, 32, 44, 170, 284, 287, 293, 361, 600, 998,  
       1572, 2529, 4637, 6161, 7369, 10481, 15293, 39016)
offset <- c(114, 440, 509, 492, 628, 618, 576, 580, 634, 657, 
            631, 584, 573, 619, 530, 384, 303, 245, 249) * 1000
nlast <- 26
# Fit PCLM
M1 <- pclm(x, y, nlast)
M2 <- pclm(x, y, nlast, offset)

# Plots
h = 7

# ps = 30
# tiff("pclm1D.tiff", height = h, width = 2.5*h, type = "cairo", pointsize = ps,
#      compression = "none")
pdf("figures/pclm1D.pdf", height = h, width = 2.5 * h)
print({
par(mfrow = c(1, 2))
plot(M1, lwd = 2,
     xlab = "Age, [x]", ylab = "Counts, [y]", 
     main = "Ungrouping of the age-at-death distribution")
plot(M2, type = "s", lwd = 2,
     xlab = "Age, [x]", ylab = "Age-specific central death-rate, m[x]",
     main = "Estimating age-specific death rates from binned data")
dev.off()})
par(mfrow = c(1, 1))


# ----------------------------------------------
# Input data
Dx <- ungroup.data$Dx
Ex <- ungroup.data$Ex

# Aggregate data to ungroup it in the examples below
x2      <- c(0, 1, seq(5, 85, by = 5))
nlast2  <- 26
n      <- c(diff(x), nlast)
group  <- rep(x, n)
y2      <- aggregate(Dx, by = list(group), FUN = "sum")[, -1]
offset2 <- aggregate(Ex, by = list(group), FUN = "sum")[, -1]

# Fit model and ungroup data using PCLM-2D
P1 <- pclm2D(x2, y2, nlast2)
P2 <- pclm2D(x2, y2, nlast2, offset2)


plot(P1, xlab = "Age, [x]", ylab = "Year, [y]", zlab = "Counts, [z]")
bgplot3d({
  plot.new()
  title(main = "Ungrouping a sequence of age-at-death distributions", 
        line = 1, cex.main = 1.7)
  
})
rgl.viewpoint(theta = 0, phi = -70, fov = 60, zoom = 0.8)
rgl.snapshot("figures/pclm2D_dx.png", fmt = "png", top = TRUE)

plot(P2, color = c(1, 3),
     xlab = "Age, [x]", ylab = "Year, [y]", 
     zlab = "log m[x]")  
bgplot3d({
  plot.new()
  title(main = "Ungrouping of a mortality surface", 
        line = 1, , cex.main = 1.7)
})
rgl.viewpoint(theta = 0, phi = -70, fov = 50, zoom = 0.8)
rgl.snapshot("figures/pclm2D_mx.png", fmt = "png", top = TRUE)



library(magick)
p1 = image_read("figures/pclm2D_dx.png")
p2 = image_read("figures/pclm2D_mx.png")
img <- c(p1, p2)
image_write(image_append(img), path = "figures/pclm2D.pdf", format = "pdf")












