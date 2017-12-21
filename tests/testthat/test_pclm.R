rm(list = ls())
library(testthat)
library(pclm)

# ----------------------------------------------
# Tests 
test_pclm_1D <- function(M) {
  fv    <- fitted(M)
  lower <- M$ci[[1]]
  upper <- M$ci[[2]]
  test_that("Test pclm", {
    expect_s3_class(M, "pclm")
    expect_output(print(M))
    expect_output(print(summary(M)))
    expect_false(is.null(plot(M)))
    expect_true(all(fv >= 0))
    expect_identical(length(fv), length(lower))
    expect_identical(length(upper), length(lower))
    if (is.null(M$input$offset)) {
      expect_identical(round(sum(fv), 1), round(sum(M$input$y), 1))
    }
  })
}

test_pclm_2D <- function(M) {
  fv    <- fitted(M)
  lower <- M$ci[[1]]
  upper <- M$ci[[2]]
  test_that("PCLM-2D", {
    expect_s3_class(M, "pclm2D")
    expect_output(print(M))
    expect_output(print(summary(M)))
    expect_true(all(fv >= 0))
    expect_identical(dim(fv), dim(lower))
    expect_identical(dim(upper), dim(lower))
    if (is.null(M$input$offset)) {
      expect_identical(round(sum(fv), 1), round(sum(M$input$y), 1))
    }
  })
}

# ----------------------------------------------
# PCLM-1D
x <- c(0, 1, seq(5, 85, by = 5))
y <- c(294, 66, 32, 44, 170, 284, 287, 293, 361, 600, 998,
       1572, 2529, 4637, 6161, 7369, 10481, 15293, 39016)
offset <- c(114, 440, 509, 492, 628, 618, 576, 580, 634, 657,
            631, 584, 573, 619, 530, 384, 303, 245, 249) * 1000
nlast <- 26 # the size of the last interval

M1 <- pclm(x, y, nlast)
M2 <- pclm(x, y, nlast, out.step = 0.5)
M3 <- pclm(x, y, nlast, out.step = 0.5,
           control = list(lambda = NA, kr = NA, deg = NA))
M4 <- pclm(x, y, nlast, offset, out.step = 0.4,
           control = list(lambda = 1, kr = 8, deg = 3))
M5 <- pclm(x, y, nlast, offset, out.step = 0.4,
           control = list(lambda = NA, kr = NA, deg = NA))

summary(M1)
summary(M2)
summary(M3)
summary(M4)
summary(M5)

for (i in 1:5) test_pclm_1D(get(paste0("M", i)))

# ----------------------------------------------
# PCLM-2D
Dx     <- pclm.data$Dx
Ex     <- pclm.data$Ex
n      <- c(diff(x), nlast)
Ex$gr  <- Dx$gr <- rep(x, n)
y2      <- aggregate(Dx[, 1:35], by = list(Dx$gr), FUN = "sum")[, -1]
offset2 <- aggregate(Ex[, 1:35], by = list(Ex$gr), FUN = "sum")[, -1]

P1 <- pclm2D(x, y2, nlast)
P2 <- pclm2D(x, y2, nlast, offset2)

summary(P1)             
summary(P2)             

for (i in 1:2) test_pclm_2D(get(paste0("P", i)))


# ----------------------------------------------
# Test error messages

expect_error(pclm(x = c(NA, x), y, nlast))
expect_error(pclm(x = c(1, x), y, nlast))
expect_error(pclm(x = c(1, x), c(y, NA), nlast))
expect_error(pclm(x = c(x, 90), c(y, -10), nlast))
expect_error(pclm(x, y, nlast = -10))
expect_error(pclm(x, y, nlast, c(offset, 1)))
expect_error(pclm(x, y, nlast, ci.level = -0.05))
expect_error(pclm(x, y, nlast, out.step = -1))
expect_error(pclm(x, y, nlast, control = c(a = 1))) #****
expect_error(pclm(x, y, nlast, control = list(lambda = c(0, 1))))
expect_error(pclm(x, y, nlast, control = list(lambda = -1)))
expect_error(pclm(x, y, nlast, control = list(kr = -1.5)))
expect_error(pclm(x, y, nlast, control = list(deg = -1.5)))
expect_error(pclm(x, y, nlast, control = list(opt.method = "AAIC")))
expect_error(pclm(x, y, nlast, control = list(max.iter = 5)))
expect_error(pclm(x, y, nlast, control = list(tol = -.1)))

expect_error(pclm2D(c(x, 90), y2, nlast))
expect_error(pclm2D(x, y, nlast))
expect_error(pclm2D(x, y2, nlast, rbind(offset, 0)))

# ----------------------------------------------
# Test warnings
expect_warning(pclm(x, y, nlast, offset, out.step = 0.32))

# ----------------------------------------------
# Test data

expect_output(print(pclm.data))





