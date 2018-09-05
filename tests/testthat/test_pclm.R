rm(list = ls())
library(testthat)
library(ungroup)

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
           control = list(lambda = NA, kr = 6, deg = 3))
M4 <- pclm(x, y, nlast, offset, out.step = 0.4,
           control = list(lambda = 1, kr = 8, deg = 3))


ungroupped_Ex <- pclm(x, y = offset, nlast, offset = NULL)$fitted # ungroupped offset data
M5 <- pclm(x, y, nlast, offset = ungroupped_Ex)

for (i in 1:5) test_pclm_1D(get(paste0("M", i)))


# ----------------------------------------------
# test residuals

test_that("Residuals", {
  expect_output(print(residuals(M1)))
  expect_output(print(residuals(M2)))
  expect_output(print(residuals(M3)))
  expect_error(residuals(M4))
})

# ----------------------------------------------
# Test error messages

expect_error(pclm(x = c("a", x), y, nlast))
expect_error(pclm(x = c(NA, x), y, nlast))
expect_error(pclm(x = c(1, x), y, nlast))
expect_error(pclm(x = c(1, x), c(y, NA), nlast))
expect_error(pclm(x = c(x, 90), c(y, -10), nlast))
expect_error(pclm(x, y, nlast = -10))
expect_error(pclm(x, y, nlast = c(1, 100)))
expect_error(pclm(x, y, nlast, c(offset, 1)))
expect_error(pclm(x, y, nlast, ci.level = -0.05))
expect_error(pclm(x, y, nlast, out.step = -1))
expect_error(pclm(x, y, nlast, control = c(a = 1))) #****
# expect_error(pclm(x, y, nlast, control = list(lambda = c(0, 1))))
expect_error(pclm(x, y, nlast, control = list(lambda = -1)))
expect_error(pclm(x, y, nlast, control = list(kr = -1.5)))
expect_error(pclm(x, y, nlast, control = list(deg = -1.5)))
expect_error(pclm(x, y, nlast, control = list(opt.method = "AAIC")))
expect_error(pclm(x, y, nlast, control = list(max.iter = 5)))
expect_error(pclm(x, y, nlast, control = list(tol = -.1)))

# ----------------------------------------------
# Test warnings
expect_warning(pclm(x, y, nlast, offset, out.step = 0.32))

# ----------------------------------------------
# Test data
expect_output(print(ungroup.data))

# ----------------------------------------------

test_that("The model works even if the first bin is zero", {
  x0 <- c(14:19, seq(20, 50, by = 5))
  y0 <- c(0, 5, 27, 154, 404, 826, 15596, 31266, 32973, 28942, 14290, 1988, 25)
  M0 <- pclm(x = x0, y = y0, nlast = 5)
  test_pclm_1D(M0)
})



