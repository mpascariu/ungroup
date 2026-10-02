# --------------------------------------------------- #
# Author: Marius D. PASCARIU
# Last update: Wed Jun 23 22:12:24 2021
# --------------------------------------------------- #
remove(list = ls())
library(testthat)
library(ungroup)


test_pclm_2D <- function(M) {
  fv    <- fitted(M)
  lower <- M$ci$lower
  upper <- M$ci$upper
  test_that("PCLM-2D", {
    expect_s3_class(M, "pclm2D")
    expect_output(print(M))
    expect_output(print(summary(M)))
    expect_false(is.null(plot(M)))
    expect_true(all(fv >= 0))
    expect_identical(dim(fv), dim(lower))
    expect_identical(dim(upper), dim(lower))
    if (is.null(M$input$offset)) {
      expect_true(abs(sum(fv) - sum(M$input$y)) < 1)
    }
  })
}

# ----------------------------------------------
# PCLM-2D
x  <- c(0, 1, seq(5, 85, by = 5))
nlast <- 26 # the size of the last interval
Dx <- ungroup.data$Dx[, 15:35]
Ex <- ungroup.data$Ex[, 15:35]
n  <- c(diff(x), nlast)
Ex$gr  <- Dx$gr <- rep(x, n)
y2      <- aggregate(Dx[, 1:20], by = list(Dx$gr), FUN = "sum")[, -1]
offset2 <- aggregate(Ex[, 1:20], by = list(Ex$gr), FUN = "sum")[, -1]

P1 <- pclm2D(x, as.matrix(y2), nlast)
P2 <- pclm2D(x, y2, nlast, offset2, control = list(max.iter = 200))
P3 <- pclm2D(x, y2, nlast, control = list(lambda = c(NA, NA), max.iter = 200))

ungroupped_Ex <- pclm2D(x, y = offset2, nlast, offset = NULL)$fitted # ungroupped offset data
P4 <- pclm2D(x, y2, nlast, offset = ungroupped_Ex)

# plot(P1)
# plot(P2)
# plot(P3)
# plot(P4)


for (i in 1:4) test_pclm_2D(get(paste0("P", i)))

# ----------------------------------------------
# test residuals

test_that("Residuals", {
  expect_output(print(residuals(P1)))
  expect_error(residuals(P2))
})


# ----------------------------------------------
# Test error messages
test_that("pclm2D rejects malformed input", {
  # x is one bin longer than the rows of y.
  expect_error(pclm2D(c(x, 90), y2, nlast), "nrow")
  # A plain vector is not a valid 2D response.
  expect_error(pclm2D(x, y2[, 1], nlast), "must be a data.frame or a matrix")
  # An offset with a row too many cannot be composed with y.
  expect_error(pclm2D(x, y2, nlast, rbind(offset2, 0)), "non-conformable")
  # The 2D model has two smoothing parameters. A scalar left the second
  # penalty unpenalised and returned an all-NaN fit with no warning.
  expect_error(
    pclm2D(x, y2, nlast, verbose = FALSE, control = list(lambda = 5)),
    "must have length"
  )
})

test_that("A partially specified lambda is optimised, not rejected", {
  # NA marks a lambda to be found by optimisation. This documented path used
  # to abort with "missing value where TRUE/FALSE needed" before any fitting.
  P <- suppressWarnings(
    pclm2D(x, y2, nlast,
           verbose  = FALSE,
           control  = list(lambda = c(5, NA), max.iter = 200))
  )
  expect_s3_class(P, "pclm2D")
})

test_that("Information criteria honour k in the 2D model", {
  tr <- P1$deep$trace
  expect_equal(AIC(P1, k = 4), AIC(P1, k = 2) + 2 * tr, tolerance = 1e-10)
  expect_error(BIC(P1, k = 7), "no extra arguments")
  expect_equal(BIC(P1), P1$deep$dev + log(P1$deep$ny_) * tr, tolerance = 1e-10)
})

test_that("Integer counts are accepted", {
  # aggregate() can hand back integers and the mapped vector refused them.
  y2i <- y2
  y2i[] <- lapply(y2i, as.integer)
  P <- suppressWarnings(
    pclm2D(x, y2i, 26, verbose = FALSE,
           control = list(lambda = c(1, 1), max.iter = 200))
  )
  expect_s3_class(P, "pclm2D")
  expect_true(all(is.finite(fitted(P))))
})

test_that("A missing year is interpolated, not fatal", {
  # This is the shape of both open data issues: a rectangular surface with
  # whole columns or interior cells unobserved.
  y3 <- y2
  y3[, 3] <- NA

  expect_error(pclm2D(x, y3, nlast, verbose = FALSE), "contains NA values")

  P <- suppressWarnings(
    pclm2D(x, y3, nlast,
           verbose   = FALSE,
           control   = list(lambda = c(1, 1), max.iter = 200),
           na.action = "omit")
  )
  expect_s3_class(P, "pclm2D")
  expect_true(all(is.finite(fitted(P))))
  # One column per input year survives, gaps included.
  expect_identical(ncol(fitted(P)), ncol(y3))
})

test_that("Missing cells work with an offset too", {
  # The Bangladesh case: exposures and deaths both unobserved at high ages in
  # the early years. The offset is ungrouped first, so it has to travel the
  # same omission path as the counts.
  y4  <- y2
  e4  <- offset2
  y4[1:3, 1:4] <- NA
  e4[1:3, 1:4] <- NA

  P <- suppressWarnings(
    pclm2D(x, y4, nlast, e4,
           verbose   = FALSE,
           control   = list(lambda = c(1, 1), max.iter = 200),
           na.action = "omit")
  )
  expect_s3_class(P, "pclm2D")
  expect_true(all(is.finite(fitted(P))))
})
