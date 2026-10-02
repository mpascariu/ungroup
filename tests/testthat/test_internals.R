# Guards inside the compiled core and the optimisation boundary.
#
# These live in the C++ solver and in optimize_par, so they are unreachable
# from pclm()/pclm2D() with valid input: the R layer has already rejected the
# malformed shapes and values before the solver sees them. The tests below
# call the internals directly to pin each stop() and warning(). That is the
# only way to keep these branches from rotting unnoticed.
rm(list = ls())

x <- c(0, 1, seq(5, 85, by = 5))
nlast  <- 26
iC     <- ungroup:::asSparseMat(matrix(1, 2, 3))
iB     <- matrix(1, 3, 2)
iP     <- diag(2)
iy     <- c(1, 1)

test_that("asSparseMat insists on a matrix", {
  expect_error(ungroup:::asSparseMat(1:5), "must be a matrix")
})

test_that("pclm_loop validates its scalars", {
  expect_error(ungroup:::pclm_loop(iC, iP, iB, iy, 0, 1e-3), "maxiter")
  expect_error(ungroup:::pclm_loop(iC, iP, iB, iy, 10, 0), "tol")
})

test_that("pclm_loop validates the shapes it multiplies", {
  expect_error(ungroup:::pclm_loop(iC, iP, iB, c(1, 1, 1), 10, 1e-3), "nrow\\(C\\)")
  expect_error(ungroup:::pclm_loop(iC, iP, matrix(1, 4, 2), iy, 10, 1e-3), "ncol\\(C\\)")
  expect_error(ungroup:::pclm_loop(iC, diag(3), iB, iy, 10, 1e-3), "must be square")
})

test_that("pclm_loop validates the counts it is asked to fit", {
  expect_error(ungroup:::pclm_loop(iC, iP, iB, c(1, Inf), 10, 1e-3), "finite")
  expect_error(ungroup:::pclm_loop(iC, iP, iB, c(1, -1), 10, 1e-3), "negative")
  expect_error(ungroup:::pclm_loop(iC, iP, iB, c(0, 0), 10, 1e-3), "positive")
})

test_that("pclm_loop reports an unfactorizable penalty", {
  expect_error(ungroup:::pclm_loop(iC, matrix(Inf, 2, 2), iB, iy, 10, 1e-3),
               "could not be factorized")
})

test_that("a lambda fixed at the upper bound of int.lambda warns", {
  # In the 2D model one lambda may be held fixed while the other is optimised.
  # Holding it exactly at int.lambda[2] makes the boundary check deterministic,
  # which the continuous search never manages by itself.
  Dx <- ungroup.data$Dx[, 15:35]
  n  <- c(diff(x), nlast)
  y2 <- aggregate(Dx[, 1:20], by = list(rep(x, n)), FUN = "sum")[, -1]

  expect_warning(
    pclm2D(x, y2, nlast,
           control = list(lambda = c(1, NA), int.lambda = c(0.1, 1),
                          max.iter = 100)),
    "reached the upper limit"
  )
})
