rm(list = ls())
library(testthat)
library(ungroup)

# ----------------------------------------------
# Tests 
test_pclm_1D <- function(M) {
  fv    <- fitted(M)
  lower <- M$ci$lower
  upper <- M$ci$upper
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
# Every expectation pins the message that is actually raised. An expect_error
# with no pattern accepts an error from any cause, which is how a typo in the
# arguments passed unnoticed here before.
test_that("Input validation rejects malformed input", {
  expect_error(pclm(x = c("a", x), y, nlast), "must be a vector of class numeric")
  expect_error(pclm(x = c(NA, x), y, nlast), "contains NA values")
  expect_error(pclm(x = c(1, x), y, nlast), "must be equal")
  expect_error(pclm(x = c(1, x), c(y, NA), nlast), "contains NA values")
  expect_error(pclm(x = c(x, 90), c(y, -10), nlast), "contains negative values")
  expect_error(pclm(x, y, nlast = -10), "must be greater than 0")
  expect_error(pclm(x, y, nlast = c(1, 100)), "has to be a scalar")
  expect_error(pclm(x, y, nlast, c(offset, 1)), "non-conformable arguments")
  expect_error(pclm(x, y, nlast, ci.level = -0.05), "must take values in the")
  expect_error(pclm(x, y, nlast, out.step = -1), "must be between")
  expect_error(pclm(x, y, nlast, control = c(a = 1)), "second argument must be a list")
  expect_error(pclm(x, y, nlast, control = list(lambda = -1)), "positive")
  # NA marks a lambda to be found by optimisation, so a partial vector is
  # legal input and must reach the length check rather than abort on an NA
  # comparison inside the value check.
  expect_error(pclm(x, y, nlast, control = list(lambda = c(1, NA))), "must have length")
  expect_error(pclm(x, y, nlast, control = list(lambda = c(0, 1))), "must have length")
  expect_error(pclm(x, y, nlast, control = list(lambda = 0)), "positive")
  expect_error(pclm(x, y, nlast, control = list(lambda = Inf)), "positive")
  expect_error(pclm(x, y, nlast, control = list(lambda = NaN)), "positive")
  expect_error(pclm(x, c(y[-length(y)], Inf), nlast), "non-finite")
  expect_error(pclm(c(x[-length(x)], Inf), y, nlast), "non-finite")
  expect_error(pclm(x, y, nlast, control = list(kr = -1.5)), "must be a positive integer")
  expect_error(pclm(x, y, nlast, control = list(deg = -1.5)), "greater or equal than 2")
  expect_error(pclm(x, y, nlast, control = list(opt.method = "AAIC")), "should be one of")
  expect_error(pclm(x, y, nlast, control = list(max.iter = 5)), "should be at least 10")
  expect_error(pclm(x, y, nlast, control = list(tol = -.1)), "must be greater than 0")
})

# ----------------------------------------------
# Test warnings
test_that("nlast is adjusted to match out.step", {
  expect_warning(pclm(x, y, nlast, offset, out.step = 0.32), "has been adjusted")
})

# ----------------------------------------------
# Test data
test_that("Bundled data prints", {
  expect_output(print(ungroup.data))
})

# ----------------------------------------------
# Regression pin. The fit is deterministic, so these numbers are what lets
# the suite notice a change to the numerics at all. Tolerances are relative
# and loose enough to survive a different BLAS, tight enough to catch a real
# change to the estimator.
test_that("Regression: the 1D fit is numerically stable", {
  M  <- pclm(x, y, nlast)
  # fitted() carries bin names, which would make the scalar comparisons fail
  # on attributes rather than on values.
  fv <- unname(fitted(M))

  expect_identical(length(fv), 111L)
  expect_equal(fv[1], 292.25494488, tolerance = 1e-6)
  expect_equal(fv[26], 56.12353504, tolerance = 1e-6)
  expect_equal(fv[55], 368.25361568, tolerance = 1e-6)
  expect_equal(fv[111], 1.76116801, tolerance = 1e-6)
  expect_equal(max(fv), 3983.17889304, tolerance = 1e-6)
  # Mass is conserved: the ungrouped counts total the observed counts.
  expect_equal(sum(fv), sum(y), tolerance = 1e-6)
  expect_equal(AIC(M), 39.97155669, tolerance = 1e-6)
  expect_equal(BIC(M), 59.80610375, tolerance = 1e-6)
})

# ----------------------------------------------
# Regression: the output labels must be sized by the validated nlast and the
# composition matrix by the fine grid. Before both were fixed, 47 of 91
# out.step values in [0.1, 1] aborted with a names-length error and others
# with a dimnames error, so this loop is the whole test for that.
test_that("Regression: labels match the fit for any out.step", {
  for (os in c(0.1, 0.11, 0.32, 0.9)) {
    M  <- suppressWarnings(
      pclm(x, y, 26, out.step = os, verbose = FALSE, control = list(lambda = 100))
    )
    nm <- M$bin.definition$output$names
    expect_identical(
      object   = length(nm),
      expected = length(fitted(M)),
      info     = paste("out.step =", os)
    )
  }
})

# ----------------------------------------------
# Coverage for the exported surface and for the argument checks that used to
# pass silently or abort with a base R message.
test_that("Control constructors return what they document", {
  expect_identical(
    names(control.pclm()),
    c("lambda", "kr", "deg", "int.lambda", "diff", "opt.method", "max.iter", "tol")
  )
  expect_identical(
    names(control.pclm2D()),
    c("lambda", "kr", "deg", "int.lambda", "diff", "opt.method", "max.iter", "tol")
  )
  expect_error(control.pclm(opt.method = "a"), "should be one of")
})

test_that("Control defaults match the values the help pages state", {
  # Each of these was documented wrongly at some point: kr and int.lambda
  # differ between the two models, and the shared param text said otherwise.
  a <- control.pclm()
  b <- control.pclm2D()

  expect_identical(a$lambda, NA)
  expect_identical(b$lambda, c(1, 1))
  expect_identical(a$kr, 2)
  expect_identical(b$kr, 7)
  expect_identical(a$int.lambda, c(0.1, 1e5))
  expect_identical(b$int.lambda, c(0.1, 1e3))
  expect_identical(a$deg, 3)
  expect_identical(b$deg, 3)
  expect_identical(a$opt.method, "BIC")
  expect_identical(a$tol, 1e-3)
  expect_identical(a$max.iter, 1e3)

  # Both models return the same eight names, in the same order.
  expect_identical(names(a), names(b))
})

test_that("Misspelled control names are refused, unnamed ones are positional", {
  # The help page makes both statements, so both are pinned here.
  expect_error(pclm(x, y, nlast, control = list(maxiter = 500)),
               "unused argument")

  # list(100) matches lambda positionally and nothing else moves.
  M <- suppressWarnings(pclm(x, y, nlast, control = list(100)))
  expect_identical(unname(M$smoothPar), c(100, 2, 3))
})

test_that("suggest.valid.out.step returns exact divisors of the span", {
  expect_equal(
    suggest.valid.out.step(111),
    c(0.1, 0.2, 0.25, 0.37, 0.5, 0.6, 0.74, 0.75, 1)
  )
})

test_that("summary reports lambda without flattening it to an integer", {
  # round() on the smoothing parameters printed lambda = 0.1 as 0, hiding the
  # value the fit actually used. lambda is continuous; kr and deg are counts.
  M <- pclm(x, y, nlast, control = list(lambda = 65.22826))
  out <- capture.output(print(summary(M)))
  expect_true(any(grepl("Smoothing parameter lambda   : 65.23", out, fixed = TRUE)))

  M0 <- pclm(x, y, nlast)
  out0 <- capture.output(print(summary(M0)))
  expect_true(any(grepl("Smoothing parameter lambda   : 0.1", out0, fixed = TRUE)))
})

test_that("Information criteria honour k", {
  M  <- pclm(x, y, nlast, control = list(lambda = 100))
  tr <- M$deep$trace
  expect_equal(AIC(M, k = 4), AIC(M, k = 2) + 2 * tr, tolerance = 1e-10)
  # BIC has no k, exactly as stats::BIC has none. BIC.pclm2D used to declare
  # one and silently drop it; now it is refused outright.
  expect_error(BIC(M, k = 7), "no extra arguments")
  expect_equal(BIC(M), M$deep$dev + log(M$deep$ny_) * tr, tolerance = 1e-10)
})

test_that("Zero counts do not derail the fit", {
  # Empty bins are routine in binned counts. They used to turn the convergence
  # metric into NaN and leave the solver burning every iteration it had.
  y0 <- y
  y0[c(2, 5, 8)] <- 0
  M  <- suppressWarnings(
    pclm(x, y0, 26, verbose = FALSE, control = list(lambda = 100))
  )
  expect_true(all(is.finite(fitted(M))))
  expect_true(all(fitted(M) >= 0))
})

test_that("Unsorted ages are rejected", {
  expect_error(pclm(rev(x), y, nlast), "strictly increasing")
})

# ----------------------------------------------
# Features for the open GitHub issues: NA omission (issues 4 and 6) and an
# omega in place of nlast (issue 4).
test_that("Missing cells can be omitted rather than rejected", {
  y0 <- y
  y0[c(3, 7)] <- NA

  # The default still refuses them. Existing callers depend on that.
  expect_error(pclm(x, y0, nlast), "contains NA values")

  M <- suppressWarnings(
    pclm(x, y0, 26,
         verbose   = FALSE,
         control   = list(lambda = 100),
         na.action = "omit")
  )
  # Omitting observations must not change the shape of the output: the fine
  # grid is untouched, only the rows entering the likelihood are dropped.
  expect_identical(length(fitted(M)), 111L)
  expect_true(all(is.finite(fitted(M))))
  expect_true(all(fitted(M) >= 0))

  # Infinite values are never "unobserved", whatever na.action says.
  expect_error(
    pclm(x, c(y[-length(y)], Inf), 26,
         verbose   = FALSE,
         control   = list(lambda = 100),
         na.action = "omit"),
    "non-finite"
  )
})

test_that("omega stands in for nlast", {
  M1 <- suppressWarnings(
    pclm(x, y, 26, verbose = FALSE, control = list(lambda = 100))
  )
  M2 <- suppressWarnings(
    pclm(x, y, omega = 26 + max(x),
         verbose = FALSE, control = list(lambda = 100))
  )
  # omega = 26 + max(x) means exactly nlast = 26, so the two must agree.
  expect_equal(unname(fitted(M2)), unname(fitted(M1)), tolerance = 1e-12)

  expect_error(pclm(x, y, 26, omega = 100), "not both")
  expect_error(pclm(x, y), "supply either")
  expect_error(pclm(x, y, omega = max(x)), "greater than max")
  expect_error(pclm(x, y, omega = Inf), "single finite value")
})

test_that("verbose reports progress", {
  expect_output(
    suppressWarnings(
      pclm(x, y, nlast, offset, verbose = TRUE, control = list(lambda = 100))
    ),
    "Ungrouping"
  )
})

# ----------------------------------------------

test_that("The model works even if the first bin is zero", {
  x0 <- c(14:19, seq(20, 50, by = 5))
  y0 <- c(0, 5, 27, 154, 404, 826, 15596, 31266, 32973, 28942, 14290, 1988, 25)
  M0 <- pclm(x = x0, y = y0, nlast = 5)
  test_pclm_1D(M0)
})



