rm(list = ls())
library(testthat)
library(ungroup)


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

P1 <- pclm2D(x, y2, nlast)
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
expect_error(pclm2D(c(x, 90), y2, nlast))
expect_error(pclm2D(x, y, nlast))
expect_error(pclm2D(x, y2, nlast, rbind(offset, 0)))
