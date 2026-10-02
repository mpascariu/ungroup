# Code review: ungroup 1.4.5

```
subject    ungroup (PCLM ungrouping of binned counts; R + RcppEigen)
branch     dev
commit     2423c19  "v1.4.5 - fix vignette typo"
date       2026-10-02
scope      whole package: R/, src/, tests/, vignettes/, man/, packaging
mode       read-only. No code was changed. This file is the only artifact.
toolchain  R 4.6.0, RcppEigen 0.3.4.0.2 (Eigen 3.4), Matrix 1.7.6, Rcpp 1.1.2, testthat 3.3.2
```

## Verdict

CRAN packaging is healthy. Correctness is not merge-ready.

There are two crash-level defects, several paths that return silently wrong
output, and a confidence interval implementation that is not an interval
around its own point estimate.

```
R CMD check --as-cran (temp copy, vignettes built)   Status: 1 NOTE
  NOTE: examples with elapsed time > 5s  ->  residuals.pclm2D  6.27s
  everything else clean: tests, examples, compiled code, all Rd checks
```

Method: 6 subagent slices (5 dispatched, 1 re-run for thin coverage) plus
direct probes by the orchestrator. Findings below are marked with how they
were established. Anything not directly observed is tagged `[reported]` or
`[unverified]`.

---

## 1. Blockers

### B1. Output labels use the raw `nlast`, the fit uses the validated one

> **Status: FIXED.** `map.bins()` now receives `nlast.adj`, the validated width,
> captured before `create.artificial.bin()` overwrites `I$nlast` with `out.step`.
> Verified: `out.step` swept over 91 values in `[0.1, 1]`, **91 OK** against
> 47 CRASH / 44 OK before. Regression test loops `0.1, 0.11, 0.32, 0.9`.

`map.bins()` is called with the user's original `nlast`, while the fit is
sized from the value `validate.nlast()` returns. When those differ the name
vector and the fitted vector have different lengths and the assignment dies.

```
R/pclm_1D.R:131   I$nlast <- validate.nlast(x, nlast, out.step)   # adjusted
R/pclm_1D.R:176   G  <- map.bins(x, nlast, out.step)              # raw
R/pclm_1D.R:178   names(R$fit) <- names(R$lower) <- ... <- dn     # aborts
R/pclm_2D.R:107   same bug, same fix
```

Reproduced:

```
'nlast' has been adjusted in order to obtain 123 bins ... Now 'nlast = 25.7'.
The impact in results should be insignificant.
nlast=26 out.step=0.9 -> ERROR: 'names' attribute [124] must be the same
length as the vector [123]
```

Sweep over `out.step` in `[0.1, 1]` by 0.01, fixed `lambda`, ordinary input:
**47 CRASH, 44 OK out of 91**. More than half of all legal `out.step` values
crash.

The warning text makes it worse. It says the adjustment "should be
insignificant" and then lists `out.step` values to try instead. A user who
picks 0.9 from that list loses the fit.

**Fix option.** Capture the adjusted value and use it at both mapping sites.

```r
I$nlast   <- validate.nlast(x, nlast, out.step)
nlast.adj <- I$nlast
# ...
G <- map.bins(x, nlast.adj, out.step)
```

Verified in advance: `map.bins(x, adjusted, out.step)` matches the fitted
length in every failing case. One line, plus the same at `R/pclm_2D.R:107`.

### B2. `build_C_matrix` sizes columns from a float sum, not from the grid

> **Status: FIXED.** `ncol = length(gx)` replaces `sum(gu)`. `gu` had no other
> consumer in `R/` (grep clean) and was removed. Verified: the reported case
> `nlast = 25.99, out.step = 0.11` returns 1009 fitted bins.

```
R/pclm_fit.R:66-69
  gx <- seq(min(x), max(x) + nlast - out.step, by = out.step)
  gu <- c(diff(x), nlast) / out.step
  matrix(0, nrow = nx, ncol = sum(gu), dimnames = list(x, gx))
```

`length(gx)` and `sum(gu)` disagree in floating point. Example: with
`nlast = 25.99`, `out.step = 0.11`, `length(gx)` is 1010 while `sum(gu)` is
1009.9999999999999, and `dimnames<-` fails. Independent root cause from B1,
and it fires earlier (inside `pclm.fit`, before any naming).

`[reported]` 10 of 90 `out.step` values fail this way. `[unverified by me]`

**Fix option.** Derive the count from the grid: build `gx` first and use
`ncol = length(gx)`. Or guard it explicitly with
`stopifnot(length(gx) == sum(gu))` and a real message.

---

## 2. Silent wrong output

### M1. Confidence bounds invert and exclude the point estimate

> **Status: DOCUMENTED, behaviour unchanged.** Option 1 from the
> re-analysis: the mass-conserving scenario pair is preserved exactly as
> `5bf1de9` wrote it, and `?pclm` now says which pair is which. The true
> marginal intervals ship alongside as `ci$conf_lower` / `ci$conf_upper`,
> computed as `fitted * exp(-/+ z SE)` and exact, since `log mu = B beta` is
> Gaussian in the coefficients. Verified: both scenario columns still total
> `sum(fitted)`, and `conf_lower <= fitted <= conf_upper` with **0 violations
> of 111**.

`pclm.confidence.dx` builds the `dx` endpoints from opposite-signed z
quantiles and then rescales each column to a common total:

```r
R/pclm_CI.R:84-108
  mxu <- exp(log(fit) + SE %*% Qn)/Lx
  qxu <- 1 - exp(-out.step*mxu)
  dxU <- apply(qxu, 2, qx_lx_dx)
  dxU <- apply(dxU, 2, function(dx.hat) dx.hat * sum(fit)/sum(dx.hat))
```

That rescaling can reorder the endpoints.

```
bins: 111
lower > upper       : 60 of 111
fitted outside [lo,up] : 64 of 111
negative lower      :  0 of 111
```

The point estimate falls outside its own confidence interval in 58% of bins.
The first bins are sane (`fit 6.90 / lo 4.30 / up 11.78`); the inversion
lives in the tail after the rescale.

`[reported]` under default settings the counts are 24 of 111 bins inverted,
and 464 of 2220 bins for the 2D surface.

**Re-analysis (2026-10-02). This is not a numerical bug.** It is a naming and
documentation defect. The construction is internally correct for what it
actually computes, which is not a confidence interval.

First, design intent is unambiguous. `git log -L` on this block shows it was
introduced by:

```
5bf1de9 BRAND NEW CONFIDENCE INTERVALS. The old ci's did not respect the
        compositional property of a distribuiton (sum up to unity in any
        scenario).
```

and `R/pclm_CI.R:97` carries `# # make sure that the size of the population is
the same`. **Mass-first was the explicit design goal.** The rescale is the
point of the rewrite, not an afterthought. Measured: both columns sum to
`sum(fit) = 104` exactly.

Second, the "inversions" are demographically correct. Column 1 is built from
`-qn` (uniformly lower hazards) and column 2 from `+qn`, then each is pushed
through a life table at fixed radix `lx[1] = sum(fit)`. Under uniformly lower
mortality with total deaths held constant, deaths shift to older ages. So the
low scenario legitimately has FEWER deaths at young ages and MORE at old ages,
and the two scenario curves must cross. Measured zones are contiguous, which
is the signature of a systematic crossing rather than noise:

```
bins  1-44 : lower <= fit <= upper      (bracketed and ordered)
bins 45-59 : fit > upper                (upper scenario below the fit)
bins 55-111: lower > upper              (the 57 "inverted" bins, contiguous)
bins 60-111: lower > fit > upper        (both scenarios crossed past the fit)
inverted bin indices: 55..111 contiguous, median 83
```

The earlier finding that dropping the rescale leaves 55 of 111 inversions is
consistent with this: the rescale is one scalar per column and cannot reorder
bins. Ordering is lost in `qx_lx_dx`, and lost correctly.

**So what is actually wrong is the contract.** `man/pclm.Rd:60` documents
`\item{ci}{ Confidence intervals around fitted values.}` and the fields are
called `lower`/`upper`. That promises pointwise bracketing and 95% coverage.
This construction delivers two equal-mass mortality scenarios, which are
neither pointwise bounds nor a coverage guarantee, and which cross the fit by
design. A user who reads `ci$lower` as "the 2.5th percentile of deaths in this
age bin" is wrong in 60 of 111 bins.

**Fix options.** Do not clamp and do not sort. `pmin`/`pmax` would look tidy
but it breaks the very property `5bf1de9` was written to enforce, because
swapping elements between two equal-sum columns destroys both column sums.

1. **Rename and document (minimal, preserves behaviour).** Treat these as
   scenarios at constant total deaths: `ci$lower_scenario` /
   `ci$upper_scenario`, documented as "death distribution under uniformly
   lower / higher hazard, total deaths held at `sum(fitted)`". Note in the
   docs that the curves cross the fit in the tail and that crossing is
   expected. Behaviour unchanged, so no user's numbers move.
2. **Also ship real intervals (recommended).** You already hold the sandwich
   covariance `H1 <- H0 %*% QmQ %*% H0`. Draw `beta` from `N(beta_hat, H1)`,
   push each draw through the full life table, take empirical 2.5%/97.5%
   quantiles per bin. Empirical quantiles guarantee `lower <= upper` and give
   honest marginal coverage. Keep these as `ci$lower`/`ci$upper` and demote
   the scenario pair to the explicit names.
3. **If what users want is `e0` or `l_x` bounds**, bin-wise bounds are the
   wrong instrument either way. Take quantiles of the derived quantity per
   bootstrap draw.

If option 2 is chosen, this is the one change in the report that moves
published output on ordinary input, so it needs a characterization test
first. Option 1 alone does not.

### M2. `AIC.pclm2D` / `BIC.pclm2D` silently ignore `k`

> **Status: FIXED.** `AIC.pclm2D` forwards `k` by name. Verified:
> `AIC(P1, k = 4)` differs from `k = 2` by exactly `2 * trace`.

```r
R/pclm_CI.R:121   AIC.pclm2D <- function(object, ..., k = 2) AIC.pclm(object, ..., k)
R/pclm_CI.R:147   BIC.pclm2D <- function(object, ..., k = 2) BIC.pclm(object, ..., k)
```

`k` is forwarded unnamed past `...`. R does not match unnamed arguments to
formals that follow `...`, so it lands in `...` and the callee keeps its
default.

```
g(1, k = 6) -> k=2 dots_len=1
```

`AIC.pclm2D(x, k = 6)` returns `dev + 2*trace`. Direct `AIC.pclm(x, k = 6)`
works, so the breakage is only through the 2D wrappers and is easy to miss.

**Fix option.** `AIC.pclm2D <- function(object, ..., k = 2) AIC.pclm(object, k = k)`.

### M3. `BIC.pclm2D`'s `k` is dead

> **Status: FIXED.** `k` removed from `BIC.pclm2D`. `stats::BIC` declares no `k`
> (not even `BIC.default`), so a method carrying one was the wrong shape, and
> forwarding `...` would still have swallowed it. `BIC.pclm` now raises on any
> extra argument instead of dropping it, so `BIC(P1, k = 7)` is refused loudly.

`BIC.pclm <- function(object, ...)` has no `k` formal at all, so `k` is
accepted and discarded.

```
h(1, k = 6) -> BIC-no-k-formal
```

**Fix option.** Add `k` to `BIC.pclm`'s formals and use it, or remove `k`
from `BIC.pclm2D`. Document either way.

### M4. Mixed `NA` lambda crashes validation

> **Status: FIXED.** The value check masks what is to be optimised before
> comparing. Verified: `pclm2D(..., lambda = c(5, NA))` now fits with 0 NaN
> fitted values; `c(1, NA)` in 1D reaches the length check instead of aborting.

```r
R/utils.R:64-66
  if (any(!is.na(lambda)) && any(lambda < 0)) {
    stop("'lambda' must be a positive scalar", call. = FALSE)
  }
```

With `lambda = c(1, NA)`, `any(lambda < 0)` is `NA`, the `if` gets `NA`, and
it aborts with "missing value where TRUE/FALSE needed".

```
lambda = 1,NA -> ERROR: missing value where TRUE/FALSE needed
```

This breaks a documented feature. `R/pclm_optim.R:64-67` states that when
only one lambda is supplied the algorithm searches for the missing one, and
the 2D example uses `lambda = c(NA, NA)`.

**Fix option.** `if (any(!is.na(lambda) & lambda < 0))`.

### M5. `lambda` length is never validated

> **Status: FIXED.** A length check keyed on the model type runs before any
> value check: 1 for `"1D"`, 2 for `"2D"`. Verified: scalar `lambda` to `pclm2D`
> is rejected with `must have length 2 in the 2D model` rather than returning
> an all-NaN fit.

`[reported]` A scalar `lambda` given to `pclm2D` reaches `build_P_matrix`,
where `L[2]` is `NA` and `P` becomes all-`NA`. The call returns a classed
object with 999/999 NaN fitted values, NaN AIC and `smoothPar = 5, NA, 7, 3`,
with no error and no warning. In 1D a length-2 `lambda` is recycled and
returns 111/111 NaN after only a recycling warning.

**Fix option.** Length check keyed on model type inside `pclm.input.check`:
length 1 for `"1D"`, length 2 for `"2D"`.

### M6. `lambda = 0` and `lambda = Inf` pass validation

> **Status: FIXED.** Anything not marked for optimisation must be positive and
> finite. Verified: `0`, `Inf` and `NaN` all raise
> `must be NA or a positive, finite value`.

The only value check is `any(lambda < 0)`. Both produce an all-NaN fit
returned as a normal object.

```
lambda = 0   -> OK, NaN fitted: 111 of 111
lambda = Inf -> OK, NaN fitted: 111 of 111
```

The package's own error text says lambda must be positive, so zero and
infinity slipping through contradicts the stated contract.

**Fix option.** `if (any(!is.finite(lambda) | lambda <= 0)) stop(...)`.

### M7. Non-finite `y` passes validation

> **Status: FIXED.** A finiteness guard follows each `NA` guard, on `x` and on
> `unlist(y)` so a data.frame response is covered too. Verified: `Inf` in `y`
> and `Inf` in `x` both raise `contains non-finite values`.

```r
R/utils.R:21   if (any(is.na(y))) stop("'y' contains NA values", ...)
R/utils.R:24   if (any(y < 0))    stop(...)
```

`Inf` clears both checks and yields an all-NaN fit with no warning.

```
any(is.na(Inf)) = FALSE ; any(Inf < 0) = FALSE  =>  Inf passes both guards
```

`[reported]` downstream: `anyNA(fitted) == TRUE`, `sum(fitted) == NaN`,
`AIC == NaN`, silently.

**Fix option.** `if (any(!is.finite(y))) stop("'y' contains non-finite values", call. = FALSE)`, likewise for `x`.

### M8. Integer counts fail in `pclm2D` but work in `pclm`

> **Status: FIXED.** `as.double(unlist(y))` in `R/pclm_fit.R` covers both entry
> points. Verified: `pclm2D` on integer columns returns a finite fit.

```
double  y -> OK
integer y -> ERROR: Wrong R type for mapped vector
```

`Eigen::Map<VectorXd>` requires `REALSXP`. `R/pclm_1D.R:128` does
`y <- as.numeric(y)`; `pclm_2D.R` never coerces and `R/pclm_fit.R:43` does
`as.vector(unlist(y))`, which preserves integer. The public `pclm2D` example
builds `y` with `aggregate(Dx, ...)`, which stays integer for integer `Dx`.
The user sees a raw C++ message rather than a package-level one.

**Fix option.** `y_ <- as.double(unlist(y))` in `R/pclm_fit.R:43`. One line,
fixes both entry points.

### M9. Zero counts silently disable the convergence test

> **Status: FIXED.** The denominator is floored at 1 and the relative change is
> guarded against a zero previous value. Verified numerically invisible:
> `max.iter` 10 vs 1000 differ by `1.4e-07`, and by iteration 20 the fit is
> converged to `7.8e-14`. So the fix recovers roughly 98% of the wasted
> iterations and moves no reported digit.

```cpp
src/RcppEigenPclm.cpp:50-53
  dd = (y-muA).cwiseQuotient(y).cwiseAbs().mean() - d;
  d  = d + dd;
  dd = std::abs(dd)/d;
  if((d < tol || dd < 0.001) && i >= 3) break;
```

Elementwise division by the observed counts. A zero bin makes the mean `NaN`,
after which both stop conditions are permanently false and every call burns
all `max.iter` iterations (default 1000).

```
no zeros  : elapsed= 0      (stops at i >= 3)
with zeros: elapsed= 1.64   (ran all 200,000 iterations)
```

The fit itself stays finite. This is silent waste plus a false convergence
guarantee, multiplied across `optimize_par`'s lambda search and `pclm2D`'s
per-column sweep.

The package already knows zeros are hazardous and does not act on it:

```
R/utils.R:28     if(any(y == 0)) message("Input data contains zeros.
                   Replace zero values with a very small number to avoid
                   erroneous results. ...")
R/pclm_fit.R:45  K <- pclm_loop(..., y_, ...)      # zeros still raw
R/pclm_fit.R:53  y_[y_ == 0] <- 10^-4              # patched 8 lines too late
```

A `message()` is also easy to miss and does not fail `R CMD check`.

**Fix option.** Stabilize the denominators:

```cpp
dd = (y - muA).cwiseQuotient(y.cwiseMax(1.0)).cwiseAbs().mean() - d;
dd = (d > 0) ? std::abs(dd)/d : 0.0;
```

Also worth promoting the `message()` to `warning()`, or actually applying the
substitution before the call so the package follows its own advice.

**Numerically invisible (verified).** Early stopping does not change the fit.
Same input with zeros in 5 of 19 bins, fixed `lambda`, comparing `max.iter`
against the 1000-iteration result:

```
max.iter  10 : max abs diff 1.412e-07
max.iter  20 : max abs diff 7.838e-14
max.iter  50 : max abs diff 1.543e-14
max.iter 100 : max abs diff 5.418e-14
```

The iteration is converged to machine precision by iteration 20. So this fix
recovers roughly 98% of the wasted iterations and moves no reported digit.
Safe to apply without characterization tests.

---

## 3. API, documentation and tests

| ID  | Defect                                                                                                                                                                                                                                                                                                                                                       | Fix option                                                                                                                                                                                                                                                                                                                                                                | Status                                                                                                                                                                                                                                                   |
| --- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| M10 | Docs say `$bins.definition`, code sets `bin.definition`. Doc at `R/pclm_1D.R:65`, `man/pclm.Rd:70`, `man/pclm2D.Rd:69`; code sets it at `R/pclm_1D.R:195`, `R/pclm_2D.R:117` and reads it at 7 sites. `M$bins.definition` is `NULL` for anyone following the docs.                                                                                           | Correct the roxygen `@return` item. `pclm2D` inherits via `@inherit pclm return`, so one fix covers both. Regenerate `man/`.                                                                                                                                                                                                                                              | **Fixed.** Renamed in the roxygen `@return`; `pclm2D` inherits it.                                                                                                                                                                                       |
| M11 | `README.md:8` badge says `License-GPL v3`. The package is MIT (`DESCRIPTION:26`, `LICENSE`).                                                                                                                                                                                                                                                                 | Replace with an MIT badge. Wrong license advertising is a legal problem, not a cosmetic one.                                                                                                                                                                                                                                                                              | **Fixed.** Badge now `License-MIT`.                                                                                                                                                                                                                      |
| M12 | NEWS top entry is `version 1.4.0`; `DESCRIPTION` is `Version: 1.4.5`. Missing 1.1.7, 1.2.0, 1.3.0, 1.4.3, 1.4.4, 1.4.5.                                                                                                                                                                                                                                      | Add the missing entries.                                                                                                                                                                                                                                                                                                                                                  | **OPEN.** Needs the version bump and the two-word nickname, both gated behind explicit approval by the commit policy on this machine.                                                                                                                    |
| M13 | Top-level `expect_*` calls are not inside `test_that()`. When a file has any passing `test_that` block, testthat masks the top-level failures and the run still exits 0. About 20 assertions cannot fail CI.                                                                                                                                                 | Wrap `tests/testthat/test_pclm.R:62-87` and `tests/testthat/test_pclm2D.R:65-67` in `test_that()`. Proven by sabotaging `test_pclm.R:62`: `FAIL 1 \| PASS 68` still `EXIT=0`.                                                                                                                                                                                             | **Fixed.** 21 calls wrapped. Proven by inserting a deliberate failure in a file that also passes: it is now reported (was masked).                                                                                                                       |
| M14 | Two tests pass on the wrong error. `test_pclm2D.R:66` calls `pclm2D(x, y, nlast)` with undefined `y`, passing on "object 'y' not found". `:67` uses `rbind(offset, 0)` where `offset` resolves to `stats::offset`, so it dies on a closure coercion and never enters `pclm2D`.                                                                               | Rename to `y2` / `offset2` (the actual fixture names) and give every `expect_error` a message pattern. Without patterns the typo was invisible.                                                                                                                                                                                                                           | **Fixed.** `:66` now passes a real vector `y` (the intent) and `:67` uses `offset2`; every expectation carries a message pattern.                                                                                                                        |
| M15 | 13 exports have zero test coverage: `build_B_spline_basis`, `build_C_matrix`, `build_P_matrix`, `control.pclm`, `control.pclm2D`, `create.artificial.bin`, `delete.artificial.bin`, `frac`, `pclm.fit`, `pclm.input.check`, `seqlast`, `suggest.valid.out.step`, `validate.nlast`. No test pins a fitted value, AIC or BIC, though the fit is deterministic. | Add numeric regression tests (known input, known result) and edge cases (empty `y`, all-zero `y`, single bin, huge `out.step`). Highest value per line of test code in the package.                                                                                                                                                                                       | **Fixed for the public surface.** Golden pin on `fitted`/`AIC`/`BIC` plus mass conservation; `control.pclm`, `control.pclm2D` and `suggest.valid.out.step` covered. The remaining 10 are now internal (see M16), so they no longer need public coverage. |
| M16 | `pclm.fit` and `pclm.input.check` are exported but tagged `@keywords internal`, and `man/pclm.fit.Rd` says "This is an internal function".                                                                                                                                                                                                                   | Remove `@export`, keep `@keywords internal`, re-document. De-export in a minor release and note in NEWS. Also decide on `build_B_spline_basis`, `build_C_matrix`, `build_P_matrix`, `create.artificial.bin`, `delete.artificial.bin`, `frac`, `seqlast`, `validate.nlast`. Keep `suggest.valid.out.step`, which is named in a user-facing warning at `R/utils.R:111-116`. | **Fixed.** Exports 15 down to 5: `pclm`, `pclm2D`, `control.pclm`, `control.pclm2D`, `suggest.valid.out.step`. The 15 S3 registrations are untouched. Breaking change, so it belongs in the same release as M12.                                         |

---

## 4. Performance

| ID  | Defect                                                                                                                         | Fix option                                                                                                                                        | Status                                                                                                                                                                                                                    |
| --- | ------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| P1  | `R/pclm_CI.R:16` computes a full n x n matrix just to take its diagonal.                                                       | `rowSums((B %*% H1) * B)`. Measured **10x faster**, 10x less memory (n=400, kB=40), max abs diff `2.7e-12`.                                       | **Fixed.** In place. Golden pin held, so the numbers did not move.                                                                                                                                                        |
| P2  | `solve(QmQP)` is factorized twice: `R/pclm_fit.R:51` (`solve(QmQP, QmQ)`) and `R/pclm_CI.R:14` (`solve(QmQP)`).                | Factor once in `pclm.fit`, pass `H0` into `compute_standard_errors`.                                                                              | **Fixed.** `H0` computed once and passed down.                                                                                                                                                                            |
| P3  | `src/RcppEigenPclm.cpp:41` materializes a dense n x m outer product every iteration just to scale the `nnz(C)` entries of `C`. | Build `W` over `C`'s nonzero pattern with `InnerIterator`: `W = C; W.valueRef() *= muA_inv[it.row()] * mu[it.col()];` Preallocate `muA_inv` once. | **Fixed**, by a better route than suggested. `W` is never needed on its own, only `W B`, and `W B = diag(muA^-1) (C (diag(mu) B))`. One sparse times dense product and a row scaling replaces the outer product entirely. |
| P4  | `src/RcppEigenPclm.cpp:27` builds `VectorXd::Constant(kB, mua).array().exp().matrix()`, three kB temporaries for a scalar.     | `mu = std::exp(mua) * (B * VectorXd::Ones(kB));`                                                                                                  | **Fixed.** One scalar times the row sums of `B`.                                                                                                                                                                          |
| P5  | `householderQr()` on a symmetric positive (semi)definite system.                                                               | `Eigen::LDLT` on `selfadjointView<Eigen::Lower>()`, roughly half the factorization cost. Check `info()`.                                          | **Fixed.** `LDLT` with `info()` checked after factorization and after the solve. Golden pin held at `1e-6`, so the solver swap moved no reported digit.                                                                   |

---

## 5. Robustness

| ID  | Defect                                                                                                                                                                                                                                                                                                                 | Fix option                                                                                                                                                                                                                                                                                                               | Status                                                                                                                                                     |
| --- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| R1  | No dimension checks at the R/C++ boundary. R builds with `-DNDEBUG`, which sets `EIGEN_NO_DEBUG`, so a shape mismatch becomes an out-of-bounds read over R memory. Reachable through the exported `pclm.fit` with ordinary user input.                                                                                 | Guard at the top of `pclm_loop`: `if (C.rows() != y.size() \|\| C.cols() != B.rows() \|\| P.rows() != P.cols() \|\| P.rows() != B.cols()) Rcpp::stop(...)`. Also validate the `dgCMatrix` slots before mapping.                                                                                                          | **Fixed.** All four shape guards added at the top of `pclm_loop`.                                                                                          |
| R2  | No `R_CheckUserInterrupt()` in the hot loop (`src/RcppEigenPclm.cpp:40`). With `max.iter = 1000` inside `optimize_par`'s lambda sweeps and `pclm2D`'s per-column loop, a fit can run a long time uninterruptibly.                                                                                                      | `if (i % 32 == 0) Rcpp::checkUserInterrupt();` at the top of the loop body.                                                                                                                                                                                                                                              | **Fixed.** Checked every 32 iterations.                                                                                                                    |
| R3  | `maxiter` and `tol` are never validated. `NA_real_` or a negative `maxiter` makes `i < maxiter` false immediately, and the function silently returns pre-loop state including 0x0 `QmQ`/`QmQP`, which the caller then feeds to `solve()`. `maxiter > INT_MAX` overflows the signed `int` counter (undefined behavior). | Validate at entry: finite `maxiter` in `[1, INT_MAX]` and cast to `int`; finite `tol > 0`.                                                                                                                                                                                                                               | **Fixed.** Both bounded at entry; `maxiter` cast to `int` so the counter cannot overflow.                                                                  |
| R4  | `std::log(y.sum()/ny)` at `src/RcppEigenPclm.cpp:26` is `-Inf` for all-zero `y`, cascading to an all-`NaN` result returned with no diagnostic. `muA.cwiseInverse()` divides by `C*mu`, zero when a row of `C` is structurally zero. `eta.array().exp()` overflows near eta 709.                                        | Validate `y` at entry (finite, non-negative, `y.sum() > 0`); floor the weights (`max(muA_i, 1e-12)`); clamp `eta` before `exp`.                                                                                                                                                                                          | **Fixed.** Counts validated finite and non-negative with a positive total, weights floored at `1e-12`, linear predictor clamped at `+/- 700` before `exp`. |
| R5  | `Eigen::MappedSparseMatrix<double>` at `src/RcppEigenPclm.cpp:16` is deprecated since Eigen 3.3.                                                                                                                                                                                                                       | Pure type rename to `Eigen::Map<Eigen::SparseMatrix<double>>` in `RcppEigenPclm.cpp`, then regenerate `RcppExports` with `Rcpp::compileAttributes()` (do not hand-edit the generated file). Compiles fine today on RcppEigen 0.3.4.0.2, so this is forward-looking, not urgent. `pr18-local` already carries the change. | **Fixed.** Renamed, and `RcppExports` regenerated from source rather than hand-edited.                                                                     |
| R6  | `asSparseMat` (`src/RcppEigenPclm.cpp:10`) silently accepts a plain vector as an n x 1 matrix, since the exporter defaults to one column. Shape mistakes are reinterpreted and surface later as dimension mismatches.                                                                                                  | Require a matrix at entry (`Rf_isMatrix` or `const Rcpp::NumericMatrix&`).                                                                                                                                                                                                                                               | **Fixed.** Takes `SEXP` and rejects a non-matrix with a package message.                                                                                   |
| R7  | Unsorted or gapped `x` is not rejected with strict monotonicity. `[reported]` malformed bins return a distribution losing about 10% of observed mass with no warning.                                                                                                                                                  | `if (is.unsorted(x, strictly = TRUE)) stop(...)`.                                                                                                                                                                                                                                                                        | **Fixed.** Placed after the length checks so the existing length-mismatch expectations keep their message.                                                 |

---

## 6. Docs and metadata

| ID  | Defect                                                                                                                                                                                                             | Fix option                                                                                                                                                                                    | Status                                                                                                                                                                                                                                                                            |
| --- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| D1  | `man/BIC.pclm2D.Rd:5` and `R/pclm_CI.R:142` title it "PCLM-2D **Akaike** Information Criterion". Its sibling `BIC.pclm` says Bayes. `@inherit AIC.pclm` also gives it AIC prose.                                   | Retitle to "PCLM-2D Bayes Information Criterion" and stop inheriting from `AIC.pclm`.                                                                                                         | **Fixed.** Retitled; now inherits from `BIC.pclm`.                                                                                                                                                                                                                                |
| D2  | `R/pclm_optim.R:90` names `'pclm2D.control'`. The real function is `control.pclm2D`.                                                                                                                               | Correct the name in the user-facing warning.                                                                                                                                                  | **Fixed.**                                                                                                                                                                                                                                                                        |
| D3  | `R/pclm_graphics.R:174` passes two string literals to `warning()`, which concatenates with **no separator** (verified: `warning('AAA','BBB')` gives `AAABBB`). Renders as `` `offset`have different dimensions. `` | Merge into one string, or paste with an explicit space.                                                                                                                                       | **Fixed.** Single `paste0` string.                                                                                                                                                                                                                                                |
| D4  | `man/pclm.input.check.Rd` documents `X` but not `pclm.type`, and `@inheritParams pclm.fit` supplies nothing for it.                                                                                                | Add `@param pclm.type` ("1D" or "2D").                                                                                                                                                        | **Fixed.** Documented with the shape rules it selects.                                                                                                                                                                                                                            |
| D5  | `DESCRIPTION:28` declares `Depends: R (>= 3.4.0)`, but RcppEigen 0.3.4.x declares `Depends: R (>= 3.6.0)`. The dependency chain is unsatisfiable on R 3.4/3.5.                                                     | Bump to `R (>= 3.6.0)`, or the lowest version actually exercised in CI.                                                                                                                       | **Fixed.** `R (>= 3.6.0)`.                                                                                                                                                                                                                                                        |
| D6  | `.travis.yml` targets a defunct CI service and still installs `libfreetype6-dev` / `libftgl-dev` / Mesa, leftovers from the pre-1.4.0 `rgl` dependency that NEWS says was dropped.                                 | Replace with GitHub Actions via `r-lib/actions`, or delete.                                                                                                                                   | **Fixed.** Travis deleted and `.github/workflows/R-CMD-check.yaml` added. **Unverified** here, it needs a push to exercise. Dead `^\.travis\.yml$` and `^appveyor\.yml$` build-ignore entries removed too.                                                                        |
| D7  | Unused imports: `Rcpp::sourceCpp` (never called), `graphics::par` (no call site), `stats::quantile` (no call site), `graphics::abline` (only a commented line at `R/pclm_graphics.R:91`).                          | Remove the `@importFrom` tags and re-document. **Caveat:** confirm `importFrom(stats, AIC)` and `importFrom(stats, BIC)` are removable without breaking S3 registration before dropping them. | **Fixed.** `quantile`, `abline`, `par` removed after confirming zero call sites with a word-boundary grep. `sourceCpp` swapped for `evalCpp` rather than deleted: the directive is what loads the Rcpp DLL at runtime, so it cannot go entirely. `AIC`/`BIC` kept per the caveat. |
| D8  | `R/pclm_CI.R:112,134` use `ls(object) == "deep"` where `names(object)` is intended. Works via `as.environment` coercion, but reads as an error.                                                                    | `if ("deep" %in% names(object))`.                                                                                                                                                             | **Fixed.** Both call sites.                                                                                                                                                                                                                                                       |
| D9  | The recursive `pclm2D` call at `R/pclm_2D.R:89` forwards `I$nlast`, `out.step`, `ci.level`, `control` positionally past named formals. Correct today only because the order happens to match.                      | Name all four. Fragile under any formal reordering.                                                                                                                                           | **Fixed.** All eight arguments named.                                                                                                                                                                                                                                             |
| D10 | `goodness.of.fit$standard.errors` is an odd home for the standard errors (they are not a goodness-of-fit measure). `ci` lists `upper` before `lower`.                                                              | Cosmetic. Consider `se` at top level and `lower, upper` order.                                                                                                                                | **Partly.** Test helpers now read `ci` by name rather than by index. The element order and the `SE` location are left alone: both are public, and moving them for cosmetics would silently change what `[[1]]` returns for existing callers.                                      |
| D11 | `DESCRIPTION:42` has a trailing space after `RdMacros:`.                                                                                                                                                           | Trim.                                                                                                                                                                                         | **Fixed.**                                                                                                                                                                                                                                                                        |
| D12 | `man/pclm.Rd:120-124`: example 2 comment says "ungroup even in smaller intervals" but the following lines use `M1`, not `M2`.                                                                                      | Use `M2` in those two lines.                                                                                                                                                                  | **Fixed.**                                                                                                                                                                                                                                                                        |
| D13 | Several roxygen blocks fuse the description into the title (multiline `\title` in the Rd): `R/pclm_2D.R:171-173`, `R/pclm_fit.R:94-96`, `R/pclm_optim.R:38-39`.                                                    | Blank roxygen line plus `@description`.                                                                                                                                                       | **Fixed.** All three split.                                                                                                                                                                                                                                                       |
| D14 | `residuals.pclm2D` example runs 6.27s, the only CRAN NOTE.                                                                                                                                                         | `\donttest{}` or a smaller example.                                                                                                                                                           | **Fixed**, after one false start. Trimmed to 10 columns of 35; the NOTE is gone. **My first attempt used 5 columns and broke the example**, see the corrections below.                                                                                                            |

---

## 7. Confirmed healthy

Recorded so nobody re-audits these.

- `R CMD check --as-cran` is clean apart from one slow-example NOTE.
- `man/` is consistent with source: 41 `.Rd` files match 41 documented roxygen blocks 1:1. No orphan or stale topics.
- `inst/CITATION` parses and points at the JOSS paper correctly.
- `joss/`, `data-raw/`, `.travis.yml` are correctly excluded from the build.
- `RcppExports` contract is clean: `src/RcppExports.cpp` and `R/RcppExports.R` carry the same generator token, signatures and arities match, and `R_registerRoutines` + `R_useDynamicSymbols(FALSE)` are present. No hand edits to generated files.
- `useDynLib(ungroup)` without `.registration = TRUE` is fine. Registration happens in C.
- `importClassesFrom(Matrix, dgCMatrix)` is justified: `asSparseMat` returns a wrapped `dgCMatrix`.
- Unknown `control` names fail loudly (`do.call(control.pclm, list(maxiter=500))` errors with "unused argument"). Not a silent-accept path.
- `pclm.input.check`'s `kr <= 0 || frac(kr) != 0` guard is correct for negative input: `||` short-circuits before `frac` runs.
- `ofun` and `optimize_par` are documented but **not** exported (verified at runtime).
- No matrix `inverse()` anywhere in the compiled layer. No OpenMP, no threading, no R API calls inside the loop.
- All source files are well under the 800-line ceiling. Largest is `R/pclm_1D.R` at 286 lines.

---

## 8. What is left

**M12**, the NEWS ledger, is the only open finding. It needs the version bump
and the two-word nickname that the commit policy on this machine gates behind
explicit approval, and the nickname is the owner's to choose. M16 is a breaking
change and belongs in the same release.

One other decision: regenerating `man/` rewrapped `NAMESPACE` (semantically
identical) and replaced `RoxygenNote: 7.3.0` with
`Config/roxygen2/version: 8.1.0`, a field rename older tooling may not read.
That churn is separable into a commit of its own.

Everything else is fixed, or in M1's case deliberately documented rather than
changed.

---

## 9. Claims that were tested and dropped

Recorded for honesty. Each looked like a defect and is not.

| Claim                                                                                | Why it was dropped                                                                                                                                                                                                                |
| ------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `frac(kr) != 0` is wrong for negative input                                          | `kr <= 0` short-circuits `\|\|` first. `frac` is never evaluated. The guard is correct.                                                                                                                                           |
| Misspelled `control` names are silently accepted                                     | `do.call(control.pclm, list(maxiter=500))` errors: "unused argument".                                                                                                                                                             |
| Missing `src/Makevars` blocks CRAN compilation                                       | It does not. `LinkingTo` supplies the RcppEigen include path, and RcppEigen's own skeleton `Makevars` ships fully commented out. The layout compiles.                                                                             |
| `ci` silently drops `fit` and `SE`                                                   | It does not. `R$fit` becomes `fitted` and `R$SE` becomes `goodness.of.fit$standard.errors` (`R/pclm_1D.R:183-188`). Redistribution, not loss.                                                                                     |
| `I$ny <- length(y)` is wrong for a data.frame                                        | `length()` on a data.frame returns the column count, which is what is wanted here. Correct as written.                                                                                                                            |
| `ofun` / `optimize_par` are wrongly exported                                         | They are documented but not exported.                                                                                                                                                                                             |
| `is.array(y)` in `pclm.input.check` contradicts its own acceptance of `is.matrix(y)` | True of the function in isolation, but unreachable through `pclm2D`, which coerces matrix to data.frame at `R/pclm_2D.R:74` before the check runs at line 82. Only reachable by calling the exported `pclm.input.check` directly. |

Two of these were my own premises, disproved by evidence rather than defended.

---

## 10. Provenance and verification

```
verified by direct probe (orchestrator)
  B1  crash + 47/91 sweep
  M1  60/111 inverted, 64/111 estimate outside interval
  M2  R argument-matching semantics
  M3  missing formal
  M4  lambda = c(1, NA) abort
  M6  lambda = 0 and Inf -> 111/111 NaN
  M7  Inf clears both guards
  M8  integer y -> "Wrong R type for mapped vector"
  M9  0s vs 1.64s at 200k iterations
  P1  10x benchmark
  D3  warning() concatenation
  M10 runtime object name is bin.definition

reported by reviewer slice, evidence cited, not independently re-run
  B2, M5, M7 (downstream NaN), R1 to R7, D1 to D14, M13 to M16 details
```

Review artifacts: two stray files created by test and probe runs during the
review (`tests/testthat/Rplots.pdf`, empty `tests/testthat/_snaps/`) were
removed. At that point the working tree matched commit `2423c19` exactly. The
fixes applied since are marked against their own findings above.

This report is a review artifact only. `.Rbuildignore` line 13 (`^doc$`)
keeps `doc/` out of the package build, so its presence does not affect
`R CMD check`.

---

## 11. Corrections during the fixes

Two corrections to my own reasoning, both caught by running the check rather
than trusting a theory:

- **The prediction about `k` on `BIC.pclm` was wrong.** I expected it to fail
  `checking S3 generic/method consistency`. That check reports OK either way.
  The change stands on the silent-drop ground, not on the one I gave.
- **The first D14 fix broke the build.** Shrinking the `residuals.pclm2D`
  example to 5 columns made it die in `MortSmooth_bbase` with `from` not finite.
  `control.pclm2D` defaults to `kr = 7`, and below about 8 columns there are no
  usable knots. Measured the cliff (5 fails, 8 works) and settled on 10.
  `checking tests ... OK` had hidden it, because the suite used 21 columns.

One incidental defect fell out of the work and went in with the rest: the 2D
branch of `pclm.confidence.dx()` did not return `qn`, so `pclm2D()` aborted
while computing the new intervals. All three return paths carry it now.

Final state: `R CMD check --as-cran` gives **Status: OK** with no NOTE, and the
suite runs 27 blocks with 0 failures, 0 errors, 89 passing expectations.
