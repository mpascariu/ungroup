# <img src="man/figures/logo.png" align="right" width="150" height="150" alt="ungroup logo" /> ungroup: estimating smooth distributions from coarsely binned data

[![CRAN status](https://www.r-pkg.org/badges/version/ungroup)](https://CRAN.R-project.org/package=ungroup)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/ungroup)](https://CRAN.R-project.org/package=ungroup)
[![CRAN downloads total](https://cranlogs.r-pkg.org/badges/grand-total/ungroup)](https://CRAN.R-project.org/package=ungroup)
[![R-CMD-check](https://github.com/mpascariu/ungroup/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mpascariu/ungroup/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/mpascariu/ungroup/actions/workflows/pkgdown.yaml/badge.svg)](https://mpascariu.github.io/ungroup/)
[![codecov](https://codecov.io/github/mpascariu/ungroup/branch/master/graphs/badge.svg)](https://app.codecov.io/github/mpascariu/ungroup)
[![issues](https://img.shields.io/github/issues-raw/mpascariu/ungroup.svg)](https://github.com/mpascariu/ungroup/issues)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![JOSS](https://joss.theoj.org/papers/10.21105/joss.00937/status.svg)](https://doi.org/10.21105/joss.00937)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/mpascariu/ungroup/blob/master/LICENSE)

`ungroup` recovers a smooth, detailed distribution from counts that were
published in wide bins. It implements the penalized composite link model
(PCLM), which treats the coarse counts as indirect observations of a smooth
underlying sequence and estimates that sequence by penalized maximum
likelihood. It also extends the same idea to two dimensions, so a sequence of
coarsely grouped distributions can be ungrouped and smoothed across time at
once.

The problem it solves shows up wherever data arrive pre-aggregated.
Hospitals publish deaths in five-year age groups. Registries report cases by
ten-year cohort. Vital statistics close the oldest age group at 85+, sometimes
at 90+, with a single wide interval holding a large share of the total. In
every case the fine-grained distribution you need is inside the bins you were
given, and taking the bins at face value is not good enough for the questions
people actually ask of the data.

The naive alternative, spreading each bin's count evenly over its width,
imposes a flat step on every interval. It is not merely inelegant, it is
biased: in the Swedish data shipped with the package, that approach
overstates life expectancy at birth by an average of 2.5 years. PCLM recovers
the same distribution to within a tenth of a year, and it redistributes
exactly the deaths it was given.

![Ungrouping the age-at-death distribution of Swedish males in 1980, and the age-specific death rates the same fit implies. The left panel compares the coarse input bins, in grey, with the smooth single-year estimate, in red. The right panel puts the implied death rates on a log scale.](man/figures/README-ungrouping.png)

## What it does

| capability | how |
|---|---|
| Ungroup counts onto a finer grid | `pclm()`, with any output width from 0.1 to 1 |
| Ungroup a surface over time | `pclm2D()`, for a matrix of years by age |
| Estimate rates instead of counts | pass an `offset` of exposures |
| State where the distribution closes | `omega` instead of the width of the last interval |
| Smooth over unobserved cells | `na.action = "omit"` |
| Quantify uncertainty | pointwise intervals, and mass-preserving scenarios for life tables |

## Installation

```r
install.packages("ungroup")
```

The development version comes from GitHub. `pak` is the recommended
installer, and it needs a working development environment: Rtools on Windows,
Xcode on macOS, a compiler on Linux.

```r
# install.packages("pak")
pak::pak("mpascariu/ungroup")
```

`pak::pak()` also installs the compiled dependencies this package links
against, so it is the simplest route on a fresh machine.

## Documentation

The tutorial is the place to start, and it works through the model, the
smoothing parameter, missing data and how to read every part of the output:

- **Tutorial:** <https://mpascariu.github.io/ungroup/articles/Intro.html>
- **Function reference:** <https://mpascariu.github.io/ungroup/reference/index.html>
- **Offline:** `vignette("Intro", package = "ungroup")`

The JOSS paper gives a short overview of the method and its motivation:
Pascariu et al. (2018), *Journal of Open Source Software*, 3(29), 937,
<https://doi.org/10.21105/joss.00937>.


## Method

The estimator is the penalized composite link model of Eilers (2007), built on
the composite link model of Thompson and Baker (1981). The one-dimensional
case follows Rizzi, Gampe and Eilers (2015); the two-dimensional extension
with smoothing across adjacent years follows Rizzi et al. (2019). Smoothing
parameters are chosen by BIC or AIC.

## Citation

```r
citation("ungroup")
```

Pascariu, M. D., Dańko, M. J., Schöley, J., and Rizzi, S. (2018). ungroup: An
R package for efficient estimation of smooth distributions from coarsely
binned data. *Journal of Open Source Software*, 3(29), 937.
<https://doi.org/10.21105/joss.00937>

## Contributing

Issues and pull requests are welcome. If `ungroup` misbehaves, please open an
issue with a minimal reproducible example. See
[CONTRIBUTING.md](https://github.com/mpascariu/ungroup/blob/master/CONTRIBUTING.md).
This project is released with a
[Contributor Code of Conduct](https://github.com/mpascariu/ungroup/blob/master/CODE_OF_CONDUCT.md).

## References

Eilers, P. H. C. (2007). Ill-posed problems with counts, the composite link
model and penalized likelihood. *Statistical Modelling*, 7(3), 239-254.
<https://doi.org/10.1177/1471082X0700700302>

Rizzi, S., Gampe, J., and Eilers, P. H. C. (2015). Efficient estimation of
smooth distributions from coarsely grouped data. *American Journal of
Epidemiology*, 182(2), 138-147. <https://doi.org/10.1093/aje/kwv020>

Rizzi, S., Halekoh, U., Thinggaard, M., Engholm, G., Christensen, N.,
Johannesen, T. B., and Lindahl-Jacobsen, R. (2019). How to estimate mortality
trends from grouped vital statistics. *International Journal of
Epidemiology*, 48(2), 571-582. <https://doi.org/10.1093/ije/dyy183>

Thompson, R. and Baker, R. J. (1981). Composite link functions in generalized
linear models. *Applied Statistics*, 30(2), 125-131.
