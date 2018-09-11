---
title: '`ungroup`: An R package for efficient estimation of smooth distributions from
  coarsely binned data'
tags:
- composite link model
- GLM
- histogram
- binned data
- smoothing
authors:
- name: Marius D. Pascariu
  affiliation: 1
  email: mpascariu@health.sdu.dk
  orcid: 0000-0002-2568-6489
- name: Maciej J. Dańko
  affiliation: 3
  orcid: 0000-0002-7924-9022
- name: Jonas Schöley
  affiliation: 1
  orcid: 0000-0002-3340-8518
- name: Silvia Rizzi
  affiliation: 2
affiliations:
- name: Institute of Public Health, Center on Population Dynamics, University of Southern Denmark, Odense, Denmark
  index: 1
- name: Institute of Public Health, Unit of Epidemiology Biostatistics and Biodemography, University of Southern Denmark, Odense, Denmark
  index: 2
- name: Max Planck Institute for Demographic Research, Rostock, Germany
  index: 3
date: "10 September 2018"
bibliography: paper.bib
---

``ungroup`` is an open source software library written in the ``R`` programming language [@team2018r] that introduces a versatile method for ungrouping histograms (binned count data) assuming that counts are Poisson distributed and that the underlying sequence over a fine grid to be estimated is smooth. The method is based on the composite link model [@thompson1981] and estimation is achieved by maximizing a penalized likelihood [@eilers2007], which extends standard generalized linear models. The penalized composite link model (PCLM) implements the idea that observed counts, interpreted as realizations from Poisson distributions, are indirect observations of a finer (ungrouped) but latent sequence. This latent sequence represents the distribution of expected means on a fine resolution and has to be estimated from the aggregated data. Estimates are obtained by maximizing a penalized likelihood. This maximization is performed efficiently by a version of the iteratively re-weighted least-squares algorithm. Optimal values of the smoothing parameter are chosen by minimizing Bayesian or Akaike's Information Criterion [@hastie1990].

Ungrouping binned data can be desirable for many reasons: Bins can be too coarse to allow for accurate analysis; comparisons can be hindered when different grouping approaches are used in different histograms; the last interval may be wide and open-ended masking the tail behaviour of the underlying distribution. Age-at-death distributions grouped into age classes and abridged life tables are examples of binned data which can be ungrouped with the package ``ungroup``. The modest assumptions of the methodology underpinning the PCLM method make it suitable for many demographic and epidemiological applications. For a detailed description of the method and applications see @rizzi2015 and @rizzi2016.

![Ungrouping of the age-at-death distribution and estimating age-specific death rates. The original death counts and exposures taken from the @hmd2018 using the ``MortalityLaws`` R package [-@MortalityLaws160] were grouped in 5-year bins plus a wide class for ages 85+. In each panel, the original aggregated data is compared with smoothly estimated values.](figures/pclm1D.pdf)

The penalized composite link model can be extended to a two-dimensional regression problem [@rizzi2018]. The two-dimensional regression analysis combines two approaches: the PCLM for ungrouping in one dimension and two-dimensional smoothing with P-splines [@currie2004]. As an example one can ungroup age-specific distributions from the coarsely grouped data and smooth across adjacent calendar years to estimate both detailed age-at-death distributions and mortality time trends.

![Two-dimensional ungrouping of the age-at-death distributions and mortality surface. The 3-D figures are generate using the ``rgl`` R package [-@rgl09916].](figures/pclm2D.pdf)


# Acknowledgments
We thank Paul H.C. Eilers who provided insight and expertise that greatly supported the creation of this R package; and Catalina Torres and Tim Riffe for testing and offering feedback on the early versions of the software. 

The authors are grateful to the following institutions for their support:

 * University of Southern Denmark;
 * Max Planck Institute for Demographic Research;
 * The SCOR Corporate Foundation for Science.

# References
