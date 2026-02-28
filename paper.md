---
title: "MultiSpline: Fast and Flexible Nonlinear Multilevel Modeling in R"
tags:
  - R
  - splines
  - multilevel modeling
  - nonlinear regression
  - longitudinal data
authors:
  - name: Subir Hait
    orcid: 0009-0004-9871-9677
    affiliation: 1
affiliations:
  - name: Michigan State University
    index: 1
date: 27 February 2026
bibliography: paper.bib
---

# Summary

`MultiSpline` is an R package that provides a unified interface for fitting,
predicting, and visualizing nonlinear relationships in single-level,
multilevel, and longitudinal regression models using spline-based methods.
The package wraps natural cubic spline basis construction and mixed-effects
estimation into five high-level functions — `nl_fit()`, `nl_summary()`,
`nl_predict()`, `nl_plot()`, and `nl_icc()` — enabling researchers to go
from raw data to publication-ready nonlinear prediction curves and intraclass
correlation estimates in only a few lines of code. The source code is
available at <https://github.com/causalfragility-lab/MultiSpline> under an
MIT license and is also distributed via CRAN.

# Statement of Need

Nonlinear covariate effects — such as diminishing returns of socioeconomic
status (SES) on academic achievement, or saturating dose-response
relationships in health research — are common in clustered and longitudinal
data. Although packages such as `lme4` [@bates2015] and `mgcv` [@wood2017]
support mixed-effects and additive modeling respectively, neither provides
an end-to-end workflow for the specific task of fitting a nonlinear spline
effect within a multilevel model and extracting interpretable predictions
and uncertainty bands across time. Users of `lme4` must manually construct
spline basis matrices, build interaction terms with time, compute prediction
grids, and write custom code for visualization. Users of `mgcv` must
navigate penalized smoothing penalties, basis selection, and a different
random-effects syntax — presenting a steep learning curve for applied
researchers. `MultiSpline` closes this gap by automating the full pipeline
within a single, consistent interface while using `lme4` as its estimation
backend, ensuring statistical rigor and compatibility with the broader R
ecosystem.

The package is particularly suited to large-scale educational and social
datasets (e.g., ECLS-K, HSLS, PISA) where students are nested within
schools and repeatedly assessed over time, and where SES gradients or growth
trajectories are theoretically expected to be nonlinear.

# Mathematical Formulation

The core model estimated by `MultiSpline` is:

$$Y_{ijt} = \beta_0 + f(X_{ijt}) + \gamma_t + f_t(X_{ijt}) +
\mathbf{Z}_{ijt}^{\top}\boldsymbol{\delta} + u_j + v_i + \epsilon_{ijt}$$

where $f(X)$ is a nonlinear function of the focal predictor estimated via
natural cubic splines [@hastie1990]; $\gamma_t$ represents wave-specific
intercepts; $f_t(X)$ allows the nonlinear effect to vary across time
through spline-by-time interactions; $\mathbf{Z}_{ijt}$ is a vector of
additional covariates with coefficients $\boldsymbol{\delta}$;
$u_j \sim \mathcal{N}(0, \sigma^2_j)$ and
$v_i \sim \mathcal{N}(0, \sigma^2_i)$ are school- and student-level random
intercepts; and $\epsilon_{ijt} \sim \mathcal{N}(0, \sigma^2)$ is residual
error.

Intraclass correlation coefficients (ICC) are computed as:

$$
\text{ICC}_j = \frac{\sigma^2_j}{\sigma^2_j + \sigma^2_i + \sigma^2},
\qquad
\text{ICC}_i = \frac{\sigma^2_i}{\sigma^2_j + \sigma^2_i + \sigma^2}
$$

# Example

The following example demonstrates the complete workflow on simulated
three-level longitudinal data:

```r
library(MultiSpline)

# Simulate a three-level longitudinal dataset
set.seed(42)
d <- data.frame(
  schid      = rep(1:10, each = 60),
  id         = rep(1:200, each = 3),
  TimePoint  = factor(rep(1:3, times = 200)),
  SES        = rnorm(600),
  math_score = rnorm(600, mean = 50, sd = 10)
)

# Fit nonlinear multilevel model
fit <- nl_fit(
  data    = d,
  y       = "math_score",
  x       = "SES",
  time    = "TimePoint",
  cluster = c("id", "schid"),
  method  = "ns",
  df      = 4
)

nl_summary(fit)          # tidy coefficient table
nl_icc(fit)              # intraclass correlations
pred <- nl_predict(fit)  # predictions with 95% CI
nl_plot(pred, x = "SES", time = "TimePoint")  # visualization
```

# Performance and Validation

## Computational Performance

`MultiSpline` was benchmarked against an equivalent direct `lme4`
specification using the `microbenchmark` package [@mersmann2023] on a
simulated three-level dataset ($N = 9{,}000$ observations). Post-estimation
utilities (`nl_predict`, `nl_icc`) are computationally negligible.

| Function        | Median time (ms) |
|:----------------|-----------------:|
| `nl_fit()`      | 308.3            |
| `lme4` direct   | 262.4            |
| `nl_predict()`  | 5.0              |
| `nl_icc()`      | 1.2              |

The modest overhead of `nl_fit()` relative to direct `lme4` usage (45.9 ms)
reflects the automated construction of spline basis matrices, formula
assembly, and object packaging — steps that would otherwise require
manual coding by the user.

## ICC Validation

To validate variance decomposition, we simulated datasets with known ICC
values and compared `nl_icc()` estimates to ground truth across 100
replications:

| Level    | True ICC | Estimated ICC | Absolute bias |
|:---------|---------:|--------------:|--------------:|
| School   | 0.15     | 0.162         | 0.012         |
| Student  | 0.35     | 0.333         | 0.017         |
| Residual | 0.50     | 0.505         | 0.005         |

The maximum absolute bias across levels was 0.017, indicating accurate
recovery of the three-level variance structure.

## Software Testing

The package includes a test suite implemented with `testthat`
[@wickham2011] covering all five core functions. Tests verify correct
output structure, expected column names, ICC bounds, and behavior under
edge cases including single-level and missing-time specifications. The
test suite is run automatically on each commit via GitHub Actions.

# Acknowledgements

The author thanks the open-source R community, and the developers of
`lme4` [@bates2015], `splines`, and `ggplot2` [@wickham2016] whose work
forms the computational foundation of `MultiSpline`.

# References
