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
date: 2026
bibliography: paper.bib
---

# Summary
In hierarchical or longitudinal data structures, nonlinear relationships
are very important in social science research. However, when using
mixed-effects models for this kind of data — especially when effects vary
over time or across levels — inclusion of nonlinear terms is often
difficult and requires substantial coding work.

To address these challenges, the `MultiSpline` R package provides a simple and
unified interface for estimating nonlinear relationships using spline-based
methods within both standard and multilevel models. It allows users to fit
models, generate predictions, compute uncertainty, and estimate intraclass
correlation coefficients (ICC) with minimal code. The source code is
available at [https://github.com/causalfragility-lab/MultiSpline](https://github.com/causalfragility-lab/MultiSpline) under an
MIT license.
 

# Statement of Need
 In social and health science research, nonlinear effects such as SES gradients or dose-response 
 relationships are very common in clustered or longitudinal data. Existing packages 
 like `lme4` [@bates2015]  and `mgcv` [@wood2017] are powerful but require users to manually
 write spline terms, set up interaction structures, and handle prediction 
 and visualization separately. This creates additional burden and time cost for applied researchers.  
 
To address these challenges, `MultiSpline` provides five core functions:
`nl_fit()` for model fitting, `nl_summary()` for coefficient output,
`nl_predict()` for prediction, `nl_plot()` for visualization, and
`nl_icc()` for intraclass correlation. This unified workflow reduces
coding burden and improves reproducibility.


# Mathematical Formulation

The core model estimated by `MultiSpline` can be written as:

$$Y_{ijt} = f(X_{ijt}) + \gamma_t + f_t(X_{ijt}) + u_j + v_i +
\epsilon_{ijt}$$

where $f(X)$ is a nonlinear function of the predictor estimated via natural
cubic splines [@hastie1990], $\gamma_t$ represents time effects, $f_t(X)$
allows nonlinear effects to vary over time, $u_j \sim N(0, \sigma^2_j)$ and
$v_i \sim N(0, \sigma^2_i)$ are cluster-level random effects, and
$\epsilon_{ijt} \sim N(0, \sigma^2)$ is the residual error.

The intraclass correlation coefficients (ICC) for school level $j$ and
student level $i$ are defined as:

$$
ICC_j = \frac{\sigma^2_j}{\sigma^2_j + \sigma^2_i + \sigma^2}, \quad
ICC_i = \frac{\sigma^2_i}{\sigma^2_j + \sigma^2_i + \sigma^2}
$$

# Example

The following example demonstrates the core workflow using simulated data:
```r
library(MultiSpline)

# Simulate a three-level longitudinal dataset
set.seed(42)
n_schools  <- 10
n_students <- 20
n_times    <- 3

d <- data.frame(
  schid      = rep(1:n_schools,
                   each = n_students * n_times),
  id         = rep(1:(n_schools * n_students),
                   each = n_times),
  TimePoint  = factor(rep(1:n_times,
                   times = n_schools * n_students)),
  SES        = rnorm(n_schools * n_students * n_times),
  math_score = rnorm(n_schools * n_students * n_times,
                     mean = 50, sd = 10)
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

# coefficient table
nl_summary(fit)

# Intraclass correlations
nl_icc(fit)

# Generate predictions
pred <- nl_predict(fit)

# Visualize nonlinear effects
nl_plot(pred, x = "SES", time = "TimePoint")
```

# Performance and Validation

## Computational Performance

We compared `MultiSpline` to a direct `lme4` implementation using equivalent
model specifications on the simulated dataset above. Benchmarks were obtained
using the `microbenchmark` package [@mersmann2023].

| Function      | Median Time (ms) |
|:------------- | ----------------:|
| `nl_fit`      | 308.3            |
| `lme4` direct | 262.4            |
| `nl_predict`  | 5.0              |
| `nl_summary`  | 328.5            |
| `nl_icc`      | 1.2              |

The results show that `MultiSpline` achieves comparable computational
performance to direct `lme4` usage while providing a significantly simpler
interface and integrated workflow.

## ICC Validation

To validate variance decomposition, we simulated data with known ICC values
and compared estimated ICCs to ground truth:

| Level    | True ICC | Estimated ICC |
|:-------- | --------:| -------------:|
| School   | 0.15     | 0.162         |
| Student  | 0.35     | 0.333         |
| Residual | 0.50     | 0.505         |

The estimated intraclass correlations closely match the true values, with a
maximum absolute deviation of 0.017,indicating accurate recovery
of variance components. `MultiSpline` builds on spline-based
modeling approaches [@hastie1990; @wood2017] and mixed-effects models
[@bates2015; @raudenbush2002].

# References
