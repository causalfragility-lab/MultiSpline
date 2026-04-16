## R CMD check results

0 errors | 0 warnings | 0 notes

Tested on:
- Windows 11 x64, R 4.5.1 (primary development platform)
- Ubuntu 24.04 LTS, R 4.3.3 (CI)

## Resubmission notes

This is a new submission (v0.2.0 is the first CRAN release; v0.1.0 existed
only as a source package distributed directly).

## Notes on suggested packages

* `reformulas`: used opportunistically for `nobars()` inside `nl_r2()` and
  `nl_predict()`; the package gracefully falls back to `lme4::nobars()` when
  `reformulas` is unavailable. No functionality is lost.

* `lmerTest`: used optionally in `nl_summary()` to provide Satterthwaite
  degrees of freedom and p-values for LMM coefficient tables. When absent,
  the table is returned without p-values.

* `numDeriv`: listed for potential future use in derivative computation.
  Not currently called by any exported function.

## Package purpose

MultiSpline provides a unified workflow for fitting, interpreting, and
visualising nonlinear multilevel spline models. It wraps lme4, mgcv, and
splines into a consistent interface aimed at applied researchers in
epidemiology, education, and developmental science.
