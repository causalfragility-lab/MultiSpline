pkgname <- "MultiSpline"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "MultiSpline-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('MultiSpline')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("nl_fit")
### * nl_fit

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nl_fit
### Title: Fit a nonlinear (spline or GAM) single-level or multilevel model
### Aliases: nl_fit

### ** Examples

## Not run: 
##D # Single-level natural spline
##D fit_sl <- nl_fit(data = mydata, y = "outcome", x = "age", df = 4)
##D 
##D # Single-level GAM
##D fit_gam <- nl_fit(
##D   data   = mydata,
##D   y      = "outcome",
##D   x      = "age",
##D   method = "gam",
##D   k      = 5
##D )
##D 
##D # Multilevel spline with random intercepts
##D fit_ml <- nl_fit(
##D   data    = mydata,
##D   y       = "outcome",
##D   x       = "age",
##D   cluster = "id",
##D   df      = 4
##D )
##D 
##D # Spline with time interaction and controls
##D fit_t <- nl_fit(
##D   data     = mydata,
##D   y        = "outcome",
##D   x        = "age",
##D   time     = "wave",
##D   controls = c("sex", "baseline_score"),
##D   df       = 4
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nl_fit", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nl_icc")
### * nl_icc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nl_icc
### Title: Intraclass correlation coefficients for a multilevel nl_fit
###   model
### Aliases: nl_icc

### ** Examples

## Not run: 
##D fit <- nl_fit(
##D   data    = mydata,
##D   y       = "math_score",
##D   x       = "SES",
##D   cluster = c("id", "schid"),
##D   df      = 4
##D )
##D nl_icc(fit)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nl_icc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
