# nolint start

#+ setup
library("rmo")
library("magrittr")
library("ggplot2")
library("microbenchmark")

#+ r parameter
## We use a compound Poisson process with intensity `0` and fixed
## deterministic jumps of size `eta`. Additionally, we allow killing
## and a drift. This model is convinient, as it can be used to parametrise
## all used models from this package.
use_seed <- 1632L
n <- 1e4L
d <- 6L

rate <- 0
eta <- 1 # dummy
rate_killing <- 0.1 / 0.4
rate_drift <- 0.3 / 0.4

psi <- function(x) {
  ifelse(x > 0, rate_killing, 0) + rate_drift * x +
    rate * (1 - exp(-eta * x))
}

## overwrite to compensate numerical problems
ex_intensities <- numeric(d)
ex_intensities[1] <- rate_drift
ex_intensities[d] <- rate_killing

intensities <- numeric(2^d-1)
for (j in 1:(2^d-1)) {
  tmp <- 0
  for (i in 1:d) {
    tmp <- tmp + rmo:::is_within(i, j)
  }
  intensities[j] <- ex_intensities[[tmp]]
}

alpha <- rate_drift
beta <- rate_killing

#+ r v0.2.0
set.seed(use_seed)
mb1 <- microbenchmark(
  ESM = rmo_esm(n, d, intensities),
  Arnold = rmo_arnold(n, d, intensities),
  Ex_Arnold = rmo_ex_arnold(n, d, ex_intensities),
  LFM_CPP = rmo_lfm_cpp(n, d, rate, rate_killing, rate_drift,
    rjump_name = "rposval", rjump_arg_list = list("value"=1)),
  Cuadras_Auge = rmo_esm_cuadras_auge(n, d, alpha, beta)
)
detach("package:rmo", unload = TRUE)

#+ r v0.1.2
tmpdir <- tempdir()
suppressMessages(devtools::install_github("hsloot/rmo@v0.1.2", lib = tmpdir, quiet = TRUE))
library(rmo, lib.loc = tmpdir)
set.seed(1632L)
mb2 <- microbenchmark(
  ESM = rmo_esm(n, d, intensities),
  Arnold = rmo_arnold(n, d, intensities),
  Ex_Arnold = rmo_ex_arnold(n, d, ex_intensities),
  LFM_CPP = rmo_lfm_cpp(n, d, rate, rate_killing, rate_drift,
    rjump_name = "rposval", rjump_arg_list = list("value"=1)),
  Cuadras_Auge = rmo_esm_cuadras_auge(n, d, alpha, beta)
)
detach("package:rmo", unload = TRUE)
# invisible(print(unlink(tmpdir, recursive = TRUE)))

mb1 %>%
  autoplot()
mb2 %>%
  autoplot()

print(unlink(tmpdir, recursive = TRUE))



rate <- 0.5
eta <- 2
rate_killing <- 0.1
rate_drift <- 0.3

psi <- function(x) {
  ifelse(x > 0, rate_killing, 0) + rate_drift * x +
    rate * (1 - exp(-eta * x))
}

## overwrite to compensate numerical problems
ex_intensities <- vapply(1:d, function(x) {
  ifelse(x==1, rate_drift, 0) +
    ifelse(x==d, rate_killing, 0) +
    rate *  exp(-eta*(d-x)) * (1-exp(-eta))^x
}, 0.5)

intensities <- numeric(2^d-1)
for (j in 1:(2^d-1)) {
  tmp <- 0
  for (i in 1:d) {
    tmp <- tmp + rmo:::is_within(i, j)
  }
  intensities[j] <- ex_intensities[[tmp]]
}

#+ r v0.2.0
library(rmo)
set.seed(use_seed)
mb1 <- microbenchmark(
  ESM = rmo_esm(n, d, intensities),
  Arnold = rmo_arnold(n, d, intensities),
  Ex_Arnold = rmo_ex_arnold(n, d, ex_intensities),
  LFM_CPP = rmo_lfm_cpp(n, d, rate, rate_killing, rate_drift,
                        rjump_name = "rposval", rjump_arg_list = list("value"=1))
)
detach("package:rmo", unload = TRUE)

#+ r v0.1.2
tmpdir <- tempdir()
suppressMessages(devtools::install_github("hsloot/rmo@v0.1.2", lib = tmpdir, quiet = TRUE))
library(rmo, lib.loc = tmpdir)
set.seed(1632L)
mb2 <- microbenchmark(
  ESM = rmo_esm(n, d, intensities),
  Arnold = rmo_arnold(n, d, intensities),
  Ex_Arnold = rmo_ex_arnold(n, d, ex_intensities),
  LFM_CPP = rmo_lfm_cpp(n, d, rate, rate_killing, rate_drift,
                        rjump_name = "rposval", rjump_arg_list = list("value"=1))
)
detach("package:rmo", unload = TRUE)
# invisible(print(unlink(tmpdir, recursive = TRUE)))

mb1 %>%
  autoplot()
mb2 %>%
  autoplot()

print(unlink(tmpdir, recursive = TRUE))

# nolint end
