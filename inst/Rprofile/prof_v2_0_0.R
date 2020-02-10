#+ setup
library("rmo")
library("magrittr")
library("ggplot2")
library("microbenchmark")

#+ r parameter
use_seed <- 1632L
n <- 1e2L
d <- 7L

alpha <- 0.5
beta <- 0.3
rate <- 1 # dummy
rate_killing <- beta
rate_drift <- alpha

psi <- function(x) {
  ifelse(x > 0, rate_killing, 0) + rate_drift * x
}
a <- round(psi(1:d) - psi(0:(d-1)), 2)
ex_intensities <- vapply(1:d, function(x) {
  sum(vapply(0:(x-1), function(y) {
    (-1)^y * choose(x-1, y) * a[[d-x+y+1]]
  }, FUN.VALUE=0.5))
}, FUN.VALUE=0.5)
intensities <- numeric(2^d-1)
for (j in 1:(2^d-1)) {
  tmp <- 0
  for (i in 1:d) {
    tmp <- tmp + rmo:::is_within(i, j)
  }
  intensities[j] <- ex_intensities[[tmp]]
}

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
invisible(print(unlink(tmpdir, recursive = TRUE)))

mb1 %>%
  autoplot()
mb2 %>%
  autoplot()
