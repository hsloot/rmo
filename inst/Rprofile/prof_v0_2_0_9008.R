# nolint start
#+ setup
library("rmo")
library("magrittr")
library("ggplot2")
library("microbenchmark")

source(system.file("Rsource", "parameter_generator.R",
  package="rmo", mustWork=TRUE))

use_seed <- 1632L
n <- 1e4L
d1 <- 2L
d2 <- 4L
d <- d1+d2


#' ## Cuadras-Augé Parameter
#'
#' We first test the speed on a parametrisation, where we would expect
#' performance increases (parameterisations with many zeros):
#' The Cuadras-Augé (individual and global shocks) distribution.
#+ r parameters-first
scale <- 0.3
alpha <- scale
beta <- 0
rate <- 0
rate_killing <- 0
rate_drift <- scale

ex_intensities <- ex_intensities_linear(d, scale=scale)
intensities <- intensities_linear(d, scale=scale)

#+ r v0.2.0.9007-first
set.seed(use_seed)
mb1 <- microbenchmark(
  ESM = rmo_esm(n, d, intensities=intensities),
  Arnold = rmo_arnold(n, d, intensities=intensities),
  Ex_Arnold = rmo_ex_arnold(n, d, ex_intensities=ex_intensities),
  LFM_CPP = rmo_lfm_cpp(n, d, rate=rate, rate_killing=rate_killing,
    rate_drift=rate_drift, rjump_name="rposval",
    rjump_arg_list=list("value"=1)),
  Cuadras_Auge = rmo_esm_cuadras_auge(n, d, alpha, beta)
)
detach("package:rmo", unload = TRUE)

#+ r v0.2.0-first
tmpdir <- tempdir()
suppressMessages(devtools::install_github("hsloot/rmo@v0.2.0", lib = tmpdir, quiet = TRUE))
library(rmo, lib.loc = tmpdir)
set.seed(use_seed)

mb2 <- microbenchmark(
  ESM = rmo_esm(n, d, intensities=intensities),
  Arnold = rmo_arnold(n, d, intensities=intensities),
  Ex_Arnold = rmo_ex_arnold(n, d, ex_intensities=ex_intensities),
  LFM_CPP = rmo_lfm_cpp(n, d, rate=rate, rate_killing=rate_killing,
    rate_drift=rate_drift, rjump_name="rposval",
    rjump_arg_list=list("value"=1)),
  Cuadras_Auge = rmo_esm_cuadras_auge(n, d, alpha, beta)
)
detach("package:rmo", unload = TRUE)

#+ r plot-results-first
mb1 %>%
  autoplot()
mb1
mb2 %>%
  autoplot()
mb2



#' ## More general parametrisation
#'
#' We also test the speed on a general, asymmetric parametrisation
#' to assure that there are non performance decreases.
#+ r parameters-second
library(rmo)
lambda <- 0.2
eta <- 0.5
alpha <- 0.4
a <- 0.3

intensities <- intensities_hierarchical(d1, d2, lambda=lambda,
  eta=eta, alpha=alpha, a=a)

#+ r v0.2.0.9007-second
set.seed(use_seed)
mb1 <- microbenchmark(
  ESM = rmo_esm(n, d, intensities=intensities),
  Arnold = rmo_arnold(n, d, intensities=intensities)
)
detach("package:rmo", unload = TRUE)

#+ r v0.2.0-second
tmpdir <- tempdir()
suppressMessages(devtools::install_github("hsloot/rmo@v0.2.0", lib = tmpdir, quiet = TRUE))
library(rmo, lib.loc = tmpdir)
set.seed(use_seed)

mb2 <- microbenchmark(
  ESM = rmo_esm(n, d, intensities=intensities),
  Arnold = rmo_arnold(n, d, intensities=intensities)
)
detach("package:rmo", unload = TRUE)

#+ r plot-results-second
mb1 %>%
  autoplot()
mb1
mb2 %>%
  autoplot()
mb2

# print(unlink(tmpdir, recursive = TRUE))
# nolint end
