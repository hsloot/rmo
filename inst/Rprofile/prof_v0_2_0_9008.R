# nolint start
#+ setup
library("rmo")
library("tidyverse")
library("ggplot2")
library("bench")

source(system.file(
  "Rsource", "parameter_generator.R",
  package="rmo", mustWork=TRUE))

## Use pre R 3.6.x sample RNG since the new one is not yet
## implemented in Rcpp.
suppressWarnings(RNGkind(kind="Mersenne-Twister", normal.kind = "Inversion", sample.kind="Rounding"))

n <- 1e4L
d1 <- 2L
d2 <- 3L
d <- d1+d2
d_max <- 10


#' ## Cuadras-Augé Parameter
#'
#' We first test the speed on a parametrisation, where we would expect
#' performance increases (parameterisations with many zeros):
#' The Cuadras-Augé (individual and global shocks) distribution.
#+ r parameters-first
alpha <- 0.3
beta <- 0.1

ex_intensities <- ex_intensities_cuadras_auge(d, alpha=alpha, beta=beta)
intensities <- intensities_cuadras_auge(d, alpha=alpha, beta=beta)

mb1 <- bench::mark(
  ESM = rmo:::Rcpp__rmo_esm(n, d, intensities=intensities),
  Arnold = rmo:::Rcpp__rmo_arnold(n, d, intensities=intensities),
  Ex_Arnold = rmo:::Rcpp__rmo_ex_arnold(n, d, ex_intensities=ex_intensities),
  Cuadras_Auge = rmo_esm_cuadras_auge(n, d, alpha, beta),
  min_iterations = 100L,
  check=FALSE
)

mb1
mb1 %>%
  autoplot()

bp1 <- bench::press(
  d = (1:(d_max%/%2))*2,
  {
    ex_intensities <- ex_intensities_cuadras_auge(d, alpha=alpha, beta=beta)
    intensities <- intensities_cuadras_auge(d, alpha=alpha, beta=beta)
    force(ex_intensities)
    force(intensities)
    bench::mark(
      ESM = rmo:::Rcpp__rmo_esm(n, d, intensities=intensities),
      Arnold = rmo:::Rcpp__rmo_arnold(n, d, intensities=intensities),
      Ex_Arnold = rmo:::Rcpp__rmo_ex_arnold(n, d, ex_intensities=ex_intensities),
      Cuadras_Auge = rmo:::Rcpp__rmo_esm_cuadras_auge(n, d, alpha, beta),
      min_iterations = 100L,
      check=FALSE
    )
  }
)

bp1 %>%
  unnest(cols = c("expression", "d", "time", "gc", "mem_alloc")) %>%
  filter(gc == "none") %>%
  mutate(expression = factor(expression, levels = c("Cuadras_Auge", "Ex_Arnold", "Arnold", "ESM"))) %>%
  ggplot(aes(x = d, y = time)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "grey50", formula = y ~ x + I(x^2)) +
  theme(legend.position = "bottom") +
  facet_grid(expression ~ .)

detach("package:rmo", unload = TRUE)

tmpdir <- tempdir()
suppressMessages(devtools::install_github("hsloot/rmo@v0.2.0", lib = tmpdir, quiet = TRUE))
library(rmo, lib.loc = tmpdir)

mb2 <- bench::mark(
  ESM = rmo:::Rcpp__rmo_esm(n, d, intensities=intensities),
  Arnold = rmo:::Rcpp__rmo_arnold(n, d, intensities=intensities),
  Ex_Arnold = rmo:::Rcpp__rmo_ex_arnold(n, d, ex_intensities=ex_intensities),
  Cuadras_Auge = rmo_esm_cuadras_auge(n, d, alpha, beta),
  min_iterations = 100L,
  check=FALSE
)

mb2
mb2 %>%
  autoplot()

detach("package:rmo", unload = TRUE)


#' ## Another exchangeable parametrisation
#'
#+ r parameters-alpha
library(rmo)
alpha <- 0.4

qplot(1:10, ex_intensities_alpha_stable(10, alpha)) + theme(axis.title.x = element_blank()) + scale_x_continuous(labels=function(x) sprintf("%.0f", x)) + scale_y_continuous(name="Intensity")

ex_intensities <- ex_intensities_alpha_stable(d, alpha)
intensities <- intensities_alpha_stable(d, alpha)

mb3 <- bench::mark(
  ESM = rmo:::Rcpp__rmo_esm(n, d, intensities=intensities),
  Arnold = rmo:::Rcpp__rmo_arnold(n, d, intensities=intensities),
  Ex_Arnold = rmo:::Rcpp__rmo_ex_arnold(n, d, ex_intensities=ex_intensities),
  min_iterations = 100L,
  check=FALSE
)

mb3
mb3 %>%
  autoplot()

bp2 <- bench::press(
  d = (1:(d_max%/%2))*2,
  {
    ex_intensities <- ex_intensities_alpha_stable(d, alpha)
    intensities <- intensities_alpha_stable(d, alpha)
    force(ex_intensities)
    force(intensities)
    bench::mark(
      ESM = rmo:::Rcpp__rmo_esm(n, d, intensities=intensities),
      Arnold = rmo:::Rcpp__rmo_arnold(n, d, intensities=intensities),
      Ex_Arnold = rmo:::Rcpp__rmo_ex_arnold(n, d, ex_intensities=ex_intensities),
      min_iterations = 100L,
      check=FALSE
    )
  }
)

bp2 %>%
  unnest(cols = c("expression", "d", "time", "gc")) %>%
  filter(gc == "none") %>%
  mutate(expression = factor(expression, levels = c("Ex_Arnold", "Arnold", "ESM"))) %>%
  ggplot(aes(x = d, y = time)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "grey50", formula = y ~ x + I(x^2)) +
  theme(legend.position = "bottom") +
  facet_grid(expression ~ .)

detach("package:rmo", unload = TRUE)

library(rmo, lib.loc = tmpdir)
mb4 <- bench::mark(
  ESM = rmo:::Rcpp__rmo_esm(n, d, intensities=intensities),
  Arnold = rmo:::Rcpp__rmo_arnold(n, d, intensities=intensities),
  Ex_Arnold = rmo:::Rcpp__rmo_ex_arnold(n, d, ex_intensities=ex_intensities),
  min_iterations = 100L,
  check=FALSE
)

mb4
mb4 %>%
  autoplot()
detach("package:rmo", unload = TRUE)



#' ## Another exchangeable parametrisation
#'
#+ r parameters-Poisson
library(rmo)

lambda <- 1
eta <- 0.7

qplot(1:10, ex_intensities_poisson(10, lambda=lambda, eta=eta)) +
  theme(axis.title.x = element_blank()) +
  scale_x_continuous(labels=function(x) sprintf("%.0f", x)) +
  scale_y_continuous(name="Intensity")

ex_intensities <- ex_intensities_poisson(d, lambda=lambda, eta=eta)
intensities <- intensities_poisson(d, lambda=lambda, eta=eta)

mb5 <- bench::mark(
  ESM = rmo:::Rcpp__rmo_esm(n, d, intensities=intensities),
  Arnold = rmo:::Rcpp__rmo_arnold(n, d, intensities=intensities),
  Ex_Arnold = rmo:::Rcpp__rmo_ex_arnold(n, d, ex_intensities=ex_intensities),
  min_iterations = 100L,
  check=FALSE
)

mb5
mb5 %>%
  autoplot()

bp3 <- bench::press(
  d = (1:(d_max%/%2))*2,
  {
    ex_intensities <- ex_intensities_poisson(d, lambda=lambda, eta=eta)
    intensities <- intensities_poisson(d, lambda=lambda, eta=eta)
    force(ex_intensities)
    force(intensities)
    bench::mark(
      ESM = rmo:::Rcpp__rmo_esm(n, d, intensities=intensities),
      Arnold = rmo:::Rcpp__rmo_arnold(n, d, intensities=intensities),
      Ex_Arnold = rmo:::Rcpp__rmo_ex_arnold(n, d, ex_intensities=ex_intensities),
      min_iterations = 100L,
      check=FALSE
    )
  }
)

bp3 %>%
  unnest(cols = c("expression", "d", "time", "gc", "mem_alloc")) %>%
  filter(gc == "none") %>%
  mutate(expression = factor(
    expression, levels = c("Cuadras_Auge", "Ex_Arnold", "Arnold", "ESM")
    )) %>%
  ggplot(aes(x = d, y = time)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "grey50", formula = y ~ x + I(x^2)) +
  theme(legend.position = "bottom") +
  facet_grid(expression ~ .)

detach("package:rmo", unload = TRUE)

library(rmo, lib.loc = tmpdir)
mb6 <- bench::mark(
  ESM = rmo:::Rcpp__rmo_esm(n, d, intensities=intensities),
  Arnold = rmo:::Rcpp__rmo_arnold(n, d, intensities=intensities),
  Ex_Arnold = rmo:::Rcpp__rmo_ex_arnold(n, d, ex_intensities=ex_intensities),
  min_iterations = 100L,
  check=FALSE
)

mb6
mb6 %>%
  autoplot()
detach("package:rmo", unload = TRUE)


#' ## Another more general parametrisation
#'
#' We also test the speed on a general, asymmetric parametrisation
#' to assure that there are non performance decreases.
#+ r parameters-third
library(rmo)

lambda <- 0.3
eta <- 0.5
alpha <- 0.4
a <- 0.3

intensities <- intensities_hierarchical(
  d1, d2, lambda=lambda, eta=eta, alpha=alpha, a=a)

mb7 <- bench::mark(
  ESM = rmo:::Rcpp__rmo_esm(n, d, intensities=intensities),
  Arnold = rmo:::Rcpp__rmo_arnold(n, d, intensities=intensities),
  min_iterations = 100L,
  check=FALSE
)
mb7
mb7 %>%
  autoplot()

bp4 <- bench::press(
  d1 = 1:5,
  d2 = 1:5,
  {
    d <- d1 + d2
    intensities <- intensities_hierarchical(
      d1, d2, lambda=lambda, eta=eta, alpha=alpha, a=a)
    force(intensities)
    bench::mark(
      ESM = rmo:::Rcpp__rmo_esm(n, d, intensities=intensities),
      Arnold = rmo:::Rcpp__rmo_arnold(n, d, intensities=intensities),
      min_iterations = 100L,
      check=FALSE
    )
  }
)

bp4 %>%
  unnest(cols = c("expression", "d1", "d2", "time", "gc", "mem_alloc")) %>%
  filter(gc == "none") %>%
  mutate(expression = factor(
    expression, levels = c("Arnold", "ESM")
  )) %>%
  ggplot(aes(x = d1+d2, y = time)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "grey50", formula = y ~ x + I(x^2)) +
  theme(legend.position = "bottom") +
  facet_grid(expression ~ .)

detach("package:rmo", unload = TRUE)


library(rmo, lib.loc = tmpdir)

mb8 <- bench::mark(
  ESM = rmo:::Rcpp__rmo_esm(n, d, intensities=intensities),
  Arnold = rmo:::Rcpp__rmo_arnold(n, d, intensities=intensities),
  min_iterations = 100L,
  check=FALSE
)

mb8
mb8 %>%
  autoplot()
detach("package:rmo", unload = TRUE)

# print(unlink(tmpdir, recursive = TRUE))
# nolint end
