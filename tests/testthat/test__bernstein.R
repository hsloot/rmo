context("Bernstein Function S4 class")

test_that("Test initialisation of BernsteinFunction classes", {
  expect_s4_class(ConstantBernsteinFunction(constant=0.5),
    class="ConstantBernsteinFunction")
  expect_error

  expect_s4_class(LinearBernsteinFunction(scale=0.4),
    class="LinearBernsteinFunction")
  expect_error(LinearBernsteinFunction(scale=-1))
  expect_error(LinearBernsteinFunction(scale=c(1, 2)))

  expect_s4_class(
    PoissonBernsteinFunction(lambda=0.3, eta=0.2),
    class="PoissonBernsteinFunction")
  expect_error(PoissonBernsteinFunction(lambda=-1, eta=0.2))
  expect_error(PoissonBernsteinFunction(lambda=c(1, 2), eta=0.2))

  expect_s4_class(
    AlphaStableBernsteinFunction(alpha=0.6),
    class="AlphaStableBernsteinFunction")
  expect_error(AlphaStableBernsteinFunction(alpha=-0.5))
  expect_error(AlphaStableBernsteinFunction(alpha=0))
  expect_error(AlphaStableBernsteinFunction(alpha=1))
  expect_error(AlphaStableBernsteinFunction(alpha=1.5))
  expect_error(AlphaStableBernsteinFunction(alpha=c(1, 2)))

  expect_s4_class(
    GammaBernsteinFunction(a=0.7),
    class="GammaBernsteinFunction")
  expect_error(GammaBernsteinFunction(a=-1))

  expect_s4_class(
    ParetoBernsteinFunction(alpha=0.7, x0=1),
    class="ParetoBernsteinFunction")
  expect_error(ParetoBernsteinFunction(alpha=-1, x0=1))

  expect_s4_class(
    ScaledBernsteinFunction(scale=0.5, original=GammaBernsteinFunction(a=0.7)),
    class="ScaledBernsteinFunction")
  expect_error(ScaledBernsteinFunction(scale=-1,
    original=GammaBernsteinFunction(a=0.7)))

  expect_s4_class(
    SumOfBernsteinFunctions(
      first=LinearBernsteinFunction(scale=0.2),
      second=ScaledBernsteinFunction(scale=0.5,
        original=AlphaStableBernsteinFunction(alpha=0.6))),
    class="SumOfBernsteinFunctions")
})


test_that("`valueOf` for `ConstantBernsteinFunction`", {
  k <- 5L
  constant <- 0.3
  bf <- ConstantBernsteinFunction(constant=constant)

  x <- seq(0, 10, by=1)
  expect_equal(
    valueOf(bf, x, difference_order=0L),
    ifelse(x==0, 0, constant))

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(valueOf(bf, y, difference_order=0L), differences=k)[seq_along(x)])

  x <- seq(0.3, 10.3, by=1)
  expect_equal(
    valueOf(bf, x, difference_order=0L),
    ifelse(x==0, 0, constant))

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])
})

test_that("`valueOf` for `LinearBernsteinFunction`", {
  k <- 5L
  scale <- 0.4
  bf <- LinearBernsteinFunction(scale=scale)

  x <- seq(0, 10, by=1)
  expect_equal(valueOf(bf, x, difference_order=0L), x*scale)

  y <- sort(unique(union(x, x+1L)))
  expect_equal(
    valueOf(bf, x, difference_order=1L),
    (-1) ^ (1L-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=1L)[seq_along(x)])

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])

  x <- seq(0.3, 10.3, by=1)
  expect_equal(valueOf(bf, x, difference_order=0L), x*scale)

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])

  y <- sort(unique(union(x, x+1L)))
  expect_equal(
    valueOf(bf, x, difference_order=1L),
    (-1) ^ (1L-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=1L)[seq_along(x)])
})

test_that("`valueOf` for `PoissonBernsteinFunction`", {
  k <- 5L
  lambda <- 0.4
  eta <- 0.2
  bf <- PoissonBernsteinFunction(lambda=lambda, eta=eta)

  x <- seq(0, 10, by=1)
  expect_equal(
    valueOf(bf, x, difference_order=0L),
    lambda * (1 - exp(-eta*x)))

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    lambda * exp(-eta*x) * (1 - exp(-eta))^k)

  x <- seq(0.3, 10.3, by=1)
  expect_equal(
    valueOf(bf, x, difference_order=0L),
    lambda * (1 - exp(-eta*x)))

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    lambda * exp(-eta*x) * (1 - exp(-eta))^k)
})

test_that("`valueOf` for `AlphaStableBernsteinFunction`", {
  alpha <- 0.5
  bf <- AlphaStableBernsteinFunction(alpha=alpha)

  x <- seq(0, 10, by=1)
  expect_equal(valueOf(bf, x, difference_order=0L),
    x^alpha)

  k <- 1L
  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])

  k <- 5L
  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])

  x <- seq(0.3, 10.3, by=1)
  expect_equal(valueOf(bf, x, difference_order=0L), x^alpha)

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])
})

test_that("`valueOf` for `GammaBernsteinFunction`", {
  k <- 5L
  a <- 0.3
  bf <- GammaBernsteinFunction(a=a)

  x <- seq(0, 10, by=1)
  expect_equal(valueOf(bf, x, difference_order=0L), log(1 + x/a))

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])

  x <- seq(0.3, 10.3, by=1)
  expect_equal(valueOf(bf, x, difference_order=0L), log(1 + x/a))

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])
})

test_that("`valueOf` for `ParetoBernsteinFunction`", {
  alpha <- 0.05
  x0 <- 0.5
  bf <- ParetoBernsteinFunction(alpha=alpha, x0=x0)

  x <- seq(0, 10, by=1)
  expect_equal(valueOf(bf, x, difference_order=0L),
    1-exp(-x0*x) + (x*x0) ^ (alpha) *
    pgamma(x0*x, 1-alpha, lower=FALSE) * gamma(1-alpha))

  k <- 1L
  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])

  k <- 5L
  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])

  x <- seq(0.3, 10.3, by=1)
  expect_equal(valueOf(bf, x, difference_order=0L),
    1-exp(-x0*x) + (x*x0) ^ (alpha) *
    pgamma(x0*x, 1-alpha, lower=FALSE) * gamma(1-alpha))

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])
})

test_that("`valueOf` for `ScaledBernsteinFunction`", {
  k <- 5L
  alpha <- 0.4
  scale <- 0.2
  bf <- ScaledBernsteinFunction(
    scale=scale,
    original=AlphaStableBernsteinFunction(alpha=alpha))

  x <- seq(0, 10, by=1)
  expect_equal(valueOf(bf, x, difference_order=0L), scale * x^alpha)

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])

  x <- seq(0.3, 10.3, by=1)
  expect_equal(valueOf(bf, x, difference_order=0L), scale * x^alpha)

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])
})

test_that("`valueOf` for `SumOfBernsteinFunctions`", {
  k <- 5L
  constant <- 0.1
  scale <- 0.2
  lambda <- 0.4
  eta <- 0.5
  bf <- SumOfBernsteinFunctions(
    first=SumOfBernsteinFunctions(
      first=ConstantBernsteinFunction(constant=constant),
      second=LinearBernsteinFunction(scale=scale)
    ),
    second=PoissonBernsteinFunction(lambda=lambda, eta=eta))

  x <- seq(0, 10, by=1)
  expect_equal(
    valueOf(bf, x, difference_order=0L),
    ifelse(x==0, 0, constant) + scale*x + lambda * (1 - exp(-eta*x)))

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])

  x <- seq(0.3, 10.3, by=1)
  expect_equal(
    valueOf(bf, x, difference_order=0L),
    ifelse(x==0, 0, constant) + scale*x + lambda * (1 - exp(-eta*x)))

  y <- sort(unique(union(x, x+k)))
  expect_equal(
    valueOf(bf, x, difference_order=k),
    (-1) ^ (k-1) * diff(
      valueOf(bf, y, difference_order=0L),
      differences=k)[seq_along(x)])
})
