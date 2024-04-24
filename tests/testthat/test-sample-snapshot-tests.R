skip_on_cran()

# RNG changes, see https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=17494
skip_if(!require_R_version("3.6.0"))

set.seed(1632)

test_that("Snapshot tests for `rmo`", {
  intensities <- c(0.4, 0.4, 0.1, 0.4, 0.1, 0.1, 0.4)
  expect_snapshot(rmo(10, 3, intensities))
  expect_snapshot(rmo(10, 3, intensities, method = "AM"))
  expect_snapshot(rmo(10, 3, intensities, method = "ESM"))
})

test_that("Snapshot tests for `rexmo`", {
  theta <- c(1.2, 0.3, 0.4)
  expect_snapshot(rexmo(10, 3, theta))
  expect_snapshot(rexmo(10, 3, theta, method = "MDCM"))
  expect_snapshot(rexmo(10, 3, theta, method = "AM"))
  expect_snapshot(rexmo(10, 3, theta, method = "ESM"))
})

test_that("Snapshot tests for `rextmo`", {
  bf <- AlphaStableBernsteinFunction(alpha = log2(2 - 0.5))
  expect_snapshot(rextmo(10, 3, bf))
  expect_snapshot(rextmo(10, 3, bf, method = "MDCM"))
  expect_snapshot(rextmo(10, 3, bf, method = "AM"))
  expect_snapshot(rextmo(10, 3, bf, method = "ESM"))
})

test_that("Snapshot tests for `rpextmo`", {
  a <- 0.2
  b <- 0.5
  gamma <- 0.5

  expect_snapshot(rpextmo(10, 3, a = a, b = b))
  expect_snapshot(rpextmo(10, 3, a = a, b = b, method = "ESM"))
  expect_snapshot(rpextmo(10, 3, a = a, b = b, method = "MDCM"))
  expect_snapshot(rpextmo(10, 3, a = a, b = b, method = "LFM"))
  expect_snapshot(rpextmo(10, 3, a = a, b = b, method = "AM"))

  eta <- 0.5
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Poisson"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Poisson",
      method = "MDCM"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Poisson",
      method = "LFM"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Poisson",
      method = "AM"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Poisson", method = "ESM"
    )
  )

  eta <- c(0.5, 1e-4)
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Pareto"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Pareto",
      method = "MDCM"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Pareto",
      method = "LFM"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Pareto",
      method = "AM"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Pareto",
      method = "ESM"
    )
  )

  eta <- 0.5
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma,
      eta = eta, family = "Exponential"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma,
      eta = eta, family = "Exponential",
      method = "MDCM"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma,
      eta = eta, family = "Exponential",
      method = "LFM"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma,
      eta = eta, family = "Exponential",
      method = "AM"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma,
      eta = eta, family = "Exponential", method = "ESM"
    )
  )

  eta <- 0.5
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma,
      eta = eta, family = "AlphaStable"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma,
      eta = eta, family = "AlphaStable",
      method = "MDCM"
    )
  )
  expect_snapshot(
    rpextmo(10, 3,
      a = a, b = b, gamma = gamma,
      eta = eta, family = "AlphaStable",
      method = "AM"
    )
  )
  expect_snapshot(
    rpextmo(10, 3,
      a = a, b = b, gamma = gamma,
      eta = eta, family = "AlphaStable",
      method = "ESM"
    )
  )

  eta <- 0.5
  expect_snapshot(
    rpextmo(10, 3,
      a = a, b = b, gamma = gamma,
      eta = eta,
      family = "InverseGaussian"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma,
      eta = eta, family = "InverseGaussian",
      method = "MDCM"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma,
      eta = eta, family = "InverseGaussian",
      method = "AM"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma,
      eta = eta, family = "InverseGaussian",
      method = "ESM"
    )
  )

  eta <- 0.5
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Gamma"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Gamma",
      method = "MDCM"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Gamma",
      method = "AM"
    )
  )
  expect_snapshot(
    rpextmo(
      10, 3,
      a = a, b = b, gamma = gamma, eta = eta,
      family = "Gamma",
      method = "ESM"
    )
  )

  expect_snapshot(
    rextmo(
      10,
      3,
      ConvexCombinationOfBernsteinFunctions(
        coefficients = c(
          2.144887,
          0.1491956,
          0.8279853,
          0.5382334,
          2.687858,
          1.164148,
          1.369062,
          0.02881049
        ),
        points = list(
          InverseGaussianBernsteinFunction(eta = 1.643128),
          LinearBernsteinFunction(scale = 1.452913),
          ParetoBernsteinFunction(alpha = 0.1474029, x0 = 1e-2),
          GammaBernsteinFunction(a = 2.518894),
          ConstantBernsteinFunction(constant = 0.6853059),
          ExponentialBernsteinFunction(lambda = 1.541644),
          InverseGaussianBernsteinFunction(eta = 0.5562086),
          LinearBernsteinFunction(scale = 0.002915767)
        )
      ),
      method = "MDCM"
    )
  )
})
