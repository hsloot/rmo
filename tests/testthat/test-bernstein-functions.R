set.seed(1624)

# #### Test initialization of `BernsteinFunction`-classes ####

test_that("Test initialisation of BernsteinFunction classes", {
    expect_s4_class(
        testutils.rmo::fuzzy_bf(ConstantBernsteinFunction()),
        class = "ConstantBernsteinFunction"
    )
    expect_error(ConstantBernsteinFunction(constant = -1))

    expect_s4_class(
        testutils.rmo::fuzzy_bf(LinearBernsteinFunction()),
        class = "LinearBernsteinFunction"
    )
    expect_error(LinearBernsteinFunction(scale = -1))
    expect_error(LinearBernsteinFunction(scale = c(1, 2)))

    expect_s4_class(
        testutils.rmo::fuzzy_bf(PoissonBernsteinFunction()),
        class = "PoissonBernsteinFunction"
    )
    expect_error(PoissonBernsteinFunction(eta = -0.2))
    expect_error(PoissonBernsteinFunction(eta = c(0.1, 0.2)))

    expect_s4_class(
        testutils.rmo::fuzzy_bf(AlphaStableBernsteinFunction()),
        class = "AlphaStableBernsteinFunction"
    )
    expect_error(AlphaStableBernsteinFunction(alpha = -0.5))
    expect_error(AlphaStableBernsteinFunction(alpha = 0))
    expect_error(AlphaStableBernsteinFunction(alpha = 1))
    expect_error(AlphaStableBernsteinFunction(alpha = 1.5))
    expect_error(AlphaStableBernsteinFunction(alpha = c(1, 2)))

    expect_s4_class(
        testutils.rmo::fuzzy_bf(GammaBernsteinFunction()),
        class = "GammaBernsteinFunction"
    )
    expect_error(GammaBernsteinFunction(a = -1))

    expect_s4_class(
        testutils.rmo::fuzzy_bf(ParetoBernsteinFunction()),
        class = "ParetoBernsteinFunction"
    )
    expect_error(ParetoBernsteinFunction(alpha = -1, x0 = 1e-4))

    expect_s4_class(
        testutils.rmo::fuzzy_bf(ScaledBernsteinFunction()),
        class = "ScaledBernsteinFunction"
    )
    expect_error(
        ScaledBernsteinFunction(scale = -1, original = GammaBernsteinFunction())
    )

    expect_s4_class(
        testutils.rmo::fuzzy_bf(
            CompositeScaledBernsteinFunction(original = GammaBernsteinFunction())
        ),
        class = "CompositeScaledBernsteinFunction"
    )
    expect_error(
        CompositeScaledBernsteinFunction(cscale = -1, original = GammaBernsteinFunction())
    )

    expect_s4_class(
        testutils.rmo::fuzzy_bf(
            SumOfBernsteinFunctions(
                first = LinearBernsteinFunction(),
                second = ScaledBernsteinFunction(
                    original = AlphaStableBernsteinFunction()
                )
            )
        ),
        class = "SumOfBernsteinFunctions"
    )

    expect_s4_class(
        testutils.rmo::fuzzy_bf(
            ConvexCombinationOfBernsteinFunctions()
        ),
        class = "ConvexCombinationOfBernsteinFunctions"
    )
})


# #### Helper functions and global variables ####

value_of_naive <- function(f, x, difference_order = 0L, n = 1L, k = 0L, cscale = 1, ...) {
    if (isTRUE(0L == difference_order)) {
        out <- f(cscale * x, ...)
    } else {
        out <- (-1)^(difference_order - 1L) *
            sapply(
                x,
                function(.x) {
                    diff(
                        f(cscale * (.x + (0:difference_order)), ...),
                        differences = difference_order
                    )
                }
            )
    }

    choose(n, k) * out
}

x <- seq(0, 10, by = 0.25)
difference_order <- 5L
cscale <- sqrt(2)
n <- difference_order - 1L
k <- n %/% 2

# #### Boundary case `BernsteinFunction` classes ####

test_that("`valueOf` for `ConstantBernsteinFunction`", {
    bf <- testutils.rmo::fuzzy_bf(ConstantBernsteinFunction())
    constant <- bf@constant
    actual_fn <- function(x, constant) {
        ifelse(0 < x, constant, 0)
    }

    expect_equal(valueOf(bf, x), actual_fn(x, constant))

    expect_equal(
        valueOf(bf, x, difference_order = difference_order),
        value_of_naive(actual_fn, x, difference_order, constant = constant)
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            constant = constant
        )
    )
})

test_that("`valueOf` for `LinearBernsteinFunction`", {
    bf <- testutils.rmo::fuzzy_bf(LinearBernsteinFunction())
    scale <- bf@scale
    actual_fn <- function(x, scale) {
        x * scale
    }

    expect_equal(valueOf(bf, x), actual_fn(x, scale))

    expect_equal(
        valueOf(bf, x, difference_order = difference_order),
        value_of_naive(actual_fn, x, difference_order, scale = scale)
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            scale = scale
        )
    )
})


# #### Classes for `BernsteinFunction` arithmetics ####

test_that("`valueOf` for `ScaledBernsteinFunction`", {
    bf <- testutils.rmo::fuzzy_bf(
        ScaledBernsteinFunction(
            scale = 1,
            original = AlphaStableBernsteinFunction()
        )
    )
    scale <- bf@scale
    alpha <- bf@original@alpha
    actual_fn <- function(x, scale, alpha) {
        scale * x^alpha
    }

    expect_equal(valueOf(bf, x), actual_fn(x, scale, alpha))
    expect_equal(
        valueOf(bf, x, method = "stieltjes", tolerance = testthat_tolerance()),
        actual_fn(x, scale, alpha)
    )
    expect_equal(
        valueOf(bf, x, method = "levy", tolerance = testthat_tolerance()),
        actual_fn(x, scale, alpha)
    )

    expect_equal(
        valueOf(bf, x, difference_order = difference_order, tolerance = testthat_tolerance()),
        value_of_naive(actual_fn, x, difference_order, scale = scale, alpha = alpha)
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "stieltjes", tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, scale = scale, alpha = alpha)
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, scale = scale, alpha = alpha)
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            scale = scale, alpha = alpha
        )
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "stieltjes", tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            scale = scale, alpha = alpha
        )
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            scale = scale, alpha = alpha
        )
    )
})

test_that("`valueOf` for `CompositeScaledBernsteinFunction`", {
    bf <- testutils.rmo::fuzzy_bf(
        CompositeScaledBernsteinFunction(
            cscale = 1,
            original = AlphaStableBernsteinFunction()
        )
    )
    cscale <- bf@cscale
    alpha <- bf@original@alpha
    actual_fn <- function(x, comp_scale, alpha) {
        (comp_scale * x)^alpha
    }

    expect_equal(valueOf(bf, x), actual_fn(x, cscale, alpha))
    expect_equal(
        valueOf(bf, x, method = "stieltjes", tolerance = testthat_tolerance()),
        actual_fn(x, cscale, alpha)
    )
    expect_equal(
        valueOf(bf, x, method = "levy", tolerance = testthat_tolerance()),
        actual_fn(x, cscale, alpha)
    )

    expect_equal(
        valueOf(bf, x, difference_order = difference_order, tolerance = testthat_tolerance()),
        value_of_naive(actual_fn, x, difference_order, comp_scale = cscale, alpha = alpha)
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "stieltjes", tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, comp_scale = cscale, alpha = alpha)
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, comp_scale = cscale, alpha = alpha)
    )
})

test_that("`valueOf` for `SumOfBernsteinFunctions`", {
    bf <- testutils.rmo::fuzzy_bf(
        SumOfBernsteinFunctions(
            first = SumOfBernsteinFunctions(
                first = ConstantBernsteinFunction(),
                second = LinearBernsteinFunction()
            ),
            second = PoissonBernsteinFunction()
        )
    )
    constant <- bf@first@first@constant
    scale <- bf@first@second@scale
    eta <- bf@second@eta
    actual_fn <- function(x, constant, scale, eta) {
        ifelse(0 < x, constant, 0) + scale * x + 1 - exp(-x * eta)
    }

    expect_equal(
        valueOf(bf, x),
        actual_fn(x, constant, scale, eta)
    )
    expect_equal(
        valueOf(bf, x, method = "levy", tolerance = testthat_tolerance()),
        actual_fn(x, constant, scale, eta)
    )

    expect_equal(
        valueOf(bf, x, difference_order = difference_order, tolerance = testthat_tolerance()),
        value_of_naive(actual_fn, x, difference_order,
            constant = constant, scale = scale, eta = eta
        )
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order,
            constant = constant, scale = scale, eta = eta
        )
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order, tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            constant = constant, scale = scale, eta = eta
        )
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            constant = constant, scale = scale, eta = eta
        )
    )
})

test_that("`valueOf` for `ConvexCombinationOfBernsteinFunctions`", {
    bf <- testutils.rmo::fuzzy_bf(ConvexCombinationOfBernsteinFunctions())
    n <- length(bf@coefficients)

    actual_fn <- function(x) {
        coefficients <- bf@coefficients
        points <- bf@points
        value_matrix <- drop(t(sapply(points, valueOf, x = x)))

        drop(t(coefficients) %*% value_matrix)
    }

    expect_equal(valueOf(bf, x), actual_fn(x))

    expect_equal(
        valueOf(bf, x, difference_order = difference_order, tolerance = testthat_tolerance()),
        value_of_naive(actual_fn, x, difference_order)
    )

    expect_equal(
        valueOf(bf, x,
                difference_order = difference_order, tolerance = testthat_tolerance(),
                n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
                       difference_order = difference_order,
                       n = n, k = k, cscale = cscale
        )
    )
})


# #### Algebraic `BernsteinFunction` classes ####

test_that("`valueOf` for `AlphaStableBernsteinFunction`", {
    bf <- testutils.rmo::fuzzy_bf(AlphaStableBernsteinFunction())
    alpha <- bf@alpha
    actual_fn <- function(x, alpha) {
        x^alpha
    }

    expect_equal(valueOf(bf, x), actual_fn(x, alpha))
    expect_equal(
        valueOf(bf, x, method = "levy", tolerance = testthat_tolerance()),
        actual_fn(x, alpha)
    )
    expect_equal(
        valueOf(bf, x, method = "stieltjes", tolerance = testthat_tolerance()),
        actual_fn(x, alpha)
    )

    expect_equal(
        valueOf(bf, x, difference_order = difference_order, tolerance = testthat_tolerance()),
        value_of_naive(actual_fn, x, difference_order, alpha = alpha)
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, alpha = alpha)
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "stieltjes", tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, alpha = alpha)
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            alpha = alpha
        )
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "stieltjes", tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            alpha = alpha
        )
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            alpha = alpha
        )
    )

    expect_error(
        valueOf(AlphaStableBernsteinFunction(log2(2 - 1e-4)), x, difference_order = 1L), NA
    )
    expect_error(
        valueOf(AlphaStableBernsteinFunction(log2(2 - (1 - 1e-4))), x, difference_order = 1L), NA
    )
})


test_that("`valueOf` for `InverseGaussianBernsteinFunction`", {
    bf <- testutils.rmo::fuzzy_bf(InverseGaussianBernsteinFunction())
    eta <- bf@eta
    actual_fn <- function(x, eta) {
        sqrt(2 * x + eta^2) - eta
    }

    expect_equal(valueOf(bf, x), actual_fn(x, eta))
    expect_equal(
        valueOf(bf, x, method = "levy", tolerance = testthat_tolerance()),
        actual_fn(x, eta)
    )
    expect_equal(
        valueOf(bf, x, method = "stieltjes", tolerance = testthat_tolerance()),
        actual_fn(x, eta)
    )

    expect_equal(
        valueOf(bf, x, difference_order = difference_order, tolerance = testthat_tolerance()),
        value_of_naive(actual_fn, x, difference_order, eta = eta)
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy"
        ),
        tolerance = testthat_tolerance(),
        value_of_naive(actual_fn, x, difference_order, eta = eta)
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "stieltjes", tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, eta = eta)
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            eta = eta
        )
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "stieltjes", tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            eta = eta
        )
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            eta = eta
        )
    )
})


test_that("`valueOf` for ExponentialBernsteinFunction`", {
    bf <- testutils.rmo::fuzzy_bf(ExponentialBernsteinFunction())
    lambda <- bf@lambda
    actual_fn <- function(x, lambda) {
        x / (x + lambda)
    }

    expect_equal(valueOf(bf, x), actual_fn(x, lambda))
    expect_equal(
        valueOf(bf, x, method = "levy", tolerance = testthat_tolerance()),
        actual_fn(x, lambda)
    )
    expect_equal(
        valueOf(bf, x, method = "stieltjes", tolerance = testthat_tolerance()),
        actual_fn(x, lambda)
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, lambda = lambda)
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, lambda = lambda)
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "stieltjes", tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, lambda = lambda)
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            lambda = lambda
        )
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "stieltjes", tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            lambda = lambda
        )
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            lambda = lambda
        )
    )
})

test_that("`valueOf` for `GammaBernsteinFunction`", {
    bf <- testutils.rmo::fuzzy_bf(GammaBernsteinFunction())
    a <- bf@a
    actual_fn <- function(x, a) {
        log(1 + x / a)
    }

    expect_equal(valueOf(bf, x), actual_fn(x, a))
    expect_equal(
        valueOf(bf, x, method = "levy", tolerance = testthat_tolerance()),
        actual_fn(x, a)
    )
    expect_equal(
        valueOf(bf, x, method = "stieltjes", tolerance = testthat_tolerance()),
        actual_fn(x, a)
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, a = a)
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, a = a)
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "stieltjes", tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, a = a)
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            a = a
        )
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "stieltjes", tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            a = a
        )
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            a = a
        )
    )
})

test_that("`valueOf` for `ParetoBernsteinFunction`", {
    bf <- testutils.rmo::fuzzy_bf(ParetoBernsteinFunction())
    alpha <- bf@alpha
    x0 <- bf@x0
    actual_fn <- function(x, alpha, x0) {
        1 - exp(-x0 * x) +
            (x * x0)^(alpha) *
                pgamma(x0 * x, 1 - alpha, lower = FALSE) *
                gamma(1 - alpha)
    }

    expect_equal(valueOf(bf, x), actual_fn(x, alpha, x0))
    expect_equal(
        valueOf(bf, x, method = "levy", tolerance = testthat_tolerance()),
        actual_fn(x, alpha, x0)
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, alpha = alpha, x0 = x0)
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance()
        ),
        value_of_naive(actual_fn, x, difference_order, alpha = alpha, x0 = x0)
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            alpha = alpha, x0 = x0
        )
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        value_of_naive(actual_fn, x,
            difference_order = difference_order,
            n = n, k = k, cscale = cscale,
            alpha = alpha, x0 = x0
        )
    )
})




# #### Other `BernsteinFunction` classes ####

test_that("`valueOf` for `PoissonBernsteinFunction`", {
    bf <- testutils.rmo::fuzzy_bf(PoissonBernsteinFunction())
    eta <- bf@eta
    actual_fn <- function(x, eta) {
        1 - exp(-eta * x)
    }

    expect_equal(valueOf(bf, x), actual_fn(x, eta))
    expect_equal(
        valueOf(bf, x, method = "levy", tolerance = testthat_tolerance()),
        actual_fn(x, eta)
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            tolerance = testthat_tolerance()
        ),
        exp(-eta * x) * (1 - exp(-eta))^difference_order
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance()
        ),
        exp(-eta * x) * (1 - exp(-eta))^difference_order
    )

    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        choose(n, k) *
            exp(-eta * cscale * x) * (1 - exp(-eta * cscale))^difference_order
    )
    expect_equal(
        valueOf(bf, x,
            difference_order = difference_order,
            method = "levy", tolerance = testthat_tolerance(),
            n = n, k = k, cscale = cscale
        ),
        choose(n, k) *
            exp(-eta * cscale * x) * (1 - exp(-eta * cscale))^difference_order
    )
})
