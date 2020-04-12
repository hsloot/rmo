context("Bernstein Function S4 class")

test_that("Test initialisation of BernsteinFunction classes", {
  expect_s4_class(ConstantBernsteinFunction(constant=0.5),
    class="ConstantBernsteinFunction")
  expect_s4_class(LinearBernsteinFunction(scale=0.4),
    class="LinearBernsteinFunction")
  expect_s4_class(PoissonBernsteinFunction(lambda=0.3, eta=0.2),
    class="PoissonBernsteinFunction")
  expect_s4_class(AlphaStableBernsteinFunction(alpha=0.6),
    class="AlphaStableBernsteinFunction")
  expect_s4_class(GammaBernsteinFunction(a=0.7),
    class="GammaBernsteinFunction")
  expect_s4_class(ScaledBernsteinFunction(scale=0.5,
    original=GammaBernsteinFunction(a=0.7)),
    class="ScaledBernsteinFunction")
  expect_s4_class(SumOfBernsteinFunctions(
    first=LinearBernsteinFunction(scale=0.2),
    second=ScaledBernsteinFunction(scale=0.5,
      original=AlphaStableBernsteinFunction(alpha=0.6))),
    class="SumOfBernsteinFunctions")
})
