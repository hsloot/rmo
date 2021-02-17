Integration tests
================

![packageversion](https://img.shields.io/badge/Package%20version-0.4.0-orange.svg?style=flat-square)
[![Last-changedate](https://img.shields.io/badge/last%20change-2021--02--17-yellowgreen.svg)](/commits/master)

## The overall minimum

For a Marshall–Olkin distributed random vector **τ** with parameters
*λ*<sub>*I*</sub>, ∅ ≠ *I* ⊆ {1, …, *d*}, we have that
min<sub>*i* ∈ {1, …, *d*}</sub>*τ*<sub>*i*</sub>
is exponentially distributed with rate
*λ* = ∑<sub>∅ ≠ *I* ⊆ {1, …, *d*}</sub>*λ*<sub>*I*</sub>.

In the following, we test this with a Kolmogorov-Smirnov test
(`ks.test`).

### Test for `rmo_esm`

``` r
d <- 10L

ks_test(n, d, rmo_esm, intensities_constant(d, constant = 0.3))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.036282, p-value = 0.1437
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_linear(d, scale = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.035678, p-value = 0.1567
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.014325, p-value = 0.9865
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_poisson(d, lambda = 0.3, eta = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.039199, p-value = 0.09255
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_alpha_stable(d, alpha = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.022701, p-value = 0.6813
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_exponential(d, lambda = 0.5))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.02785, p-value = 0.4199
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_gamma(d, a = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.028078, p-value = 0.4097
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_pareto(d, alpha = 0.4, x0 = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.018616, p-value = 0.8789
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_inverse_gaussian(d, eta = 0.1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.029515, p-value = 0.3484
#> alternative hypothesis: two-sided

ks_test(n, d, rmo_esm, intensities_constant(d, constant = 0.3), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.021808, p-value = 0.7284
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_linear(d, scale = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.029312, p-value = 0.3567
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.027318, p-value = 0.4445
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_poisson(d, lambda = 0.3, eta = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.039344, p-value = 0.09046
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_alpha_stable(d, alpha = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.019048, p-value = 0.8612
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_exponential(d, lambda = 0.5), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.029765, p-value = 0.3383
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_gamma(d, a = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.033873, p-value = 0.2014
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_pareto(d, alpha = 0.4, x0 = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.017689, p-value = 0.9131
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_esm, intensities_inverse_gaussian(d, eta = 0.1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.026982, p-value = 0.4604
#> alternative hypothesis: two-sided
```

### Test for `rmo_arnold`

``` r
d <- 10L

ks_test(n, d, rmo_arnold, intensities_constant(d, constant = 0.3))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.026023, p-value = 0.5073
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_linear(d, scale = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.024935, p-value = 0.563
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.019065, p-value = 0.8604
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_poisson(d, lambda = 0.3, eta = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.024261, p-value = 0.5983
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_alpha_stable(d, alpha = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.029265, p-value = 0.3586
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_exponential(d, lambda = 0.5))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.027324, p-value = 0.4442
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_gamma(d, a = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.037951, p-value = 0.1122
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_pareto(d, alpha = 0.4, x0 = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.025464, p-value = 0.5356
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_inverse_gaussian(d, eta = 0.1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.026383, p-value = 0.4895
#> alternative hypothesis: two-sided

ks_test(n, d, rmo_arnold, intensities_constant(d, constant = 0.3), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.031305, p-value = 0.281
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_linear(d, scale = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.051486, p-value = 0.009967
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.018432, p-value = 0.8861
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_poisson(d, lambda = 0.3, eta = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.023479, p-value = 0.6399
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_alpha_stable(d, alpha = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.013021, p-value = 0.9958
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_exponential(d, lambda = 0.5), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.026641, p-value = 0.4769
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_gamma(d, a = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.032075, p-value = 0.255
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_pareto(d, alpha = 0.4, x0 = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.042573, p-value = 0.0533
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_inverse_gaussian(d, eta = 0.1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.03007, p-value = 0.3264
#> alternative hypothesis: two-sided
```

### Test for `rexmo_markovian` with `d = 10`

``` r
d <- 10L

ex_ks_test(n, d, rexmo_markovian, ex_intensities_constant(d, constant = 0.3))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.035942, p-value = 0.1509
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_linear(d, scale = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.026694, p-value = 0.4743
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.032068, p-value = 0.2552
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_poisson(d, lambda = 0.3, eta = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.026338, p-value = 0.4917
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_alpha_stable(d, alpha = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.026433, p-value = 0.487
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_exponential(d, lambda = 0.5))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.019662, p-value = 0.8342
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_gamma(d, a = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.028713, p-value = 0.3818
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.019993, p-value = 0.8189
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_inverse_gaussian(d, eta = 0.1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.022671, p-value = 0.6829
#> alternative hypothesis: two-sided

ex_ks_test(n, d, rexmo_markovian, ex_intensities_constant(d, constant = 0.3), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.014153, p-value = 0.9882
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_linear(d, scale = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.016268, p-value = 0.954
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.025005, p-value = 0.5593
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_poisson(d, lambda = 0.3, eta = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.032837, p-value = 0.2311
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_alpha_stable(d, alpha = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.032911, p-value = 0.2289
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_exponential(d, lambda = 0.5), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.025134, p-value = 0.5526
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_gamma(d, a = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.039712, p-value = 0.08534
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.013555, p-value = 0.9929
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_inverse_gaussian(d, eta = 0.1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.020199, p-value = 0.8092
#> alternative hypothesis: two-sided
```

### Test for `rexmo_markovian` with `d = 125`

``` r
d <- 125L

ex_ks_test(n, d, rexmo_markovian, ex_intensities_constant(d, constant = 0.3))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.019775, p-value = 0.829
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_linear(d, scale = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.033294, p-value = 0.2176
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.049606, p-value = 0.01458
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_poisson(d, lambda = 0.3, eta = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.034026, p-value = 0.1972
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_alpha_stable(d, alpha = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.032751, p-value = 0.2337
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_exponential(d, lambda = 0.5))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.018992, p-value = 0.8635
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_gamma(d, a = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.029997, p-value = 0.3292
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.02915, p-value = 0.3633
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_inverse_gaussian(d, eta = 0.1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.030367, p-value = 0.315
#> alternative hypothesis: two-sided

ex_ks_test(n, d, rexmo_markovian, ex_intensities_constant(d, constant = 0.3), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.030146, p-value = 0.3235
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_linear(d, scale = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.018388, p-value = 0.8878
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.029783, p-value = 0.3377
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_poisson(d, lambda = 0.3, eta = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.0401, p-value = 0.08023
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_alpha_stable(d, alpha = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.031409, p-value = 0.2774
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_exponential(d, lambda = 0.5), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.044296, p-value = 0.03951
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_gamma(d, a = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.029509, p-value = 0.3486
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.018764, p-value = 0.8729
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rexmo_markovian, ex_intensities_inverse_gaussian(d, eta = 0.1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.036052, p-value = 0.1486
#> alternative hypothesis: two-sided
```

### Test `rmo_esm_cuadras_auge` with `d = 10`

``` r
d <- 10L

cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.034483, p-value = 0.1853
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.018891, p-value = 0.8677
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.021093, p-value = 0.7652
#> alternative hypothesis: two-sided

cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.027677, p-value = 0.4279
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.022126, p-value = 0.7118
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.046118, p-value = 0.02842
#> alternative hypothesis: two-sided
```

### Test `rmo_esm_cuadras_auge` with `d = 125`

``` r
d <- 125L

cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.024057, p-value = 0.6091
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.027069, p-value = 0.4562
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.023546, p-value = 0.6363
#> alternative hypothesis: two-sided

cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.01749, p-value = 0.9197
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.020532, p-value = 0.7931
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.028568, p-value = 0.3881
#> alternative hypothesis: two-sided
```

### Test `rmo_lfm_cpp` with `d = 10`

``` r
d <- 10L

lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.028047, p-value = 0.4111
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.027797, p-value = 0.4223
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.012979, p-value = 0.996
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.02466, p-value = 0.5773
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.028842, p-value = 0.3763
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.024097, p-value = 0.607
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.026141, p-value = 0.5014
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.01741, p-value = 0.9223
#> alternative hypothesis: two-sided

lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.036631, p-value = 0.1366
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.028933, p-value = 0.3724
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.040782, p-value = 0.07185
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.044265, p-value = 0.03973
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.020566, p-value = 0.7915
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.021723, p-value = 0.7328
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.038015, p-value = 0.1111
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.033596, p-value = 0.209
#> alternative hypothesis: two-sided
```

### Test `rmo_lfm_cpp` with `d = 125`

``` r
d <- 125L

lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.03032, p-value = 0.3168
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.017788, p-value = 0.9097
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.034297, p-value = 0.1901
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.019484, p-value = 0.8422
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.025405, p-value = 0.5387
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.031115, p-value = 0.2876
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.022078, p-value = 0.7143
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.022406, p-value = 0.697
#> alternative hypothesis: two-sided

lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.03464, p-value = 0.1813
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.022447, p-value = 0.6948
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.041612, p-value = 0.06267
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.019981, p-value = 0.8195
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.023402, p-value = 0.644
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.04129, p-value = 0.0661
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.057301, p-value = 0.002812
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.029986, p-value = 0.3297
#> alternative hypothesis: two-sided
```
