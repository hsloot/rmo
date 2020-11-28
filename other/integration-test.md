Integration tests
================

![packageversion](https://img.shields.io/badge/Package%20version-0.2.5.9000-orange.svg?style=flat-square)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--11--29-yellowgreen.svg)](/commits/master)

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
#> D = 0.033871, p-value = 0.2014
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
#> D = 0.031997, p-value = 0.2575
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_linear(d, scale = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.020938, p-value = 0.773
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.021681, p-value = 0.735
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_poisson(d, lambda = 0.3, eta = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.025579, p-value = 0.5298
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_alpha_stable(d, alpha = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.021559, p-value = 0.7414
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_exponential(d, lambda = 0.5))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.014325, p-value = 0.9865
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_gamma(d, a = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.021625, p-value = 0.7379
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_pareto(d, alpha = 0.4, x0 = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.01982, p-value = 0.827
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_inverse_gaussian(d, eta = 0.1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.024684, p-value = 0.576
#> alternative hypothesis: two-sided

ks_test(n, d, rmo_arnold, intensities_constant(d, constant = 0.3), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.027411, p-value = 0.4402
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_linear(d, scale = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.017813, p-value = 0.9088
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.031268, p-value = 0.2822
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_poisson(d, lambda = 0.3, eta = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.03063, p-value = 0.3052
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_alpha_stable(d, alpha = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.03275, p-value = 0.2337
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_exponential(d, lambda = 0.5), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.026292, p-value = 0.4939
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_gamma(d, a = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.025818, p-value = 0.5176
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_pareto(d, alpha = 0.4, x0 = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.031074, p-value = 0.2891
#> alternative hypothesis: two-sided
ks_test(n, d, rmo_arnold, intensities_inverse_gaussian(d, eta = 0.1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, intensities), 1, min)
#> D = 0.026316, p-value = 0.4928
#> alternative hypothesis: two-sided
```

### Test for `rmo_ex_arnold` with `d = 10`

``` r
d <- 10L

ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_constant(d, constant = 0.3))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.032708, p-value = 0.235
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_linear(d, scale = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.017169, p-value = 0.9297
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.03056, p-value = 0.3078
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_poisson(d, lambda = 0.3, eta = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.023334, p-value = 0.6476
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_alpha_stable(d, alpha = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.028151, p-value = 0.4064
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_exponential(d, lambda = 0.5))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.032847, p-value = 0.2308
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_gamma(d, a = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.03272, p-value = 0.2347
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.02176, p-value = 0.7309
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta = 0.1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.032392, p-value = 0.2448
#> alternative hypothesis: two-sided

ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_constant(d, constant = 0.3), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.021804, p-value = 0.7286
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_linear(d, scale = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.018679, p-value = 0.8764
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.019447, p-value = 0.8439
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_poisson(d, lambda = 0.3, eta = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.038687, p-value = 0.1002
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_alpha_stable(d, alpha = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.035325, p-value = 0.1648
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_exponential(d, lambda = 0.5), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.028333, p-value = 0.3983
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_gamma(d, a = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.021774, p-value = 0.7302
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.020728, p-value = 0.7835
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta = 0.1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.030609, p-value = 0.306
#> alternative hypothesis: two-sided
```

### Test for `rmo_ex_arnold` with `d = 125`

``` r
d <- 125L

ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_constant(d, constant = 0.3))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.047726, p-value = 0.02102
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_linear(d, scale = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.021516, p-value = 0.7436
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.025, p-value = 0.5595
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_poisson(d, lambda = 0.3, eta = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.024216, p-value = 0.6007
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_alpha_stable(d, alpha = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.022049, p-value = 0.7158
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_exponential(d, lambda = 0.5))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.028076, p-value = 0.4097
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_gamma(d, a = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.017721, p-value = 0.912
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.018434, p-value = 0.886
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta = 0.1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.02062, p-value = 0.7888
#> alternative hypothesis: two-sided

ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_constant(d, constant = 0.3), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.03938, p-value = 0.08994
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_linear(d, scale = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.032457, p-value = 0.2428
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.02525, p-value = 0.5466
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_poisson(d, lambda = 0.3, eta = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.039911, p-value = 0.08269
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_alpha_stable(d, alpha = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.025462, p-value = 0.5357
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_exponential(d, lambda = 0.5), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.026355, p-value = 0.4909
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_gamma(d, a = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.02883, p-value = 0.3768
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.032808, p-value = 0.232
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta = 0.1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.022454, p-value = 0.6944
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
#> D = 0.02438, p-value = 0.592
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.033524, p-value = 0.211
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.025832, p-value = 0.517
#> alternative hypothesis: two-sided

cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.033897, p-value = 0.2007
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.035895, p-value = 0.152
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.050807, p-value = 0.01145
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
#> D = 0.030001, p-value = 0.3291
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.021248, p-value = 0.7573
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.018363, p-value = 0.8888
#> alternative hypothesis: two-sided

cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.0357, p-value = 0.1563
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.025235, p-value = 0.5474
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.035052, p-value = 0.1712
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
#> D = 0.024628, p-value = 0.579
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.023417, p-value = 0.6431
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.031145, p-value = 0.2866
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.023795, p-value = 0.623
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.028306, p-value = 0.3995
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.020666, p-value = 0.7865
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.02345, p-value = 0.6414
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.028562, p-value = 0.3883
#> alternative hypothesis: two-sided

lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.029056, p-value = 0.3672
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.041143, p-value = 0.06772
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.019426, p-value = 0.8448
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.029828, p-value = 0.3359
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.021867, p-value = 0.7253
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.0309, p-value = 0.2953
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.02747, p-value = 0.4374
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.018171, p-value = 0.896
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
#> D = 0.023183, p-value = 0.6557
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.02562, p-value = 0.5277
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.028296, p-value = 0.4
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.039368, p-value = 0.09012
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.030491, p-value = 0.3104
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.024345, p-value = 0.5939
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.020214, p-value = 0.8085
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.027878, p-value = 0.4187
#> alternative hypothesis: two-sided

lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.019333, p-value = 0.8489
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.013624, p-value = 0.9924
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.030949, p-value = 0.2936
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.019706, p-value = 0.8322
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.034464, p-value = 0.1858
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.026869, p-value = 0.4658
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.016314, p-value = 0.9528
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
#> Warning in ks.test(apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, :
#> ties should not be present for the Kolmogorov-Smirnov test
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.030417, p-value = 0.3131
#> alternative hypothesis: two-sided
```
