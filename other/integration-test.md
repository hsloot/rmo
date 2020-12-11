Integration tests
================

![packageversion](https://img.shields.io/badge/Package%20version-0.2.6-orange.svg?style=flat-square)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--12--11-yellowgreen.svg)](/commits/master)

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
#> D = 0.024714, p-value = 0.5745
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_linear(d, scale = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.022538, p-value = 0.69
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.023793, p-value = 0.6231
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_poisson(d, lambda = 0.3, eta = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.028248, p-value = 0.4021
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_alpha_stable(d, alpha = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.022885, p-value = 0.6715
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_exponential(d, lambda = 0.5))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.036459, p-value = 0.1401
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_gamma(d, a = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.02014, p-value = 0.812
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.044431, p-value = 0.03858
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta = 0.1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.033847, p-value = 0.2021
#> alternative hypothesis: two-sided

ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_constant(d, constant = 0.3), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.02485, p-value = 0.5674
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_linear(d, scale = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.03423, p-value = 0.1918
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.017181, p-value = 0.9294
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_poisson(d, lambda = 0.3, eta = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.031727, p-value = 0.2665
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_alpha_stable(d, alpha = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.036162, p-value = 0.1462
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_exponential(d, lambda = 0.5), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.021222, p-value = 0.7587
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_gamma(d, a = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.018069, p-value = 0.8998
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.025567, p-value = 0.5304
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta = 0.1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.02865, p-value = 0.3845
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
#> D = 0.023354, p-value = 0.6465
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_linear(d, scale = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.029479, p-value = 0.3498
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.024526, p-value = 0.5843
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_poisson(d, lambda = 0.3, eta = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.017669, p-value = 0.9138
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_alpha_stable(d, alpha = 0.4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.026218, p-value = 0.4976
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_exponential(d, lambda = 0.5))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.015571, p-value = 0.9686
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_gamma(d, a = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.018642, p-value = 0.8778
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.020826, p-value = 0.7786
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta = 0.1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.020288, p-value = 0.805
#> alternative hypothesis: two-sided

ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_constant(d, constant = 0.3), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.022065, p-value = 0.715
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_linear(d, scale = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.019575, p-value = 0.8382
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.042986, p-value = 0.04967
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_poisson(d, lambda = 0.3, eta = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.057279, p-value = 0.002827
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_alpha_stable(d, alpha = 0.4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.026052, p-value = 0.5059
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_exponential(d, lambda = 0.5), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.023103, p-value = 0.6599
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_gamma(d, a = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.018769, p-value = 0.8727
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.025224, p-value = 0.548
#> alternative hypothesis: two-sided
ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta = 0.1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
#> D = 0.03578, p-value = 0.1545
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
#> D = 0.041218, p-value = 0.06689
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.024012, p-value = 0.6115
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.019822, p-value = 0.8269
#> alternative hypothesis: two-sided

cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.021487, p-value = 0.7451
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.021357, p-value = 0.7518
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.03602, p-value = 0.1492
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
#> D = 0.023826, p-value = 0.6214
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.031212, p-value = 0.2842
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.024466, p-value = 0.5875
#> alternative hypothesis: two-sided

cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.0217, p-value = 0.7341
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.024245, p-value = 0.5991
#> alternative hypothesis: two-sided
cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3, normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
#> D = 0.022881, p-value = 0.6717
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
#> D = 0.018143, p-value = 0.897
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.017968, p-value = 0.9034
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.018995, p-value = 0.8634
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.013042, p-value = 0.9957
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.017214, p-value = 0.9284
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.023014, p-value = 0.6647
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.029963, p-value = 0.3306
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.02219, p-value = 0.7084
#> alternative hypothesis: two-sided

lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.02669, p-value = 0.4744
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.019581, p-value = 0.8379
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.020816, p-value = 0.7791
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.034773, p-value = 0.178
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.018515, p-value = 0.8829
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.026455, p-value = 0.4859
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.025778, p-value = 0.5197
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.022643, p-value = 0.6844
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
#> D = 0.031764, p-value = 0.2652
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.016526, p-value = 0.9476
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.028202, p-value = 0.4041
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.025181, p-value = 0.5502
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.028229, p-value = 0.4029
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.018768, p-value = 0.8728
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.020832, p-value = 0.7783
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.022231, p-value = 0.7062
#> alternative hypothesis: two-sided

lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.02443, p-value = 0.5894
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.029012, p-value = 0.3691
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.030023, p-value = 0.3282
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.021311, p-value = 0.7541
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.021804, p-value = 0.7286
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.026692, p-value = 0.4744
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.017314, p-value = 0.9253
#> alternative hypothesis: two-sided
lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
#> Warning in ks.test(apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, :
#> ties should not be present for the Kolmogorov-Smirnov test
#> 
#>  One-sample Kolmogorov-Smirnov test
#> 
#> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
#> D = 0.023037, p-value = 0.6634
#> alternative hypothesis: two-sided
```
