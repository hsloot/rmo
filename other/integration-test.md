Integration tests
================

![packageversion](https://img.shields.io/badge/Package%20version-0.2.5-orange.svg?style=flat-square)
[![Last-changedate](https://img.shields.io/badge/last%20change-2020--09--22-yellowgreen.svg)](/commits/master)

The overall minimum
-------------------

For a Marshall–Olkin distributed random vector **τ** with parameters
*λ*<sub>*I*</sub>, ∅ ≠ *I* ⊆ {1, …, *d*}, we have that
min<sub>*i* ∈ {1, …, *d*}</sub>*τ*<sub>*i*</sub>
is exponentially distributed with rate
*λ* = ∑<sub>∅ ≠ *I* ⊆ {1, …, *d*}</sub>*λ*<sub>*I*</sub>.

In the following, we test this with a Kolmogorov-Smirnov test
(`ks.test`).

### Test for `rmo_esm`

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
    #> D = 0.018614, p-value = 0.879
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
    #> D = 0.017691, p-value = 0.913
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_inverse_gaussian(d, eta = 0.1), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.026982, p-value = 0.4604
    #> alternative hypothesis: two-sided

### Test for `rmo_arnold`

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
    #> D = 0.023564, p-value = 0.6353
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

### Test for `rmo_ex_arnold` with `d = 10`

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

### Test for `rmo_ex_arnold` with `d = 125`

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
    #> D = 0.023499, p-value = 0.6388
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
    #> D = 0.017915, p-value = 0.9053
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta = 0.1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.021505, p-value = 0.7442
    #> alternative hypothesis: two-sided

    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_constant(d, constant = 0.3), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.03538, p-value = 0.1635
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_linear(d, scale = 0.2), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.035745, p-value = 0.1552
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_cuadras_auge(d, alpha = 0.6, beta = 0.2), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.02625, p-value = 0.496
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_poisson(d, lambda = 0.3, eta = 0.4), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.035911, p-value = 0.1516
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_alpha_stable(d, alpha = 0.4), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.023077, p-value = 0.6613
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_exponential(d, lambda = 0.5), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.023708, p-value = 0.6277
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_gamma(d, a = 0.6), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.03083, p-value = 0.2979
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_pareto(d, alpha = 0.4, x0 = 1e-4), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.028739, p-value = 0.3807
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta = 0.1), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.021355, p-value = 0.7519
    #> alternative hypothesis: two-sided

### Test `rmo_esm_cuadras_auge` with `d = 10`

    d <- 10L

    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.029696, p-value = 0.3411
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.031444, p-value = 0.2762
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.022474, p-value = 0.6934
    #> alternative hypothesis: two-sided

    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0, normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.027268, p-value = 0.4468
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3, normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.029019, p-value = 0.3688
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3, normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.048141, p-value = 0.01941
    #> alternative hypothesis: two-sided

### Test `rmo_esm_cuadras_auge` with `d = 125`

    d <- 125L

    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.039128, p-value = 0.09358
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.036383, p-value = 0.1416
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.012662, p-value = 0.9971
    #> alternative hypothesis: two-sided

    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0, normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.022197, p-value = 0.708
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3, normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.027944, p-value = 0.4157
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3, normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.031454, p-value = 0.2758
    #> alternative hypothesis: two-sided

### Test `rmo_lfm_cpp` with `d = 10`

    d <- 10L

    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.02386, p-value = 0.6196
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.02602, p-value = 0.5075
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.023808, p-value = 0.6223
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.020598, p-value = 0.7899
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.023253, p-value = 0.6519
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.024622, p-value = 0.5793
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.023941, p-value = 0.6153
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.020562, p-value = 0.7917
    #> alternative hypothesis: two-sided

    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.021094, p-value = 0.7651
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.025445, p-value = 0.5366
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.030426, p-value = 0.3128
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.036421, p-value = 0.1408
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.025107, p-value = 0.554
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.040951, p-value = 0.06988
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.02647, p-value = 0.4852
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.021774, p-value = 0.7302
    #> alternative hypothesis: two-sided

### Test `rmo_lfm_cpp` with `d = 125`

    d <- 125L

    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.036738, p-value = 0.1345
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.026541, p-value = 0.4817
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.015334, p-value = 0.9728
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.036368, p-value = 0.1419
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
    #> D = 0.020832, p-value = 0.7783
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.022878, p-value = 0.6719
    #> alternative hypothesis: two-sided

    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.022927, p-value = 0.6693
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.015726, p-value = 0.9656
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.043372, p-value = 0.04646
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.018706, p-value = 0.8753
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.021189, p-value = 0.7603
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6), normalize = TRUE)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.021869, p-value = 0.7252
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
    #> D = 0.034536, p-value = 0.184
    #> alternative hypothesis: two-sided
