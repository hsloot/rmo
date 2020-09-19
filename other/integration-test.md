Integration tests
================

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
    #> D = 0.0099132, p-value = 0.2795
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_linear(d, scale = 0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0090191, p-value = 0.3901
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_cuadras_auge(d, alpha = 0.6, beta=0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.008895, p-value = 0.4074
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_poisson(d, lambda = 0.3, eta=0.4))
    #> Warning in ks.test(scale * apply(FUN(n, d, intensities), 1, min), pexp): ties
    #> should not be present for the Kolmogorov-Smirnov test
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0051629, p-value = 0.9526
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_alpha_stable(d, alpha=0.4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0060342, p-value = 0.8597
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_exponential(d, lambda=0.5))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0077851, p-value = 0.5795
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_gamma(d, a=0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0078169, p-value = 0.5742
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_pareto(d, alpha=0.4, x0=1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0066057, p-value = 0.7755
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_inverse_gaussian(d, eta=0.1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0088816, p-value = 0.4093
    #> alternative hypothesis: two-sided

### Test for `rmo_arnold`

    d <- 10L

    ks_test(n, d, rmo_arnold, intensities_constant(d, constant = 0.3))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.012994, p-value = 0.06829
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_linear(d, scale = 0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0099317, p-value = 0.2774
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_cuadras_auge(d, alpha = 0.6, beta=0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.013308, p-value = 0.05789
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_poisson(d, lambda = 0.3, eta=0.4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0078823, p-value = 0.5634
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_alpha_stable(d, alpha=0.4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0095864, p-value = 0.317
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_exponential(d, lambda=0.5))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0074581, p-value = 0.6342
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_gamma(d, a=0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0083873, p-value = 0.4826
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_pareto(d, alpha=0.4, x0=1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0107, p-value = 0.2023
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_inverse_gaussian(d, eta=0.1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.011374, p-value = 0.1504
    #> alternative hypothesis: two-sided

### Test for `rmo_ex_arnold` with `d = 10`

    d <- 10L

    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_constant(d, constant = 0.3))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0078811, p-value = 0.5636
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_linear(d, scale = 0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.012575, p-value = 0.08464
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_cuadras_auge(d, alpha = 0.6, beta=0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0073724, p-value = 0.6487
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_poisson(d, lambda = 0.3, eta=0.4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0095825, p-value = 0.3175
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_alpha_stable(d, alpha=0.4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.00888, p-value = 0.4095
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_exponential(d, lambda=0.5))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0081944, p-value = 0.5129
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_gamma(d, a=0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.006751, p-value = 0.7522
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_pareto(d, alpha=0.4, x0=1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.008749, p-value = 0.4283
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta=0.1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0057175, p-value = 0.8993
    #> alternative hypothesis: two-sided

### Test for `rmo_ex_arnold` with `d = 125`

    d <- 125L

    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_constant(d, constant = 0.3))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.005573, p-value = 0.9153
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_linear(d, scale = 0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0087822, p-value = 0.4235
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_cuadras_auge(d, alpha = 0.6, beta=0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0082596, p-value = 0.5025
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_poisson(d, lambda = 0.3, eta=0.4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 1, p-value < 2.2e-16
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_alpha_stable(d, alpha=0.4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.059381, p-value < 2.2e-16
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_exponential(d, lambda=0.5))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.32825, p-value < 2.2e-16
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_gamma(d, a=0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.16897, p-value < 2.2e-16
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_pareto(d, alpha=0.4, x0=1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.064089, p-value < 2.2e-16
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta=0.1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.052456, p-value < 2.2e-16
    #> alternative hypothesis: two-sided

### Test `rmo_esm_cuadras_auge` with `d = 10`

    d <- 10L

    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  (alpha * d + beta) * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.010507, p-value = 0.2196
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  (alpha * d + beta) * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.0048306, p-value = 0.9738
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  (alpha * d + beta) * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.0065953, p-value = 0.7771
    #> alternative hypothesis: two-sided

### Test `rmo_esm_cuadras_auge` with `d = 125`

    d <- 125L

    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  (alpha * d + beta) * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.0066051, p-value = 0.7756
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  (alpha * d + beta) * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.0081785, p-value = 0.5154
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  (alpha * d + beta) * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.006143, p-value = 0.8448
    #> alternative hypothesis: two-sided

### Test `rmo_lfm_cpp` with `d = 10`

    d <- 10L

    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0061259, p-value = 0.8472
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0069704, p-value = 0.7162
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0058249, p-value = 0.8866
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0067881, p-value = 0.7462
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0089002, p-value = 0.4067
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0085816, p-value = 0.453
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0084993, p-value = 0.4654
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
    #> Warning in ks.test(apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, :
    #> ties should not be present for the Kolmogorov-Smirnov test
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0093975, p-value = 0.3402
    #> alternative hypothesis: two-sided

### Test `rmo_lfm_cpp` with `d = 125`

    d <- 125L

    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.010421, p-value = 0.2276
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1))
    #> Warning in ks.test(apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, :
    #> ties should not be present for the Kolmogorov-Smirnov test
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.011471, p-value = 0.1439
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.013362, p-value = 0.05625
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2))
    #> Warning in ks.test(apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, :
    #> ties should not be present for the Kolmogorov-Smirnov test
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0071391, p-value = 0.688
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0061082, p-value = 0.8496
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.006008, p-value = 0.8632
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.014959, p-value = 0.02277
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0071606, p-value = 0.6844
    #> alternative hypothesis: two-sided
