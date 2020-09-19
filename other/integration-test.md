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
    #> D = 0.0087721, p-value = 0.425
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_linear(d, scale = 0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0080876, p-value = 0.53
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_cuadras_auge(d, alpha = 0.6, beta=0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0083085, p-value = 0.4949
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_poisson(d, lambda = 0.3, eta=0.4))
    #> Warning in ks.test(scale * apply(FUN(n, d, intensities), 1, min), pexp): ties
    #> should not be present for the Kolmogorov-Smirnov test
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0075196, p-value = 0.6239
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_alpha_stable(d, alpha=0.4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0073382, p-value = 0.6544
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_exponential(d, lambda=0.5))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.010149, p-value = 0.2543
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_gamma(d, a=0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0094018, p-value = 0.3397
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_pareto(d, alpha=0.4, x0=1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.010727, p-value = 0.2001
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_esm, intensities_inverse_gaussian(d, eta=0.1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.011065, p-value = 0.1727
    #> alternative hypothesis: two-sided

### Test for `rmo_arnold`

    d <- 10L

    ks_test(n, d, rmo_arnold, intensities_constant(d, constant = 0.3))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0044813, p-value = 0.988
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_linear(d, scale = 0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0069035, p-value = 0.7272
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_cuadras_auge(d, alpha = 0.6, beta=0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0055693, p-value = 0.9157
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_poisson(d, lambda = 0.3, eta=0.4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0065462, p-value = 0.7848
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_alpha_stable(d, alpha=0.4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0094244, p-value = 0.3369
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_exponential(d, lambda=0.5))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0070064, p-value = 0.7102
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_gamma(d, a=0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.010076, p-value = 0.2619
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_pareto(d, alpha=0.4, x0=1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0089692, p-value = 0.397
    #> alternative hypothesis: two-sided
    ks_test(n, d, rmo_arnold, intensities_inverse_gaussian(d, eta=0.1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, intensities), 1, min)
    #> D = 0.0044204, p-value = 0.9897
    #> alternative hypothesis: two-sided

### Test for `rmo_ex_arnold` with `d = 10`

    d <- 10L

    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_constant(d, constant = 0.3))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0053409, p-value = 0.9379
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_linear(d, scale = 0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.007295, p-value = 0.6617
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_cuadras_auge(d, alpha = 0.6, beta=0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0078784, p-value = 0.564
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_poisson(d, lambda = 0.3, eta=0.4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0064326, p-value = 0.8024
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_alpha_stable(d, alpha=0.4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0071937, p-value = 0.6788
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_exponential(d, lambda=0.5))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0074589, p-value = 0.6341
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_gamma(d, a=0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0049257, p-value = 0.9685
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_pareto(d, alpha=0.4, x0=1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0077615, p-value = 0.5834
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta=0.1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0080887, p-value = 0.5298
    #> alternative hypothesis: two-sided

### Test for `rmo_ex_arnold` with `d = 125`

    d <- 125L

    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_constant(d, constant = 0.3))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0066587, p-value = 0.767
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_linear(d, scale = 0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0073234, p-value = 0.6569
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_cuadras_auge(d, alpha = 0.6, beta=0.2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.0069524, p-value = 0.7191
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
    #> D = 0.058764, p-value < 2.2e-16
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_exponential(d, lambda=0.5))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.32381, p-value < 2.2e-16
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_gamma(d, a=0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.16427, p-value < 2.2e-16
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_pareto(d, alpha=0.4, x0=1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.065773, p-value < 2.2e-16
    #> alternative hypothesis: two-sided
    ex_ks_test(n, d, rmo_ex_arnold, ex_intensities_inverse_gaussian(d, eta=0.1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  scale * apply(FUN(n, d, ex_intensities), 1, min)
    #> D = 0.043168, p-value < 2.2e-16
    #> alternative hypothesis: two-sided

### Test `rmo_esm_cuadras_auge` with `d = 10`

    d <- 10L

    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  (alpha * d + beta) * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.0077054, p-value = 0.5927
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  (alpha * d + beta) * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.0084865, p-value = 0.4674
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  (alpha * d + beta) * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.011434, p-value = 0.1463
    #> alternative hypothesis: two-sided

### Test `rmo_esm_cuadras_auge` with `d = 125`

    d <- 125L

    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.3, 0)
    #> Warning in ks.test((alpha * d + beta) * apply(FUN(n, d, alpha, beta), 1, : ties
    #> should not be present for the Kolmogorov-Smirnov test
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  (alpha * d + beta) * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.015187, p-value = 0.01984
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0, 0.3)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  (alpha * d + beta) * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.0097656, p-value = 0.296
    #> alternative hypothesis: two-sided
    cuadras_auge_ks_test(n, d, rmo_esm_cuadras_auge, 0.6, 0.3)
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  (alpha * d + beta) * apply(FUN(n, d, alpha, beta), 1, min)
    #> D = 0.0055913, p-value = 0.9134
    #> alternative hypothesis: two-sided

### Test `rmo_lfm_cpp` with `d = 10`

    d <- 10L

    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0094486, p-value = 0.3338
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0060839, p-value = 0.853
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0078832, p-value = 0.5632
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0057066, p-value = 0.9006
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.009982, p-value = 0.272
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.011943, p-value = 0.1153
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0066677, p-value = 0.7656
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
    #> Warning in ks.test(apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, :
    #> ties should not be present for the Kolmogorov-Smirnov test
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0064399, p-value = 0.8013
    #> alternative hypothesis: two-sided

### Test `rmo_lfm_cpp` with `d = 125`

    d <- 125L

    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0.3, 0, "rposval", list("value" = 1))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0066621, p-value = 0.7665
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0, 0, 0.4, "rposval", list("value" = 1))
    #> Warning in ks.test(apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, :
    #> ties should not be present for the Kolmogorov-Smirnov test
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0096052, p-value = 0.3148
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rposval", list("value" = 2))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0061816, p-value = 0.8394
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rposval", list("value" = 2))
    #> Warning in ks.test(apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, :
    #> ties should not be present for the Kolmogorov-Smirnov test
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0060219, p-value = 0.8614
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rexp", list("rate" = 0.6))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0064489, p-value = 0.7999
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rexp", list("rate" = 0.6))
    #> Warning in ks.test(apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, :
    #> ties should not be present for the Kolmogorov-Smirnov test
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.0078147, p-value = 0.5746
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0, 0, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.019872, p-value = 0.0007432
    #> alternative hypothesis: two-sided
    lfm_ks_test(n, d, rmo_lfm_cpp, 0.1, 0.2, 0.4, "rpareto", list("alpha" = 0.4, "x0" = 1e-4))
    #> 
    #>  One-sample Kolmogorov-Smirnov test
    #> 
    #> data:  apply(FUN(n, d, rate, rate_killing, rate_drift, rjump_name, rjump_arg_list), 1, min) * scale
    #> D = 0.009416, p-value = 0.3379
    #> alternative hypothesis: two-sided
