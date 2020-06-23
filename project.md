Stat 357 Project - Parkinson’s Disease
================
Prahar Ijner

### Functions

``` r
summarize = function(data){
  c(min = min(data), max = max(data), mean = mean(data), SD = sd(data))
}

analyzeModel = function(model){
  print(summary(model))
  sig = summary(model)$sig
  par(mfrow = c(2, 3))
  
  # Model assumptions
  # Homoscedasticity
  plot(model, 1)
  abline(h=c(-1, 1)*sig, lty=3)
  abline(h=c(-2, 2)*sig, lty=2)
  abline(h=c(-3, 3)*sig, lty=1)
  
  # Q-Q norm plot
  plot(model, 2)
  
  plot(model, 3)
  
  # Cook's Distance
  plot(model, 4)
  abline(h=3*sd(cooks.distance(model)))
  
  # Leverage plots
  plot(model, 5)
  plot(model, 6)
}
```

### Read and summarize data

``` r
pdData = read.table("parkinsons_motor_updrs.dat", header = TRUE, sep = '\t')
t(apply(pdData, 2, summarize))
```

    ##                       min        max         mean           SD
    ## subject        4.0000e+00 5.9980e+03 3.221005e+03 1.825412e+03
    ## age            3.6000e+01 8.5000e+01 6.440476e+01 9.150678e+00
    ## sex            0.0000e+00 1.0000e+00 3.333333e-01 4.725309e-01
    ## test_time      4.9861e-01 2.0853e+02 9.001205e+01 5.638123e+01
    ## motor_UPDRS    5.1376e+00 3.9511e+01 2.132383e+01 8.360711e+00
    ## Jitter.Percent 1.6700e-03 3.5880e-02 6.176286e-03 4.724752e-03
    ## Jitter.Abs     6.5100e-06 3.8918e-04 4.450714e-05 3.947725e-05
    ## Jitter.RAP     6.9000e-04 1.9820e-02 3.045143e-03 2.605899e-03
    ## Jitter.PPQ5    8.8000e-04 2.9830e-02 3.244714e-03 2.936720e-03
    ## Jitter.DDP     2.0800e-03 5.9470e-02 9.135619e-03 7.818232e-03
    ## Shimmer        8.3000e-03 1.7591e-01 3.349124e-02 2.545494e-02
    ## Shimmer.dB     8.2000e-02 1.5130e+00 3.057952e-01 2.250864e-01
    ## Shimmer.APQ3   3.6000e-03 1.0051e-01 1.703424e-02 1.369773e-02
    ## Shimmer.APQ5   4.1700e-03 1.1553e-01 1.969067e-02 1.624932e-02
    ## Shimmer.APQ11  6.4700e-03 1.2993e-01 2.669557e-02 1.943050e-02
    ## Shimmer.DDA    1.0790e-02 3.0154e-01 5.110257e-02 4.109396e-02
    ## NHR            1.5770e-03 3.9251e-01 3.118085e-02 4.209122e-02
    ## HNR            4.5090e+00 3.2505e+01 2.192228e+01 4.348618e+00
    ## RPDE           2.7694e-01 7.8395e-01 5.410531e-01 9.967906e-02
    ## DFA            5.1977e-01 8.1420e-01 6.465373e-01 7.379235e-02
    ## PPE            6.3115e-02 5.3421e-01 2.211805e-01 9.257823e-02

### Initial analysis

``` r
X = as.matrix(pdData[, c(2, 3, 4, 6:ncol(pdData))])
Y = as.matrix(pdData[, 5])
n = nrow(X)
p = ncol(X)

# Plot measurement vs Y graph for each measurement
# par(mfrow=c(5, 4))
for (i in 1:p){
  x = X[, i]
  model = lm(Y~x)
  
  print(summary(model))
  
  plot(x, Y, main = paste(colnames(X)[i], " vs motor_UPDRS"), ylab = 'motor_UPDRS', xlab = colnames(X)[i] )
}
```

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.3213  -6.8807  -0.4899   6.8305  18.2480 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.70468    3.99174   1.680 0.094529 .  
    ## x            0.22699    0.06137   3.699 0.000277 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.118 on 208 degrees of freedom
    ## Multiple R-squared:  0.06172,    Adjusted R-squared:  0.05721 
    ## F-statistic: 13.68 on 1 and 208 DF,  p-value: 0.0002771

![](project_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -16.261  -6.399  -0.769   6.330  18.113 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  21.3985     0.7082  30.213   <2e-16 ***
    ## x            -0.2240     1.2267  -0.183    0.855    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.38 on 208 degrees of freedom
    ## Multiple R-squared:  0.0001603,  Adjusted R-squared:  -0.004647 
    ## F-statistic: 0.03334 on 1 and 208 DF,  p-value: 0.8553

![](project_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -18.333  -6.628  -1.286   6.224  16.953 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 18.49693    1.06658  17.342  < 2e-16 ***
    ## x            0.03141    0.01005   3.125  0.00203 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.191 on 208 degrees of freedom
    ## Multiple R-squared:  0.04485,    Adjusted R-squared:  0.04026 
    ## F-statistic: 9.768 on 1 and 208 DF,  p-value: 0.00203

![](project_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.685  -6.092  -1.001   5.910  19.073 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  19.6273     0.9417  20.842   <2e-16 ***
    ## x           274.6853   121.2093   2.266   0.0245 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.279 on 208 degrees of freedom
    ## Multiple R-squared:  0.0241, Adjusted R-squared:  0.0194 
    ## F-statistic: 5.136 on 1 and 208 DF,  p-value: 0.02447

![](project_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -15.8736  -5.9987  -0.9889   6.5125  18.6477 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2.041e+01  8.686e-01  23.500   <2e-16 ***
    ## x           2.048e+04  1.462e+04   1.401    0.163    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.342 on 208 degrees of freedom
    ## Multiple R-squared:  0.00935,    Adjusted R-squared:  0.004587 
    ## F-statistic: 1.963 on 1 and 208 DF,  p-value: 0.1627

![](project_files/figure-gfm/unnamed-chunk-3-5.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -15.7756  -6.5613  -0.5779   6.3960  18.9484 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  19.8616     0.8807  22.553   <2e-16 ***
    ## x           480.2004   219.9552   2.183   0.0301 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.286 on 208 degrees of freedom
    ## Multiple R-squared:  0.0224, Adjusted R-squared:  0.0177 
    ## F-statistic: 4.766 on 1 and 208 DF,  p-value: 0.03014

![](project_files/figure-gfm/unnamed-chunk-3-6.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -15.8592  -5.9675  -0.6331   6.5195  18.7648 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  20.1105     0.8555  23.508   <2e-16 ***
    ## x           373.9297   195.6907   1.911   0.0574 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.308 on 208 degrees of freedom
    ## Multiple R-squared:  0.01725,    Adjusted R-squared:  0.01253 
    ## F-statistic: 3.651 on 1 and 208 DF,  p-value: 0.0574

![](project_files/figure-gfm/unnamed-chunk-3-7.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -15.7757  -6.5627  -0.5788   6.3950  18.9482 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  19.8619     0.8807  22.553   <2e-16 ***
    ## x           160.0293    73.3137   2.183   0.0302 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.286 on 208 degrees of freedom
    ## Multiple R-squared:  0.02239,    Adjusted R-squared:  0.01769 
    ## F-statistic: 4.765 on 1 and 208 DF,  p-value: 0.03017

![](project_files/figure-gfm/unnamed-chunk-3-8.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -16.114  -6.194  -0.562   6.376  18.262 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  19.7643     0.9475  20.859   <2e-16 ***
    ## x            46.5646    22.5440   2.065   0.0401 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.296 on 208 degrees of freedom
    ## Multiple R-squared:  0.0201, Adjusted R-squared:  0.01539 
    ## F-statistic: 4.266 on 1 and 208 DF,  p-value: 0.04012

![](project_files/figure-gfm/unnamed-chunk-3-9.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.1859  -6.1363  -0.5731   6.2585  18.3748 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  19.6259     0.9661   20.31   <2e-16 ***
    ## x             5.5526     2.5466    2.18   0.0303 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.287 on 208 degrees of freedom
    ## Multiple R-squared:  0.02235,    Adjusted R-squared:  0.01765 
    ## F-statistic: 4.754 on 1 and 208 DF,  p-value: 0.03035

![](project_files/figure-gfm/unnamed-chunk-3-10.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -16.048  -6.157  -0.321   6.583  18.118 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  19.9841     0.9165  21.804   <2e-16 ***
    ## x            78.6489    41.9689   1.874   0.0623 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.311 on 208 degrees of freedom
    ## Multiple R-squared:  0.0166, Adjusted R-squared:  0.01188 
    ## F-statistic: 3.512 on 1 and 208 DF,  p-value: 0.06234

![](project_files/figure-gfm/unnamed-chunk-3-11.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -15.5572  -6.1451  -0.5107   6.3325  18.0944 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  19.8735     0.9005  22.068   <2e-16 ***
    ## x            73.6566    35.3086   2.086   0.0382 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.294 on 208 degrees of freedom
    ## Multiple R-squared:  0.02049,    Adjusted R-squared:  0.01578 
    ## F-statistic: 4.352 on 1 and 208 DF,  p-value: 0.03819

![](project_files/figure-gfm/unnamed-chunk-3-12.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.223  -6.305  -1.163   5.924  18.368 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  19.2070     0.9674  19.854  < 2e-16 ***
    ## x            79.2959    29.3241   2.704  0.00742 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.237 on 208 degrees of freedom
    ## Multiple R-squared:  0.03396,    Adjusted R-squared:  0.02932 
    ## F-statistic: 7.312 on 1 and 208 DF,  p-value: 0.007416

![](project_files/figure-gfm/unnamed-chunk-3-13.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -16.049  -6.157  -0.321   6.583  18.119 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  19.9840     0.9165  21.805   <2e-16 ***
    ## x            26.2178    13.9893   1.874   0.0623 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.311 on 208 degrees of freedom
    ## Multiple R-squared:  0.01661,    Adjusted R-squared:  0.01188 
    ## F-statistic: 3.512 on 1 and 208 DF,  p-value: 0.06231

![](project_files/figure-gfm/unnamed-chunk-3-14.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -15.7549  -6.1964  -0.6019   6.6503  18.6777 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  20.5351     0.7145  28.742   <2e-16 ***
    ## x            25.2955    13.6606   1.852   0.0655 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.313 on 208 degrees of freedom
    ## Multiple R-squared:  0.01622,    Adjusted R-squared:  0.01149 
    ## F-statistic: 3.429 on 1 and 208 DF,  p-value: 0.06548

![](project_files/figure-gfm/unnamed-chunk-3-15.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -16.135  -6.227  -1.042   6.564  18.001 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  31.3601     2.8934  10.838  < 2e-16 ***
    ## x            -0.4578     0.1295  -3.536 0.000501 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.14 on 208 degrees of freedom
    ## Multiple R-squared:  0.0567, Adjusted R-squared:  0.05217 
    ## F-statistic:  12.5 on 1 and 208 DF,  p-value: 0.0005009

![](project_files/figure-gfm/unnamed-chunk-3-16.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -14.210  -6.436  -1.105   6.554  18.262 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   14.059      3.158   4.452 1.39e-05 ***
    ## x             13.428      5.741   2.339   0.0203 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.273 on 208 degrees of freedom
    ## Multiple R-squared:  0.02563,    Adjusted R-squared:  0.02095 
    ## F-statistic: 5.471 on 1 and 208 DF,  p-value: 0.02028

![](project_files/figure-gfm/unnamed-chunk-3-17.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -16.547  -6.828  -1.502   5.788  17.476 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   32.804      5.049   6.497 5.93e-10 ***
    ## x            -17.757      7.759  -2.289   0.0231 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.277 on 208 degrees of freedom
    ## Multiple R-squared:  0.02456,    Adjusted R-squared:  0.01987 
    ## F-statistic: 5.238 on 1 and 208 DF,  p-value: 0.02311

![](project_files/figure-gfm/unnamed-chunk-3-18.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.504  -6.561  -1.332   6.351  19.307 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   17.241      1.469  11.735  < 2e-16 ***
    ## x             18.459      6.130   3.011  0.00292 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 8.204 on 208 degrees of freedom
    ## Multiple R-squared:  0.04178,    Adjusted R-squared:  0.03717 
    ## F-statistic: 9.069 on 1 and 208 DF,  p-value: 0.002922

![](project_files/figure-gfm/unnamed-chunk-3-19.png)<!-- -->

### Correlation between jitter and shimmer measurements

``` r
# correlation between jitter measurenets
rcorr(X[, 4:8])
```

    ##                Jitter.Percent Jitter.Abs Jitter.RAP Jitter.PPQ5 Jitter.DDP
    ## Jitter.Percent           1.00       0.89       0.98        0.93       0.98
    ## Jitter.Abs               0.89       1.00       0.90        0.72       0.90
    ## Jitter.RAP               0.98       0.90       1.00        0.88       1.00
    ## Jitter.PPQ5              0.93       0.72       0.88        1.00       0.88
    ## Jitter.DDP               0.98       0.90       1.00        0.88       1.00
    ## 
    ## n= 210 
    ## 
    ## 
    ## P
    ##                Jitter.Percent Jitter.Abs Jitter.RAP Jitter.PPQ5 Jitter.DDP
    ## Jitter.Percent                 0          0          0           0        
    ## Jitter.Abs      0                         0          0           0        
    ## Jitter.RAP      0              0                     0           0        
    ## Jitter.PPQ5     0              0          0                      0        
    ## Jitter.DDP      0              0          0          0

``` r
# correlation between shimmer measurements
rcorr(X[, 9:13])
```

    ##               Shimmer Shimmer.dB Shimmer.APQ3 Shimmer.APQ5 Shimmer.APQ11
    ## Shimmer          1.00       0.99         0.99         0.98          0.95
    ## Shimmer.dB       0.99       1.00         0.97         0.98          0.95
    ## Shimmer.APQ3     0.99       0.97         1.00         0.96          0.91
    ## Shimmer.APQ5     0.98       0.98         0.96         1.00          0.96
    ## Shimmer.APQ11    0.95       0.95         0.91         0.96          1.00
    ## 
    ## n= 210 
    ## 
    ## 
    ## P
    ##               Shimmer Shimmer.dB Shimmer.APQ3 Shimmer.APQ5 Shimmer.APQ11
    ## Shimmer                0          0            0            0           
    ## Shimmer.dB     0                  0            0            0           
    ## Shimmer.APQ3   0       0                       0            0           
    ## Shimmer.APQ5   0       0          0                         0           
    ## Shimmer.APQ11  0       0          0            0

## Initial Model selection

``` r
# Make full model
fullModel = lm(motor_UPDRS ~ (age + sex + test_time + Jitter.Percent + Jitter.RAP + Jitter.PPQ5 + Jitter.DDP + Shimmer + Shimmer.dB + Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 + Shimmer.DDA + NHR + HNR + RPDE + DFA + PPE), pdData)

print(summary(fullModel))
```

    ## 
    ## Call:
    ## lm(formula = motor_UPDRS ~ (age + sex + test_time + Jitter.Percent + 
    ##     Jitter.RAP + Jitter.PPQ5 + Jitter.DDP + Shimmer + Shimmer.dB + 
    ##     Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 + Shimmer.DDA + 
    ##     NHR + HNR + RPDE + DFA + PPE), data = pdData)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -18.4312  -5.4949  -0.7211   4.9898  19.9632 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     5.489e+01  1.175e+01   4.671 5.64e-06 ***
    ## age             1.469e-01  6.069e-02   2.421  0.01640 *  
    ## sex             5.520e-01  1.197e+00   0.461  0.64515    
    ## test_time       2.663e-02  9.339e-03   2.851  0.00483 ** 
    ## Jitter.Percent  5.071e+02  9.153e+02   0.554  0.58021    
    ## Jitter.RAP      2.633e+05  2.010e+05   1.310  0.19186    
    ## Jitter.PPQ5    -9.986e+02  8.067e+02  -1.238  0.21728    
    ## Jitter.DDP     -8.743e+04  6.707e+04  -1.304  0.19397    
    ## Shimmer         1.573e+02  3.254e+02   0.484  0.62929    
    ## Shimmer.dB     -2.117e+01  2.108e+01  -1.004  0.31641    
    ## Shimmer.APQ3   -2.588e+05  1.971e+05  -1.313  0.19076    
    ## Shimmer.APQ5    1.827e+02  2.823e+02   0.647  0.51842    
    ## Shimmer.APQ11   1.888e+02  1.450e+02   1.302  0.19459    
    ## Shimmer.DDA     8.615e+04  6.570e+04   1.311  0.19138    
    ## NHR            -9.051e+01  3.918e+01  -2.310  0.02193 *  
    ## HNR            -6.923e-01  2.573e-01  -2.690  0.00777 ** 
    ## RPDE           -3.038e+00  7.502e+00  -0.405  0.68592    
    ## DFA            -5.128e+01  1.001e+01  -5.121 7.39e-07 ***
    ## PPE             1.472e+01  1.323e+01   1.113  0.26703    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.37 on 191 degrees of freedom
    ## Multiple R-squared:  0.2898, Adjusted R-squared:  0.2229 
    ## F-statistic:  4.33 on 18 and 191 DF,  p-value: 9.804e-08

``` r
# Check VIFs for each variable
print(as.matrix(vif(fullModel)))
```

    ##                        [,1]
    ## age            1.186618e+00
    ## sex            1.230498e+00
    ## test_time      1.066717e+00
    ## Jitter.Percent 7.195902e+01
    ## Jitter.RAP     1.055560e+06
    ## Jitter.PPQ5    2.159459e+01
    ## Jitter.DDP     1.058026e+06
    ## Shimmer        2.639983e+02
    ## Shimmer.dB     8.659427e+01
    ## Shimmer.APQ3   2.804869e+07
    ## Shimmer.APQ5   8.098877e+01
    ## Shimmer.APQ11  3.054742e+01
    ## Shimmer.DDA    2.804885e+07
    ## NHR            1.046123e+01
    ## HNR            4.816991e+00
    ## RPDE           2.151343e+00
    ## DFA            2.100683e+00
    ## PPE            5.768013e+00

``` r
# Drop models based on largest VIF
modelA2 = update(fullModel, ".~.-Shimmer.DDA")
print(as.matrix(vif(modelA2)))
```

    ##                        [,1]
    ## age            1.183046e+00
    ## sex            1.226860e+00
    ## test_time      1.064590e+00
    ## Jitter.Percent 7.195860e+01
    ## Jitter.RAP     1.053305e+06
    ## Jitter.PPQ5    2.136291e+01
    ## Jitter.DDP     1.055810e+06
    ## Shimmer        2.639951e+02
    ## Shimmer.dB     8.614736e+01
    ## Shimmer.APQ3   1.099713e+02
    ## Shimmer.APQ5   8.098447e+01
    ## Shimmer.APQ11  3.006479e+01
    ## NHR            1.044030e+01
    ## HNR            4.790525e+00
    ## RPDE           2.148402e+00
    ## DFA            2.098486e+00
    ## PPE            5.767068e+00

``` r
modelA3 = update(modelA2, ".~.-Jitter.DDP")
print(as.matrix(vif(modelA3)))
```

    ##                      [,1]
    ## age              1.157119
    ## sex              1.221633
    ## test_time        1.063768
    ## Jitter.Percent  69.907827
    ## Jitter.RAP      44.002106
    ## Jitter.PPQ5     21.294895
    ## Shimmer        262.939559
    ## Shimmer.dB      85.890998
    ## Shimmer.APQ3   109.537588
    ## Shimmer.APQ5    80.940801
    ## Shimmer.APQ11   29.956966
    ## NHR             10.433213
    ## HNR              4.771407
    ## RPDE             2.134011
    ## DFA              2.089353
    ## PPE              5.764471

``` r
modelA4 = update(modelA3, ".~.-Shimmer")
print(as.matrix(vif(modelA4)))
```

    ##                     [,1]
    ## age             1.157052
    ## sex             1.219699
    ## test_time       1.055228
    ## Jitter.Percent 69.564536
    ## Jitter.RAP     43.578222
    ## Jitter.PPQ5    21.195473
    ## Shimmer.dB     57.530069
    ## Shimmer.APQ3   49.361848
    ## Shimmer.APQ5   80.326262
    ## Shimmer.APQ11  25.926295
    ## NHR             9.771188
    ## HNR             4.732537
    ## RPDE            2.103948
    ## DFA             2.038894
    ## PPE             5.612516

``` r
modelA5 = update(modelA4, ".~.-Shimmer.APQ5")
print(as.matrix(vif(modelA5)))
```

    ##                     [,1]
    ## age             1.156185
    ## sex             1.215183
    ## test_time       1.054580
    ## Jitter.Percent 69.559943
    ## Jitter.RAP     41.323145
    ## Jitter.PPQ5    16.244822
    ## Shimmer.dB     56.742883
    ## Shimmer.APQ3   27.910709
    ## Shimmer.APQ11  17.125377
    ## NHR             9.481991
    ## HNR             4.712253
    ## RPDE            2.098926
    ## DFA             2.026957
    ## PPE             5.278768

``` r
modelA6 = update(modelA5, ".~.-Jitter.Percent")
print(as.matrix(vif(modelA6)))
```

    ##                    [,1]
    ## age            1.150094
    ## sex            1.196849
    ## test_time      1.053767
    ## Jitter.RAP     7.631105
    ## Jitter.PPQ5   11.815189
    ## Shimmer.dB    54.554371
    ## Shimmer.APQ3  25.403786
    ## Shimmer.APQ11 17.024788
    ## NHR            9.476408
    ## HNR            4.453485
    ## RPDE           2.073657
    ## DFA            2.026662
    ## PPE            4.483808

``` r
modelA7 = update(modelA6, ".~.-Shimmer.dB")
print(as.matrix(vif(modelA7)))
```

    ##                    [,1]
    ## age            1.142265
    ## sex            1.196561
    ## test_time      1.053394
    ## Jitter.RAP     7.413215
    ## Jitter.PPQ5   10.974127
    ## Shimmer.APQ3   8.038379
    ## Shimmer.APQ11  9.561075
    ## NHR            7.845284
    ## HNR            4.453480
    ## RPDE           2.035634
    ## DFA            2.009335
    ## PPE            4.475277

``` r
modelB = update(modelA7, ".~.-Jitter.PPQ5")
print(as.matrix(vif(modelB)))
```

    ##                   [,1]
    ## age           1.135884
    ## sex           1.196559
    ## test_time     1.048580
    ## Jitter.RAP    5.079520
    ## Shimmer.APQ3  7.589666
    ## Shimmer.APQ11 8.940885
    ## NHR           4.817029
    ## HNR           4.451606
    ## RPDE          1.956309
    ## DFA           1.892853
    ## PPE           4.472000

``` r
# Use AIC and eliminate variables based on highest AIC until removing a variable is detrimental
modelC = step(modelB)
```

    ## Start:  AIC=848.33
    ## motor_UPDRS ~ age + sex + test_time + Jitter.RAP + Shimmer.APQ3 + 
    ##     Shimmer.APQ11 + NHR + HNR + RPDE + DFA + PPE
    ## 
    ##                 Df Sum of Sq   RSS    AIC
    ## - RPDE           1      0.03 10641 846.33
    ## - sex            1      8.77 10650 846.50
    ## - PPE            1     66.61 10708 847.64
    ## <none>                       10641 848.33
    ## - age            1    234.30 10875 850.90
    ## - Shimmer.APQ3   1    278.23 10919 851.75
    ## - Jitter.RAP     1    332.97 10974 852.80
    ## - Shimmer.APQ11  1    358.26 10999 853.28
    ## - HNR            1    372.51 11014 853.55
    ## - test_time      1    449.43 11090 855.01
    ## - NHR            1    861.62 11503 862.68
    ## - DFA            1   1582.09 12223 875.44
    ## 
    ## Step:  AIC=846.33
    ## motor_UPDRS ~ age + sex + test_time + Jitter.RAP + Shimmer.APQ3 + 
    ##     Shimmer.APQ11 + NHR + HNR + DFA + PPE
    ## 
    ##                 Df Sum of Sq   RSS    AIC
    ## - sex            1      9.27 10650 844.51
    ## - PPE            1     68.51 10710 845.68
    ## <none>                       10641 846.33
    ## - age            1    237.01 10878 848.95
    ## - Shimmer.APQ3   1    278.81 10920 849.76
    ## - Jitter.RAP     1    337.69 10979 850.89
    ## - Shimmer.APQ11  1    358.24 10999 851.28
    ## - HNR            1    412.34 11053 852.31
    ## - test_time      1    452.27 11093 853.07
    ## - NHR            1    867.86 11509 860.79
    ## - DFA            1   1583.79 12225 873.46
    ## 
    ## Step:  AIC=844.51
    ## motor_UPDRS ~ age + test_time + Jitter.RAP + Shimmer.APQ3 + Shimmer.APQ11 + 
    ##     NHR + HNR + DFA + PPE
    ## 
    ##                 Df Sum of Sq   RSS    AIC
    ## - PPE            1     60.84 10711 843.71
    ## <none>                       10650 844.51
    ## - age            1    236.72 10887 847.13
    ## - Shimmer.APQ3   1    277.60 10928 847.91
    ## - Jitter.RAP     1    346.02 10996 849.22
    ## - Shimmer.APQ11  1    353.29 11004 849.36
    ## - HNR            1    410.08 11060 850.44
    ## - test_time      1    462.78 11113 851.44
    ## - NHR            1    864.37 11515 858.90
    ## - DFA            1   1586.21 12236 871.67
    ## 
    ## Step:  AIC=843.71
    ## motor_UPDRS ~ age + test_time + Jitter.RAP + Shimmer.APQ3 + Shimmer.APQ11 + 
    ##     NHR + HNR + DFA
    ## 
    ##                 Df Sum of Sq   RSS    AIC
    ## <none>                       10711 843.71
    ## - age            1    298.18 11009 847.47
    ## - Shimmer.APQ3   1    376.24 11087 848.96
    ## - Shimmer.APQ11  1    439.86 11151 850.16
    ## - test_time      1    472.55 11184 850.77
    ## - HNR            1    574.57 11286 852.68
    ## - Jitter.RAP     1    678.85 11390 854.61
    ## - NHR            1    861.48 11573 857.95
    ## - DFA            1   1531.40 12243 869.77

``` r
AIC(modelC)
```

    ## [1] 1441.66

``` r
# Perform model diagnostic
analyzeModel(modelC)
```

    ## 
    ## Call:
    ## lm(formula = motor_UPDRS ~ age + test_time + Jitter.RAP + Shimmer.APQ3 + 
    ##     Shimmer.APQ11 + NHR + HNR + DFA, data = pdData)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -18.1868  -5.4174  -0.8156   5.3083  19.0382 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    5.532e+01  1.014e+01   5.457 1.42e-07 ***
    ## age            1.354e-01  5.724e-02   2.365 0.018959 *  
    ## test_time      2.717e-02  9.124e-03   2.978 0.003260 ** 
    ## Jitter.RAP     1.337e+03  3.745e+02   3.569 0.000448 ***
    ## Shimmer.APQ3  -2.599e+02  9.781e+01  -2.657 0.008514 ** 
    ## Shimmer.APQ11  2.177e+02  7.577e+01   2.873 0.004503 ** 
    ## NHR           -1.037e+02  2.580e+01  -4.021 8.21e-05 ***
    ## HNR           -7.228e-01  2.201e-01  -3.284 0.001209 ** 
    ## DFA           -4.877e+01  9.098e+00  -5.361 2.26e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.3 on 201 degrees of freedom
    ## Multiple R-squared:  0.2668, Adjusted R-squared:  0.2377 
    ## F-statistic: 9.144 on 8 and 201 DF,  p-value: 1.051e-10

![](project_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Model model transformation

``` r
# Find optimal transformation lambda
boxCox(fullModel)
```

![](project_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# make individual plots after transformation
X = as.matrix(pdData[, c(2, 3, 4, 6:ncol(pdData))])
Y = sqrt(as.matrix(pdData[, 5]))
n = nrow(X)
p = ncol(X)

# Plot measurement vs Y graph for each measurement
# par(mfrow=c(5, 4))
for (i in 1:p){
  x = X[, i]
  model = lm(Y~x)
  
  print(summary(model))
  
  plot(x, Y, main = paste(colnames(X)[i], " vs sqrt(motor_UPDRS)"), ylab = 'sqrt(motor_UPDRS)', xlab = colnames(X)[i] )
}
```

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.27032 -0.71915  0.03445  0.77961  1.82841 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 2.888422   0.448238   6.444 7.96e-10 ***
    ## x           0.025362   0.006891   3.681 0.000297 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9116 on 208 degrees of freedom
    ## Multiple R-squared:  0.06114,    Adjusted R-squared:  0.05663 
    ## F-statistic: 13.55 on 1 and 208 DF,  p-value: 0.0002965

![](project_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.26658 -0.66022  0.01341  0.73263  1.75257 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.53321    0.07950  57.021   <2e-16 ***
    ## x           -0.03406    0.13770  -0.247    0.805    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9407 on 208 degrees of freedom
    ## Multiple R-squared:  0.000294,   Adjusted R-squared:  -0.004512 
    ## F-statistic: 0.06118 on 1 and 208 DF,  p-value: 0.8049

![](project_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.48276 -0.71007 -0.04922  0.74060  1.71236 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 4.222235   0.120036  35.175  < 2e-16 ***
    ## x           0.003329   0.001131   2.943  0.00362 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9218 on 208 degrees of freedom
    ## Multiple R-squared:  0.03998,    Adjusted R-squared:  0.03537 
    ## F-statistic: 8.663 on 1 and 208 DF,  p-value: 0.003616

![](project_files/figure-gfm/unnamed-chunk-6-4.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.19386 -0.62650 -0.01092  0.67649  1.87233 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   4.3143     0.1055  40.906   <2e-16 ***
    ## x            33.6001    13.5751   2.475   0.0141 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9272 on 208 degrees of freedom
    ## Multiple R-squared:  0.02861,    Adjusted R-squared:  0.02394 
    ## F-statistic: 6.126 on 1 and 208 DF,  p-value: 0.01412

![](project_files/figure-gfm/unnamed-chunk-6-5.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.21412 -0.60802 -0.01147  0.75536  1.82446 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 4.402e+00  9.734e-02  45.224   <2e-16 ***
    ## x           2.692e+03  1.638e+03   1.644    0.102    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9348 on 208 degrees of freedom
    ## Multiple R-squared:  0.01282,    Adjusted R-squared:  0.008076 
    ## F-statistic: 2.702 on 1 and 208 DF,  p-value: 0.1018

![](project_files/figure-gfm/unnamed-chunk-6-6.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.20557 -0.67961  0.03861  0.72275  1.85597 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.34503    0.09868  44.031   <2e-16 ***
    ## x           58.06650   24.64613   2.356   0.0194 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9285 on 208 degrees of freedom
    ## Multiple R-squared:  0.02599,    Adjusted R-squared:  0.02131 
    ## F-statistic: 5.551 on 1 and 208 DF,  p-value: 0.0194

![](project_files/figure-gfm/unnamed-chunk-6-7.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.21461 -0.61329  0.02111  0.75440  1.83566 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.37118    0.09585  45.606   <2e-16 ***
    ## x           46.43649   21.92447   2.118   0.0354 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9308 on 208 degrees of freedom
    ## Multiple R-squared:  0.02111,    Adjusted R-squared:  0.01641 
    ## F-statistic: 4.486 on 1 and 208 DF,  p-value: 0.03536

![](project_files/figure-gfm/unnamed-chunk-6-8.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.20558 -0.67978  0.03851  0.72263  1.85595 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.34506    0.09868  44.033   <2e-16 ***
    ## x           19.35174    8.21484   2.356   0.0194 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9285 on 208 degrees of freedom
    ## Multiple R-squared:  0.02599,    Adjusted R-squared:  0.0213 
    ## F-statistic: 5.549 on 1 and 208 DF,  p-value: 0.01942

![](project_files/figure-gfm/unnamed-chunk-6-9.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.17837 -0.63346  0.02521  0.72716  1.77281 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   4.3372     0.1062  40.823   <2e-16 ***
    ## x             5.5131     2.5278   2.181   0.0303 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9302 on 208 degrees of freedom
    ## Multiple R-squared:  0.02236,    Adjusted R-squared:  0.01766 
    ## F-statistic: 4.757 on 1 and 208 DF,  p-value: 0.0303

![](project_files/figure-gfm/unnamed-chunk-6-10.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.16444 -0.61575  0.02192  0.72347  1.78619 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   4.3204     0.1083  39.889   <2e-16 ***
    ## x             0.6589     0.2855   2.308    0.022 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.929 on 208 degrees of freedom
    ## Multiple R-squared:  0.02497,    Adjusted R-squared:  0.02028 
    ## F-statistic: 5.326 on 1 and 208 DF,  p-value: 0.02199

![](project_files/figure-gfm/unnamed-chunk-6-11.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.17839 -0.62096  0.04821  0.74738  1.75578 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   4.3635     0.1028  42.450   <2e-16 ***
    ## x             9.2976     4.7070   1.975   0.0496 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9321 on 208 degrees of freedom
    ## Multiple R-squared:  0.01841,    Adjusted R-squared:  0.01369 
    ## F-statistic: 3.902 on 1 and 208 DF,  p-value: 0.04956

![](project_files/figure-gfm/unnamed-chunk-6-12.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.18093 -0.61609  0.02939  0.72872  1.75297 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    4.351      0.101  43.084   <2e-16 ***
    ## x              8.699      3.959   2.197   0.0291 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9301 on 208 degrees of freedom
    ## Multiple R-squared:  0.02268,    Adjusted R-squared:  0.01798 
    ## F-statistic: 4.827 on 1 and 208 DF,  p-value: 0.02911

![](project_files/figure-gfm/unnamed-chunk-6-13.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.14172 -0.65845 -0.05594  0.69754  1.78519 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   4.2724     0.1084  39.411   <2e-16 ***
    ## x             9.3453     3.2859   2.844   0.0049 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.923 on 208 degrees of freedom
    ## Multiple R-squared:  0.03743,    Adjusted R-squared:  0.0328 
    ## F-statistic: 8.089 on 1 and 208 DF,  p-value: 0.004899

![](project_files/figure-gfm/unnamed-chunk-6-14.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.17838 -0.62093  0.04822  0.74734  1.75581 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   4.3635     0.1028  42.451   <2e-16 ***
    ## x             3.0994     1.5690   1.975   0.0495 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9321 on 208 degrees of freedom
    ## Multiple R-squared:  0.01842,    Adjusted R-squared:  0.0137 
    ## F-statistic: 3.902 on 1 and 208 DF,  p-value: 0.04954

![](project_files/figure-gfm/unnamed-chunk-6-15.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.20035 -0.63337  0.01964  0.78400  1.82633 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.42151    0.08002  55.257   <2e-16 ***
    ## x            3.21810    1.52990   2.103   0.0366 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.931 on 208 degrees of freedom
    ## Multiple R-squared:  0.02083,    Adjusted R-squared:  0.01612 
    ## F-statistic: 4.425 on 1 and 208 DF,  p-value: 0.03663

![](project_files/figure-gfm/unnamed-chunk-6-16.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.24905 -0.64496 -0.02761  0.73101  1.74167 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.72254    0.32348  17.691  < 2e-16 ***
    ## x           -0.05477    0.01448  -3.784 0.000202 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.91 on 208 degrees of freedom
    ## Multiple R-squared:  0.0644, Adjusted R-squared:  0.0599 
    ## F-statistic: 14.32 on 1 and 208 DF,  p-value: 0.000202

![](project_files/figure-gfm/unnamed-chunk-6-17.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9742 -0.6630 -0.0138  0.7630  1.8916 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   3.6050     0.3533  10.204  < 2e-16 ***
    ## x             1.6946     0.6422   2.639  0.00895 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9254 on 208 degrees of freedom
    ## Multiple R-squared:  0.03239,    Adjusted R-squared:  0.02774 
    ## F-statistic: 6.963 on 1 and 208 DF,  p-value: 0.00895

![](project_files/figure-gfm/unnamed-chunk-6-18.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.25585 -0.72775 -0.07613  0.68953  1.79842 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   5.8804     0.5660  10.390   <2e-16 ***
    ## x            -2.1012     0.8698  -2.416   0.0166 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9279 on 208 degrees of freedom
    ## Multiple R-squared:  0.02729,    Adjusted R-squared:  0.02262 
    ## F-statistic: 5.836 on 1 and 208 DF,  p-value: 0.01656

![](project_files/figure-gfm/unnamed-chunk-6-19.png)<!-- -->

    ## 
    ## Call:
    ## lm(formula = Y ~ x)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.17442 -0.68063 -0.05026  0.74750  1.89658 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   4.0382     0.1645  24.545  < 2e-16 ***
    ## x             2.1868     0.6864   3.186  0.00166 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9187 on 208 degrees of freedom
    ## Multiple R-squared:  0.04653,    Adjusted R-squared:  0.04194 
    ## F-statistic: 10.15 on 1 and 208 DF,  p-value: 0.001665

![](project_files/figure-gfm/unnamed-chunk-6-20.png)<!-- -->

``` r
# Make new full model
modelD = lm(sqrt(motor_UPDRS) ~ (age + sex + test_time + Jitter.Percent + Jitter.RAP + Jitter.PPQ5 + Jitter.DDP + Shimmer + Shimmer.dB + Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 + Shimmer.DDA + NHR + HNR + RPDE + DFA + PPE), pdData)

print(summary(modelD))
```

    ## 
    ## Call:
    ## lm(formula = sqrt(motor_UPDRS) ~ (age + sex + test_time + Jitter.Percent + 
    ##     Jitter.RAP + Jitter.PPQ5 + Jitter.DDP + Shimmer + Shimmer.dB + 
    ##     Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 + Shimmer.DDA + 
    ##     NHR + HNR + RPDE + DFA + PPE), data = pdData)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.44775 -0.57408 -0.02808  0.55755  2.14659 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     8.452e+00  1.310e+00   6.454 8.78e-10 ***
    ## age             1.578e-02  6.764e-03   2.333  0.02067 *  
    ## sex             5.749e-02  1.334e-01   0.431  0.66694    
    ## test_time       2.801e-03  1.041e-03   2.691  0.00776 ** 
    ## Jitter.Percent  7.431e+01  1.020e+02   0.728  0.46726    
    ## Jitter.RAP      2.911e+04  2.240e+04   1.300  0.19532    
    ## Jitter.PPQ5    -1.044e+02  8.991e+01  -1.161  0.24695    
    ## Jitter.DDP     -9.678e+03  7.476e+03  -1.295  0.19703    
    ## Shimmer         1.126e+01  3.627e+01   0.311  0.75650    
    ## Shimmer.dB     -2.185e+00  2.349e+00  -0.930  0.35353    
    ## Shimmer.APQ3   -3.100e+04  2.197e+04  -1.411  0.15989    
    ## Shimmer.APQ5    1.409e+01  3.147e+01   0.448  0.65490    
    ## Shimmer.APQ11   2.468e+01  1.616e+01   1.527  0.12840    
    ## Shimmer.DDA     1.032e+04  7.323e+03   1.410  0.16031    
    ## NHR            -9.901e+00  4.366e+00  -2.268  0.02447 *  
    ## HNR            -8.011e-02  2.868e-02  -2.794  0.00574 ** 
    ## RPDE           -1.547e-01  8.361e-01  -0.185  0.85342    
    ## DFA            -5.954e+00  1.116e+00  -5.335 2.69e-07 ***
    ## PPE             1.414e+00  1.474e+00   0.960  0.33851    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8214 on 191 degrees of freedom
    ## Multiple R-squared:    0.3,  Adjusted R-squared:  0.234 
    ## F-statistic: 4.547 on 18 and 191 DF,  p-value: 3.236e-08

``` r
# Check VIFs for each variable
print(as.matrix(vif(modelD)))
```

    ##                        [,1]
    ## age            1.186618e+00
    ## sex            1.230498e+00
    ## test_time      1.066717e+00
    ## Jitter.Percent 7.195902e+01
    ## Jitter.RAP     1.055560e+06
    ## Jitter.PPQ5    2.159459e+01
    ## Jitter.DDP     1.058026e+06
    ## Shimmer        2.639983e+02
    ## Shimmer.dB     8.659427e+01
    ## Shimmer.APQ3   2.804869e+07
    ## Shimmer.APQ5   8.098877e+01
    ## Shimmer.APQ11  3.054742e+01
    ## Shimmer.DDA    2.804885e+07
    ## NHR            1.046123e+01
    ## HNR            4.816991e+00
    ## RPDE           2.151343e+00
    ## DFA            2.100683e+00
    ## PPE            5.768013e+00

``` r
# We know VIFs don't change, so remove all variables as we did last time
modelE1 = update(modelD, ".~.-Shimmer.DDA-Jitter.DDP-Shimmer-Shimmer.APQ5-Jitter.Percent-Shimmer.dB-Jitter.PPQ5")
print(as.matrix(vif(modelE1)))
```

    ##                   [,1]
    ## age           1.135884
    ## sex           1.196559
    ## test_time     1.048580
    ## Jitter.RAP    5.079520
    ## Shimmer.APQ3  7.589666
    ## Shimmer.APQ11 8.940885
    ## NHR           4.817029
    ## HNR           4.451606
    ## RPDE          1.956309
    ## DFA           1.892853
    ## PPE           4.472000

``` r
# Use AIC and eliminate variables based on highest AIC until removing a variable is detrimental
modelF = step(modelE1)
```

    ## Start:  AIC=-73.28
    ## sqrt(motor_UPDRS) ~ age + sex + test_time + Jitter.RAP + Shimmer.APQ3 + 
    ##     Shimmer.APQ11 + NHR + HNR + RPDE + DFA + PPE
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - RPDE           1    0.0211 132.16 -75.251
    ## - sex            1    0.0827 132.22 -75.153
    ## - PPE            1    0.8085 132.95 -74.003
    ## <none>                       132.14 -73.284
    ## - age            1    2.6449 134.78 -71.122
    ## - Shimmer.APQ3   1    3.8862 136.03 -69.197
    ## - Jitter.RAP     1    4.4138 136.55 -68.384
    ## - Shimmer.APQ11  1    4.7511 136.89 -67.866
    ## - HNR            1    5.0139 137.15 -67.463
    ## - test_time      1    5.0246 137.16 -67.447
    ## - NHR            1   10.8009 142.94 -58.784
    ## - DFA            1   21.5664 153.71 -43.536
    ## 
    ## Step:  AIC=-75.25
    ## sqrt(motor_UPDRS) ~ age + sex + test_time + Jitter.RAP + Shimmer.APQ3 + 
    ##     Shimmer.APQ11 + NHR + HNR + DFA + PPE
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - sex            1    0.0704 132.23 -77.139
    ## - PPE            1    0.8900 133.05 -75.841
    ## <none>                       132.16 -75.251
    ## - age            1    2.7397 134.90 -72.942
    ## - Shimmer.APQ3   1    3.9313 136.09 -71.095
    ## - Jitter.RAP     1    4.3998 136.56 -70.373
    ## - Shimmer.APQ11  1    4.7590 136.92 -69.822
    ## - test_time      1    5.0037 137.16 -69.447
    ## - HNR            1    5.8150 137.97 -68.208
    ## - NHR            1   10.7931 142.95 -60.765
    ## - DFA            1   21.5454 153.71 -45.535
    ## 
    ## Step:  AIC=-77.14
    ## sqrt(motor_UPDRS) ~ age + test_time + Jitter.RAP + Shimmer.APQ3 + 
    ##     Shimmer.APQ11 + NHR + HNR + DFA + PPE
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - PPE            1    0.8233 133.05 -77.835
    ## <none>                       132.23 -77.139
    ## - age            1    2.7370 134.97 -74.837
    ## - Shimmer.APQ3   1    3.9190 136.15 -73.005
    ## - Jitter.RAP     1    4.4866 136.72 -72.132
    ## - Shimmer.APQ11  1    4.7123 136.94 -71.785
    ## - test_time      1    5.1037 137.33 -71.186
    ## - HNR            1    5.7922 138.02 -70.136
    ## - NHR            1   10.8408 143.07 -62.592
    ## - DFA            1   21.5705 153.80 -47.405
    ## 
    ## Step:  AIC=-77.84
    ## sqrt(motor_UPDRS) ~ age + test_time + Jitter.RAP + Shimmer.APQ3 + 
    ##     Shimmer.APQ11 + NHR + HNR + DFA
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## <none>                       133.05 -77.835
    ## - age            1    3.4992 136.55 -74.384
    ## - test_time      1    5.2227 138.28 -71.750
    ## - Shimmer.APQ3   1    5.2872 138.34 -71.652
    ## - Shimmer.APQ11  1    5.8740 138.93 -70.763
    ## - HNR            1    8.0782 141.13 -67.457
    ## - Jitter.RAP     1    8.8686 141.92 -66.285
    ## - NHR            1   10.8031 143.86 -63.442
    ## - DFA            1   20.8304 153.88 -49.291

``` r
AIC(modelF)
```

    ## [1] 520.1188

``` r
# Perform model diagnostic
analyzeModel(modelF)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(motor_UPDRS) ~ age + test_time + Jitter.RAP + 
    ##     Shimmer.APQ3 + Shimmer.APQ11 + NHR + HNR + DFA, data = pdData)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.46774 -0.59057 -0.05118  0.59419  2.06205 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     8.626732   1.129868   7.635 8.94e-13 ***
    ## age             0.014669   0.006380   2.299 0.022524 *  
    ## test_time       0.002856   0.001017   2.809 0.005462 ** 
    ## Jitter.RAP    152.790410  41.742834   3.660 0.000322 ***
    ## Shimmer.APQ3  -30.810248  10.901741  -2.826 0.005186 ** 
    ## Shimmer.APQ11  25.156615   8.445004   2.979 0.003249 ** 
    ## NHR           -11.616663   2.875556  -4.040 7.61e-05 ***
    ## HNR            -0.085707   0.024534  -3.493 0.000587 ***
    ## DFA            -5.688217   1.014009  -5.610 6.65e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8136 on 201 degrees of freedom
    ## Multiple R-squared:  0.2773, Adjusted R-squared:  0.2485 
    ## F-statistic:  9.64 on 8 and 201 DF,  p-value: 2.771e-11

![](project_files/figure-gfm/unnamed-chunk-6-21.png)<!-- -->

### Remove outliers and recalculate model

``` r
pdData2 = pdData[-c(10, 92, 206), ]

modelG = lm(sqrt(motor_UPDRS) ~ (age + sex + test_time + Jitter.Percent + Jitter.RAP + Jitter.PPQ5 + Jitter.DDP + Shimmer + Shimmer.dB + Shimmer.APQ3 + Shimmer.APQ5 + Shimmer.APQ11 + Shimmer.DDA + NHR + HNR + RPDE + DFA + PPE), pdData2)

# modelG = update(modelG, ".~.-Shimmer.DDA-Jitter.DDP-Shimmer-Shimmer.APQ5-Jitter.Percent-Shimmer.dB.-Jitter.PPQ5")

modelG = update(modelG, ".~.-Shimmer.DDA")
print(as.matrix(vif(modelG)))
```

    ##                        [,1]
    ## age            1.182547e+00
    ## sex            1.215516e+00
    ## test_time      1.065384e+00
    ## Jitter.Percent 7.210831e+01
    ## Jitter.RAP     8.538360e+05
    ## Jitter.PPQ5    2.025944e+01
    ## Jitter.DDP     8.560914e+05
    ## Shimmer        3.361384e+02
    ## Shimmer.dB     8.378395e+01
    ## Shimmer.APQ3   1.385048e+02
    ## Shimmer.APQ5   8.016065e+01
    ## Shimmer.APQ11  3.291220e+01
    ## NHR            9.287245e+00
    ## HNR            4.658054e+00
    ## RPDE           2.084800e+00
    ## DFA            2.063892e+00
    ## PPE            5.420450e+00

``` r
modelG = update(modelG, ".~.-Jitter.DDP")
print(as.matrix(vif(modelG)))
```

    ##                      [,1]
    ## age              1.159024
    ## sex              1.209850
    ## test_time        1.064766
    ## Jitter.Percent  69.755516
    ## Jitter.RAP      42.074956
    ## Jitter.PPQ5     20.159664
    ## Shimmer        333.474118
    ## Shimmer.dB      83.376157
    ## Shimmer.APQ3   137.449224
    ## Shimmer.APQ5    80.118459
    ## Shimmer.APQ11   32.876226
    ## NHR              9.281081
    ## HNR              4.648409
    ## RPDE             2.073959
    ## DFA              2.054688
    ## PPE              5.412117

``` r
modelG = update(modelG, ".~.-Shimmer")
print(as.matrix(vif(modelG)))
```

    ##                     [,1]
    ## age             1.155941
    ## sex             1.208163
    ## test_time       1.058558
    ## Jitter.Percent 68.598648
    ## Jitter.RAP     41.437863
    ## Jitter.PPQ5    20.143828
    ## Shimmer.dB     60.673466
    ## Shimmer.APQ3   49.098701
    ## Shimmer.APQ5   78.632951
    ## Shimmer.APQ11  25.541399
    ## NHR             8.848349
    ## HNR             4.580548
    ## RPDE            2.043038
    ## DFA             2.032963
    ## PPE             5.219976

``` r
modelG = update(modelG, ".~.-Shimmer.APQ5")
print(as.matrix(vif(modelG)))
```

    ##                     [,1]
    ## age             1.155491
    ## sex             1.202946
    ## test_time       1.057514
    ## Jitter.Percent 68.544934
    ## Jitter.RAP     40.094743
    ## Jitter.PPQ5    15.588811
    ## Shimmer.dB     59.376838
    ## Shimmer.APQ3   30.276598
    ## Shimmer.APQ11  17.425432
    ## NHR             8.577129
    ## HNR             4.557280
    ## RPDE            2.036961
    ## DFA             2.019388
    ## PPE             4.897378

``` r
modelG = update(modelG, ".~.-Jitter.Percent")
print(as.matrix(vif(modelG)))
```

    ##                    [,1]
    ## age            1.152163
    ## sex            1.183073
    ## test_time      1.057501
    ## Jitter.RAP     6.888142
    ## Jitter.PPQ5   10.461644
    ## Shimmer.dB    56.137746
    ## Shimmer.APQ3  26.463700
    ## Shimmer.APQ11 17.394960
    ## NHR            8.571885
    ## HNR            4.379070
    ## RPDE           2.020746
    ## DFA            2.019345
    ## PPE            4.217668

``` r
modelG = update(modelG, ".~.-Shimmer.dB")
print(as.matrix(vif(modelG)))
```

    ##                   [,1]
    ## age           1.149389
    ## sex           1.182850
    ## test_time     1.057180
    ## Jitter.RAP    6.782552
    ## Jitter.PPQ5   9.825858
    ## Shimmer.APQ3  7.766673
    ## Shimmer.APQ11 9.330205
    ## NHR           7.415831
    ## HNR           4.379061
    ## RPDE          1.981315
    ## DFA           2.012771
    ## PPE           4.202385

``` r
# This isn't greater than 10, but it is very close to 10
modelG = update(modelG, ".~.-Jitter.PPQ5")
print(as.matrix(vif(modelG)))
```

    ##                   [,1]
    ## age           1.142023
    ## sex           1.182847
    ## test_time     1.052784
    ## Jitter.RAP    4.792205
    ## Shimmer.APQ3  7.342401
    ## Shimmer.APQ11 8.719148
    ## NHR           4.608099
    ## HNR           4.379021
    ## RPDE          1.912712
    ## DFA           1.886607
    ## PPE           4.199135

``` r
modelG = step(modelG)
```

    ## Start:  AIC=-77.55
    ## sqrt(motor_UPDRS) ~ age + sex + test_time + Jitter.RAP + Shimmer.APQ3 + 
    ##     Shimmer.APQ11 + NHR + HNR + RPDE + DFA + PPE
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - RPDE           1    0.0001 126.74 -79.551
    ## - sex            1    0.0596 126.80 -79.454
    ## - PPE            1    0.3921 127.13 -78.912
    ## <none>                       126.74 -77.551
    ## - age            1    2.4517 129.19 -75.585
    ## - Shimmer.APQ3   1    4.6807 131.42 -72.044
    ## - Shimmer.APQ11  1    5.0276 131.77 -71.499
    ## - HNR            1    5.5642 132.30 -70.657
    ## - test_time      1    6.2165 132.96 -69.639
    ## - Jitter.RAP     1    6.5955 133.34 -69.050
    ## - NHR            1   11.5159 138.25 -61.549
    ## - DFA            1   23.5313 150.27 -44.298
    ## 
    ## Step:  AIC=-79.55
    ## sqrt(motor_UPDRS) ~ age + sex + test_time + Jitter.RAP + Shimmer.APQ3 + 
    ##     Shimmer.APQ11 + NHR + HNR + DFA + PPE
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - sex            1    0.0627 126.80 -81.449
    ## - PPE            1    0.4023 127.14 -80.895
    ## <none>                       126.74 -79.551
    ## - age            1    2.4837 129.22 -77.534
    ## - Shimmer.APQ3   1    4.6976 131.44 -74.017
    ## - Shimmer.APQ11  1    5.0274 131.77 -73.499
    ## - HNR            1    6.2237 132.96 -71.628
    ## - test_time      1    6.2473 132.99 -71.591
    ## - Jitter.RAP     1    6.6694 133.41 -70.935
    ## - NHR            1   11.6051 138.34 -63.415
    ## - DFA            1   23.5497 150.29 -46.273
    ## 
    ## Step:  AIC=-81.45
    ## sqrt(motor_UPDRS) ~ age + test_time + Jitter.RAP + Shimmer.APQ3 + 
    ##     Shimmer.APQ11 + NHR + HNR + DFA + PPE
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## - PPE            1    0.3531 127.16 -82.873
    ## <none>                       126.80 -81.449
    ## - age            1    2.4779 129.28 -79.443
    ## - Shimmer.APQ3   1    4.6827 131.49 -75.942
    ## - Shimmer.APQ11  1    4.9873 131.79 -75.463
    ## - HNR            1    6.1936 133.00 -73.577
    ## - test_time      1    6.3606 133.16 -73.317
    ## - Jitter.RAP     1    6.7997 133.60 -72.636
    ## - NHR            1   11.6405 138.44 -65.268
    ## - DFA            1   23.5879 150.39 -48.134
    ## 
    ## Step:  AIC=-82.87
    ## sqrt(motor_UPDRS) ~ age + test_time + Jitter.RAP + Shimmer.APQ3 + 
    ##     Shimmer.APQ11 + NHR + HNR + DFA
    ## 
    ##                 Df Sum of Sq    RSS     AIC
    ## <none>                       127.16 -82.873
    ## - age            1    2.9882 130.14 -80.065
    ## - Shimmer.APQ11  1    5.8109 132.97 -75.623
    ## - Shimmer.APQ3   1    5.8747 133.03 -75.524
    ## - test_time      1    6.5092 133.66 -74.539
    ## - HNR            1    8.0053 135.16 -72.235
    ## - Jitter.RAP     1   11.3014 138.46 -67.247
    ## - NHR            1   11.5922 138.75 -66.813
    ## - DFA            1   23.5992 150.75 -49.633

``` r
AIC(modelG)
```

    ## [1] 506.5674

``` r
analyzeModel(modelG)
```

    ## 
    ## Call:
    ## lm(formula = sqrt(motor_UPDRS) ~ age + test_time + Jitter.RAP + 
    ##     Shimmer.APQ3 + Shimmer.APQ11 + NHR + HNR + DFA, data = pdData2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4951 -0.5651 -0.0126  0.6033  2.1170 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     8.915155   1.122185   7.944 1.44e-13 ***
    ## age             0.013659   0.006332   2.157 0.032202 *  
    ## test_time       0.003212   0.001009   3.184 0.001689 ** 
    ## Jitter.RAP    187.747800  44.755172   4.195 4.12e-05 ***
    ## Shimmer.APQ3  -32.614368  10.783254  -3.025 0.002820 ** 
    ## Shimmer.APQ11  25.208637   8.380316   3.008 0.002970 ** 
    ## NHR           -12.522923   2.947527  -4.249 3.31e-05 ***
    ## HNR            -0.086056   0.024374  -3.531 0.000516 ***
    ## DFA            -6.141259   1.013080  -6.062 6.68e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8014 on 198 degrees of freedom
    ## Multiple R-squared:  0.3004, Adjusted R-squared:  0.2721 
    ## F-statistic: 10.63 on 8 and 198 DF,  p-value: 2.188e-12

![](project_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Transform X for better fit?

``` r
# For some reason these don't work in R markdown but I was able to run them in console
# boxTidwell((motor_UPDRS) ~ age + test_time + Jitter.RAP + Shimmer.APQ3 + Shimmer.APQ11 + NHR + HNR + DFA, data = pdData)
# boxTidwell(sqrt(motor_UPDRS) ~ age + test_time + Jitter.RAP + Shimmer.APQ3 + Shimmer.APQ11 + NHR + HNR + DFA, data = pdData2)
# boxTidwell(sqrt(motor_UPDRS) ~ age + test_time + Jitter.RAP + Shimmer.APQ3 + Shimmer.APQ11 + NHR + HNR + DFA, data = pdData2)
```
