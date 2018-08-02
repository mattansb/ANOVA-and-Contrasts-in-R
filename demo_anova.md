ANOVA
================

Import data
-----------

ChickWeight data describes a mixed design: a group of 45 chickens were split into 4 groups, each group was given a different diet. Each chick was weighed at 6 time points (4 week intervals).
The sav file has the following variables:

-   Chick - the chich ID
-   Diet - 1-4 the Diet number, with 1 being the standard diet.
-   T0:T20 - weigts at each of the 6 time points.

``` r
library(haven)

# import wide format
df_wide <- read_spss('data/ChickWeight.sav')

head(df_wide)
```

    ## # A tibble: 6 x 8
    ##   Chick     Diet         T0    T4    T8   T12   T16   T20
    ##   <dbl+lbl> <dbl+lbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 4         1            41    53    65    71    71    91
    ## 2 5         1            42    59    85    90    93   100
    ## 3 6         1            41    54    65    77    98   115
    ## 4 7         1            41    52    74    89   101   120
    ## 5 9         1            42    61    83    98   113   133
    ## 6 10        1            43    55    65    82   106   144

We need the data in the long format:

``` r
library(tidyverse)

df_long <- df_wide %>% 
  gather("Time","weight",T0:T20)


head(df_long)
```

    ## # A tibble: 6 x 4
    ##   Chick     Diet      Time  weight
    ##   <dbl+lbl> <dbl+lbl> <chr>  <dbl>
    ## 1 4         1         T0        41
    ## 2 5         1         T0        42
    ## 3 6         1         T0        41
    ## 4 7         1         T0        41
    ## 5 9         1         T0        42
    ## 6 10        1         T0        43

repeated measure ANOVA
----------------------

We will fit a mixed-anova model with `afex`:

``` r
library(afex)

# fit using `aov_ez`
fit <- aov_ez(id = "Chick", dv = "weight", data = df_long,
              between = c("Diet"), within = "Time")
fit
```

    ## Anova Table (Type 3 tests)
    ## 
    ## Response: weight
    ##      Effect          df     MSE          F ges p.value
    ## 1      Diet       3, 41 3147.91    5.08 ** .15    .004
    ## 2      Time 1.25, 51.43 2587.62 304.91 *** .79  <.0001
    ## 3 Diet:Time 3.76, 51.43 2587.62    3.84 ** .12    .010
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
    ## 
    ## Sphericity correction method: GG

We can change some of the defults, such as *η*<sub>2</sub><sup>2</sup> for effect-sizes, and not correcting for GG:

``` r
fit <- aov_ez(id = "Chick", dv = "weight", data = df_long,
              between = "Diet", within = "Time",
              anova_table = list(es = "pes", correction = "none"))
fit
```

    ## Anova Table (Type 3 tests)
    ## 
    ## Response: weight
    ##      Effect      df     MSE          F pes p.value
    ## 1      Diet   3, 41 3147.91    5.08 ** .27    .004
    ## 2      Time  5, 205  649.18 304.91 *** .88  <.0001
    ## 3 Diet:Time 15, 205  649.18   3.84 *** .22  <.0001
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

Contrasts
---------

Before running contrasts in `emmeans`, we need to set some of `afex` options, to ensure that MSE and DF for between-subject contrasts are computed/estimated using the multivariate method (producing the same results as SPSS ans Statistica):

``` r
afex_options(emmeans_model = "multivariate")
```

`emmeans` gives you much control over how contrasts are calculated.
But... with great power comes great responsibility... (This is probably true for most anything in R)

### simple effects

``` r
library(emmeans)  

joint_tests(fit, by = 'Diet')
```

    ## Diet = 1:
    ##  model term df1 df2 F.ratio p.value
    ##  Time         5  41  79.635  <.0001
    ## 
    ## Diet = 2:
    ##  model term df1 df2 F.ratio p.value
    ##  Time         5  41  82.338  <.0001
    ## 
    ## Diet = 3:
    ##  model term df1 df2 F.ratio p.value
    ##  Time         5  41 108.788  <.0001
    ## 
    ## Diet = 4:
    ##  model term df1 df2 F.ratio p.value
    ##  Time         5  41 115.770  <.0001

``` r
joint_tests(fit, by = 'Time')
```

    ## Time = T0:
    ##  model term df1 df2 F.ratio p.value
    ##  Diet         3  41   1.697  0.1826
    ## 
    ## Time = T4:
    ##  model term df1 df2 F.ratio p.value
    ##  Diet         3  41  13.538  <.0001
    ## 
    ## Time = T8:
    ##  model term df1 df2 F.ratio p.value
    ##  Diet         3  41   7.788  0.0003
    ## 
    ## Time = T12:
    ##  model term df1 df2 F.ratio p.value
    ##  Diet         3  41   4.291  0.0101
    ## 
    ## Time = T16:
    ##  model term df1 df2 F.ratio p.value
    ##  Diet         3  41   3.355  0.0279
    ## 
    ## Time = T20:
    ##  model term df1 df2 F.ratio p.value
    ##  Diet         3  41   4.918  0.0052

### Simple plot (at a glance)

``` r
emmip(fit, ~Time, CIs = TRUE)
```

![](doc/unnamed-chunk-7-1.png)

### compute contrasts with `contrast`

We first look create a refernce grid (which we can print to look at the predicted means):

``` r
rg <- emmeans(fit, ~Time)
rg
```

    ##  Time    emmean        SE df  lower.CL  upper.CL
    ##  T0    40.98785 0.1696356 41  40.64526  41.33043
    ##  T4    60.78299 0.4801868 41  59.81323  61.75274
    ##  T8    94.38785 1.9695809 41  90.41020  98.36550
    ##  T12  136.06215 4.4362445 41 127.10298 145.02133
    ##  T16  173.50590 6.7214856 41 159.93159 187.08022
    ##  T20  217.90972 8.9612023 41 199.81221 236.00724
    ## 
    ## Results are averaged over the levels of: Diet 
    ## Confidence level used: 0.95

Using some methods that come baked into `emmeans`:

``` r
contrast(rg, "poly")
```

    ##  contrast    estimate        SE df t.ratio p.value
    ##  linear    1264.45243 66.268599 41  19.081  <.0001
    ##  quadratic  138.39896 27.963203 41   4.949  <.0001
    ##  cubic      -71.14826 23.225667 41  -3.063  0.0039
    ##  quartic     16.93090  3.673819 41   4.609  <.0001
    ##  degree 5    30.05035  8.259246 41   3.638  0.0008
    ## 
    ## Results are averaged over the levels of: Diet

``` r
contrast(rg, "consec") # using built in methods, emmeans knows when contrasts are dependant
```

    ##  contrast  estimate        SE df t.ratio p.value
    ##  T4 - T0   19.79514 0.4792445 41  41.305  <.0001
    ##  T8 - T4   33.60486 1.7100385 41  19.652  <.0001
    ##  T12 - T8  41.67431 2.7711664 41  15.039  <.0001
    ##  T16 - T12 37.44375 3.0619722 41  12.229  <.0001
    ##  T20 - T16 44.40382 3.6438128 41  12.186  <.0001
    ## 
    ## Results are averaged over the levels of: Diet 
    ## P value adjustment: mvt method for 5 tests

``` r
contrast(rg, "consec", adjust = "fdr") # or adjust = "tukey"
```

    ##  contrast  estimate        SE df t.ratio p.value
    ##  T4 - T0   19.79514 0.4792445 41  41.305  <.0001
    ##  T8 - T4   33.60486 1.7100385 41  19.652  <.0001
    ##  T12 - T8  41.67431 2.7711664 41  15.039  <.0001
    ##  T16 - T12 37.44375 3.0619722 41  12.229  <.0001
    ##  T20 - T16 44.40382 3.6438128 41  12.186  <.0001
    ## 
    ## Results are averaged over the levels of: Diet 
    ## P value adjustment: fdr method for 5 tests

``` r
contrast(rg, "consec", adjust = "none")
```

    ##  contrast  estimate        SE df t.ratio p.value
    ##  T4 - T0   19.79514 0.4792445 41  41.305  <.0001
    ##  T8 - T4   33.60486 1.7100385 41  19.652  <.0001
    ##  T12 - T8  41.67431 2.7711664 41  15.039  <.0001
    ##  T16 - T12 37.44375 3.0619722 41  12.229  <.0001
    ##  T20 - T16 44.40382 3.6438128 41  12.186  <.0001
    ## 
    ## Results are averaged over the levels of: Diet

Some custom methods:

``` r
my_weights <- list(half_time = c(-1,-1,-1,1,1,1),
                   start_end = c(-1,0,0,0,0,1))
contrast(rg, my_weights)
```

    ##  contrast  estimate        SE df t.ratio p.value
    ##  half_time 331.3191 17.827329 41  18.585  <.0001
    ##  start_end 176.9219  8.998433 41  19.661  <.0001
    ## 
    ## Results are averaged over the levels of: Diet

``` r
my_weights <- list(half_time = c(-1,-1,-1,1,1,1)/3, # divide to get un-inflated estimate
                   start_end = c(-1,-0,-0,0,0,1))
contrast(rg, my_weights)
```

    ##  contrast  estimate       SE df t.ratio p.value
    ##  half_time 110.4397 5.942443 41  18.585  <.0001
    ##  start_end 176.9219 8.998433 41  19.661  <.0001
    ## 
    ## Results are averaged over the levels of: Diet

Interactions
------------

We can do the same for interactions:

``` r
emmip(fit, Diet~Time, CIs = T)
```

![](doc/unnamed-chunk-11-1.png)

``` r
rg <- emmeans(fit, ~Diet*Time)
rg
```

    ##  Diet Time    emmean         SE df  lower.CL  upper.CL
    ##  1    T0    41.56250  0.2775283 41  41.00202  42.12298
    ##  2    T0    40.70000  0.3510486 41  39.99104  41.40896
    ##  3    T0    40.80000  0.3510486 41  40.09104  41.50896
    ##  4    T0    40.88889  0.3700377 41  40.14158  41.63620
    ##  1    T4    56.68750  0.7855980 41  55.10095  58.27405
    ##  2    T4    59.80000  0.9937116 41  57.79316  61.80684
    ##  3    T4    62.20000  0.9937116 41  60.19316  64.20684
    ##  4    T4    64.44444  1.0474640 41  62.32905  66.55984
    ##  1    T8    81.56250  3.2222848 41  75.05496  88.07004
    ##  2    T8    91.70000  4.0759037 41  83.46855  99.93145
    ##  3    T8    98.40000  4.0759037 41  90.16855 106.63145
    ##  4    T8   105.88889  4.2963798 41  97.21217 114.56560
    ##  1    T12  114.43750  7.2578098 41  99.78006 129.09494
    ##  2    T12  131.30000  9.1804839 41 112.75964 149.84036
    ##  3    T12  144.40000  9.1804839 41 125.85964 162.94036
    ##  4    T12  154.11111  9.6770797 41 134.56785 173.65437
    ##  1    T16  145.81250 10.9965229 41 123.60457 168.02043
    ##  2    T16  164.70000 13.9096235 41 136.60895 192.79105
    ##  3    T16  197.40000 13.9096235 41 169.30895 225.49105
    ##  4    T16  186.11111 14.6620305 41 156.50054 215.72168
    ##  1    T20  173.25000 14.6607567 41 143.64200 202.85800
    ##  2    T20  205.60000 18.5445534 41 168.14851 243.05149
    ##  3    T20  258.90000 18.5445534 41 221.44851 296.35149
    ##  4    T20  233.88889 19.5476756 41 194.41156 273.36622
    ## 
    ## Confidence level used: 0.95

``` r
contrast(rg, "consec", by = 'Diet')
```

    ## Diet = 1:
    ##  contrast  estimate        SE df t.ratio p.value
    ##  T4 - T0   15.12500 0.7840563 41  19.291  <.0001
    ##  T8 - T4   24.87500 2.7976668 41   8.891  <.0001
    ##  T12 - T8  32.87500 4.5336993 41   7.251  <.0001
    ##  T16 - T12 31.37500 5.0094651 41   6.263  <.0001
    ##  T20 - T16 27.43750 5.9613712 41   4.603  0.0007
    ## 
    ## Diet = 2:
    ##  contrast  estimate        SE df t.ratio p.value
    ##  T4 - T0   19.10000 0.9917615 41  19.259  <.0001
    ##  T8 - T4   31.90000 3.5387997 41   9.014  <.0001
    ##  T12 - T8  39.60000 5.7347264 41   6.905  <.0001
    ##  T16 - T12 33.40000 6.3365279 41   5.271  0.0001
    ##  T20 - T16 40.90000 7.5406044 41   5.424  <.0001
    ## 
    ## Diet = 3:
    ##  contrast  estimate        SE df t.ratio p.value
    ##  T4 - T0   21.40000 0.9917615 41  21.578  <.0001
    ##  T8 - T4   36.20000 3.5387997 41  10.229  <.0001
    ##  T12 - T8  46.00000 5.7347264 41   8.021  <.0001
    ##  T16 - T12 53.00000 6.3365279 41   8.364  <.0001
    ##  T20 - T16 61.50000 7.5406044 41   8.156  <.0001
    ## 
    ## Diet = 4:
    ##  contrast  estimate        SE df t.ratio p.value
    ##  T4 - T0   23.55556 1.0454084 41  22.532  <.0001
    ##  T8 - T4   41.44444 3.7302224 41  11.110  <.0001
    ##  T12 - T8  48.22222 6.0449324 41   7.977  <.0001
    ##  T16 - T12 32.00000 6.6792868 41   4.791  0.0004
    ##  T20 - T16 47.77778 7.9484949 41   6.011  <.0001
    ## 
    ## P value adjustment: mvt method for 5 tests

``` r
contrast(rg, "pairwise", by = 'Time')
```

    ## Time = T0:
    ##  contrast     estimate         SE df t.ratio p.value
    ##  1 - 2      0.86250000  0.4475009 41   1.927  0.2327
    ##  1 - 3      0.76250000  0.4475009 41   1.704  0.3348
    ##  1 - 4      0.67361111  0.4625471 41   1.456  0.4726
    ##  2 - 3     -0.10000000  0.4964576 41  -0.201  0.9971
    ##  2 - 4     -0.18888889  0.5100617 41  -0.370  0.9824
    ##  3 - 4     -0.08888889  0.5100617 41  -0.174  0.9981
    ## 
    ## Time = T4:
    ##  contrast     estimate         SE df t.ratio p.value
    ##  1 - 2     -3.11250000  1.2667387 41  -2.457  0.0822
    ##  1 - 3     -5.51250000  1.2667387 41  -4.352  0.0005
    ##  1 - 4     -7.75694444  1.3093300 41  -5.924  <.0001
    ##  2 - 3     -2.40000000  1.4053204 41  -1.708  0.3328
    ##  2 - 4     -4.64444444  1.4438295 41  -3.217  0.0130
    ##  3 - 4     -2.24444444  1.4438295 41  -1.555  0.4154
    ## 
    ## Time = T8:
    ##  contrast     estimate         SE df t.ratio p.value
    ##  1 - 2    -10.13750000  5.1957782 41  -1.951  0.2233
    ##  1 - 3    -16.83750000  5.1957782 41  -3.241  0.0122
    ##  1 - 4    -24.32638889  5.3704747 41  -4.530  0.0003
    ##  2 - 3     -6.70000000  5.7641983 41  -1.162  0.6536
    ##  2 - 4    -14.18888889  5.9221508 41  -2.396  0.0938
    ##  3 - 4     -7.48888889  5.9221508 41  -1.265  0.5902
    ## 
    ## Time = T12:
    ##  contrast     estimate         SE df t.ratio p.value
    ##  1 - 2    -16.86250000 11.7028666 41  -1.441  0.4818
    ##  1 - 3    -29.96250000 11.7028666 41  -2.560  0.0654
    ##  1 - 4    -39.67361111 12.0963496 41  -3.280  0.0110
    ##  2 - 3    -13.10000000 12.9831648 41  -1.009  0.7451
    ##  2 - 4    -22.81111111 13.3389338 41  -1.710  0.3316
    ##  3 - 4     -9.71111111 13.3389338 41  -0.728  0.8853
    ## 
    ## Time = T16:
    ##  contrast     estimate         SE df t.ratio p.value
    ##  1 - 2    -18.88750000 17.7313604 41  -1.065  0.7123
    ##  1 - 3    -51.58750000 17.7313604 41  -2.909  0.0286
    ##  1 - 4    -40.29861111 18.3275382 41  -2.199  0.1406
    ##  2 - 3    -32.70000000 19.6711782 41  -1.662  0.3563
    ##  2 - 4    -21.41111111 20.2102144 41  -1.059  0.7157
    ##  3 - 4     11.28888889 20.2102144 41   0.559  0.9437
    ## 
    ## Time = T20:
    ##  contrast     estimate         SE df t.ratio p.value
    ##  1 - 2    -32.35000000 23.6397599 41  -1.368  0.5259
    ##  1 - 3    -85.65000000 23.6397599 41  -3.623  0.0043
    ##  1 - 4    -60.63888889 24.4345946 41  -2.482  0.0779
    ##  2 - 3    -53.30000000 26.2259589 41  -2.032  0.1930
    ##  2 - 4    -28.28888889 26.9446114 41  -1.050  0.7214
    ##  3 - 4     25.01111111 26.9446114 41   0.928  0.7899
    ## 
    ## P value adjustment: tukey method for comparing a family of 4 estimates

``` r
contrast(rg, "pairwise", by = 'Time', adjust = 'none')
```

    ## Time = T0:
    ##  contrast     estimate         SE df t.ratio p.value
    ##  1 - 2      0.86250000  0.4475009 41   1.927  0.0609
    ##  1 - 3      0.76250000  0.4475009 41   1.704  0.0960
    ##  1 - 4      0.67361111  0.4625471 41   1.456  0.1529
    ##  2 - 3     -0.10000000  0.4964576 41  -0.201  0.8414
    ##  2 - 4     -0.18888889  0.5100617 41  -0.370  0.7130
    ##  3 - 4     -0.08888889  0.5100617 41  -0.174  0.8625
    ## 
    ## Time = T4:
    ##  contrast     estimate         SE df t.ratio p.value
    ##  1 - 2     -3.11250000  1.2667387 41  -2.457  0.0183
    ##  1 - 3     -5.51250000  1.2667387 41  -4.352  0.0001
    ##  1 - 4     -7.75694444  1.3093300 41  -5.924  <.0001
    ##  2 - 3     -2.40000000  1.4053204 41  -1.708  0.0952
    ##  2 - 4     -4.64444444  1.4438295 41  -3.217  0.0025
    ##  3 - 4     -2.24444444  1.4438295 41  -1.555  0.1277
    ## 
    ## Time = T8:
    ##  contrast     estimate         SE df t.ratio p.value
    ##  1 - 2    -10.13750000  5.1957782 41  -1.951  0.0579
    ##  1 - 3    -16.83750000  5.1957782 41  -3.241  0.0024
    ##  1 - 4    -24.32638889  5.3704747 41  -4.530  0.0001
    ##  2 - 3     -6.70000000  5.7641983 41  -1.162  0.2518
    ##  2 - 4    -14.18888889  5.9221508 41  -2.396  0.0212
    ##  3 - 4     -7.48888889  5.9221508 41  -1.265  0.2132
    ## 
    ## Time = T12:
    ##  contrast     estimate         SE df t.ratio p.value
    ##  1 - 2    -16.86250000 11.7028666 41  -1.441  0.1572
    ##  1 - 3    -29.96250000 11.7028666 41  -2.560  0.0142
    ##  1 - 4    -39.67361111 12.0963496 41  -3.280  0.0021
    ##  2 - 3    -13.10000000 12.9831648 41  -1.009  0.3189
    ##  2 - 4    -22.81111111 13.3389338 41  -1.710  0.0948
    ##  3 - 4     -9.71111111 13.3389338 41  -0.728  0.4707
    ## 
    ## Time = T16:
    ##  contrast     estimate         SE df t.ratio p.value
    ##  1 - 2    -18.88750000 17.7313604 41  -1.065  0.2930
    ##  1 - 3    -51.58750000 17.7313604 41  -2.909  0.0058
    ##  1 - 4    -40.29861111 18.3275382 41  -2.199  0.0336
    ##  2 - 3    -32.70000000 19.6711782 41  -1.662  0.1041
    ##  2 - 4    -21.41111111 20.2102144 41  -1.059  0.2956
    ##  3 - 4     11.28888889 20.2102144 41   0.559  0.5795
    ## 
    ## Time = T20:
    ##  contrast     estimate         SE df t.ratio p.value
    ##  1 - 2    -32.35000000 23.6397599 41  -1.368  0.1786
    ##  1 - 3    -85.65000000 23.6397599 41  -3.623  0.0008
    ##  1 - 4    -60.63888889 24.4345946 41  -2.482  0.0173
    ##  2 - 3    -53.30000000 26.2259589 41  -2.032  0.0486
    ##  2 - 4    -28.28888889 26.9446114 41  -1.050  0.2999
    ##  3 - 4     25.01111111 26.9446114 41   0.928  0.3587

``` r
contrast(rg, interaction = c("pairwise","consec"))
```

    ##  Diet_pairwise Time_consec   estimate        SE df t.ratio p.value
    ##  1 - 2         T4 - T0      -3.975000  1.264253 41  -3.144  0.0031
    ##  1 - 3         T4 - T0      -6.275000  1.264253 41  -4.963  <.0001
    ##  1 - 4         T4 - T0      -8.430556  1.306760 41  -6.451  <.0001
    ##  2 - 3         T4 - T0      -2.300000  1.402563 41  -1.640  0.1087
    ##  2 - 4         T4 - T0      -4.455556  1.440996 41  -3.092  0.0036
    ##  3 - 4         T4 - T0      -2.155556  1.440996 41  -1.496  0.1423
    ##  1 - 2         T8 - T4      -7.025000  4.511102 41  -1.557  0.1271
    ##  1 - 3         T8 - T4     -11.325000  4.511102 41  -2.510  0.0161
    ##  1 - 4         T8 - T4     -16.569444  4.662778 41  -3.554  0.0010
    ##  2 - 3         T8 - T4      -4.300000  5.004618 41  -0.859  0.3952
    ##  2 - 4         T8 - T4      -9.544444  5.141757 41  -1.856  0.0706
    ##  3 - 4         T8 - T4      -5.244444  5.141757 41  -1.020  0.3137
    ##  1 - 2         T12 - T8     -6.725000  7.310370 41  -0.920  0.3630
    ##  1 - 3         T12 - T8    -13.125000  7.310370 41  -1.795  0.0800
    ##  1 - 4         T12 - T8    -15.347222  7.556165 41  -2.031  0.0488
    ##  2 - 3         T12 - T8     -6.400000  8.110128 41  -0.789  0.4346
    ##  2 - 4         T12 - T8     -8.622222  8.332364 41  -1.035  0.3068
    ##  3 - 4         T12 - T8     -2.222222  8.332364 41  -0.267  0.7910
    ##  1 - 2         T16 - T12    -2.025000  8.077520 41  -0.251  0.8033
    ##  1 - 3         T16 - T12   -21.625000  8.077520 41  -2.677  0.0106
    ##  1 - 4         T16 - T12    -0.625000  8.349109 41  -0.075  0.9407
    ##  2 - 3         T16 - T12   -19.600000  8.961204 41  -2.187  0.0345
    ##  2 - 4         T16 - T12     1.400000  9.206762 41   0.152  0.8799
    ##  3 - 4         T16 - T12    21.000000  9.206762 41   2.281  0.0278
    ##  1 - 2         T20 - T16   -13.462500  9.612422 41  -1.401  0.1689
    ##  1 - 3         T20 - T16   -34.062500  9.612422 41  -3.544  0.0010
    ##  1 - 4         T20 - T16   -20.340278  9.935619 41  -2.047  0.0471
    ##  2 - 3         T20 - T16   -20.600000 10.664025 41  -1.932  0.0603
    ##  2 - 4         T20 - T16    -6.877778 10.956244 41  -0.628  0.5336
    ##  3 - 4         T20 - T16    13.722222 10.956244 41   1.252  0.2175

``` r
my_weights <- list(half_time_1_vs_all = c(3,-1,-1,-1,3,-1,-1,-1,3,-1,-1,-1,-3,1,1,1,-3,1,1,1,-3,1,1,1))
contrast(rg, my_weights)
```

    ##  contrast           estimate       SE df t.ratio p.value
    ##  half_time_1_vs_all 310.5264 109.0424 41   2.848  0.0069

``` r
# or...

my_Diet_weights.emmc <- function(...){
  data.frame(none_all = c(3,-1,-1,-1)/3)
}

my_Time_weights.emmc <- function(...){
  data.frame(halves = c(-1,-1,-1,1,1,1)/3,
             start_all = c(-5,1,1,1,1,1)/5)
}

contrast(rg, interaction = c('my_Diet_weights','my_Time_weights'))
```

    ##  Diet_my_Diet_weights Time_my_Time_weights  estimate        SE df t.ratio
    ##  none_all             halves               -34.50293 12.115824 41  -2.848
    ##  none_all             start_all            -30.33917  8.647057 41  -3.509
    ##  p.value
    ##   0.0069
    ##   0.0011
