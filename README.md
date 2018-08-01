ANOVA and Contrasts in R
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
*Last updated August 01, 2018*

Programme
=========

In a joint Berger-Kessler lab meeting (May 16, 2018), I will introduce and demonstrate how to conduct ANOVAs and compute contrasts (including interaction contrasts) in R using the afex and emmeans packages.

What you'll need to play along
==============================

-   These SPSS `.sav` files:
    -   [ChickWeight.sav](data/ChickWeight.sav)
    -   [anxiety.sav](data/anxiety.sav)
-   [R](https://cran.r-project.org/)
    -   Optional: [RStudio Desktop](https://www.rstudio.com/)
-   The following R packages:
    -   `haven` (for importing SPSS sav files)
    -   `tidyverse` (for data manipulation and ggplotting)
    -   `afex` (version 0.20-0 at least) (for running ANOVA)
    -   `emmeans` (for contrasts and simple slopes)

Install these packages by running in the R command line:

``` r
install.packages(c("haven","afex","emmeans","tidyverse"))
```

Reading Materials
=================

The following vignettes are the basis of the demo:
- [ANOVA with afex](https://github.com/singmann/afex/blob/master/vignettes/afex_anova_example.Rmd)
- [Contrasts](https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html) and [interaction contrasts](l), and [more](https://cran.r-project.org/web/packages/emmeans/vignettes/confidence-intervals.html) with emmeans.

The Demo
========

The demo consists of two parts:

1.  [Running Anovas and analyzing effects and interactions with contrasts (planned and post-hoc)](demo_anova.md).
2.  [Analyzing regression interactions](demo_SimpleSlopes.md).
