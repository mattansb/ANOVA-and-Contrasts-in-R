ANOVA and Contrasts in R
================
Mattan S. Ben-Shachar
Last updated 2018-05-05

<!-- README.md is generated from README.Rmd. Please edit that file -->
A joint Berger-Kessler lab meeting (16/5/2018)

Programme
=========

I will introduce and demonstrate how to conduct ANOVAs and compute contrasts in R using the afex and emmeans packages.

What you'll need to play along
==============================

-   These SPSS `.sav` files:
    -   [ChickWeight.sav](.\data\ChickWeight.sav)
    -   [anxiety.sav](.\data\anxiety.sav)
-   [R](https://cran.r-project.org/)
    -   Optional: [RStudio Desktop](https://www.rstudio.com/)
-   The following R packages (`install.packages(c("haven","afex","emmeans","tidyverse"))`):
    -   `haven` (for importing SPSS sav files)
    -   `tidyverse` (for data manipulation and ggplotting)
    -   `afex` (version 0.20-0 at least) (for running ANOVA)
    -   `emmeans` (for contrasts and simple slopes)

Reading Materials
=================

The following vignettes are the basis of the demo: - [ANOVA with afex](https://github.com/singmann/afex/blob/master/vignettes/afex_anova_example.Rmd) - Contrasts with emmeans: - [Contrasts](https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html) - [Interactions](https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html)

The Demo
========

After the lab meeting, the code used will be available in this repository as well.
