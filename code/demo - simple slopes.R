# Import data -------------------------------------------------------------

# The data in anxiety is from a hypothetical correlational design, where anxiety,
# depression and sex can be used to predict some outcome.
# The sav file has the following variables:
#   anxiety - score between 0-60 of anxiety symptoms.
#   depression - score between 0-40 of depression symptoms.
#   sex - 1 = male, 2 = female
#   outcome - score between 0-35 of some outcome

library(tidyverse)
library(haven)

# import data
df_outcome <- read_spss("data/anxiety.sav") %>% 
  mutate(sex = factor(sex,labels = c("M","F")),
         anxiety = scale(anxiety))


# Interaction with binary var ---------------------------------------------
library(lm.beta)

fit_1 <- lm(outcome ~ anxiety+sex, df_outcome)
summary(lm.beta(fit_1))

fit_2 <- lm(outcome ~ anxiety*sex, df_outcome)
summary(fit_2)

# compare F+ test
anova(fit_1,fit_2)

# Simple slopes -----

library(emmeans)

rg <- emtrends(fit_2, ~sex, var = "anxiety")
rg

emmip(rg,~sex, CIs = TRUE)
contrast(rg,"pairwise")
test(rg)












# Interaction with continous var ------------------------------------------

fit_1 <- lm(outcome ~ anxiety+depression, df_outcome)
summary(fit_1)

fit_2 <- lm(outcome ~ anxiety*depression, df_outcome)
summary(fit_2)

anova(fit_1,fit_2)


# Simple slopes ----
rg <- emtrends(fit_2, ~depression, var = "anxiety")
rg


rg <- emtrends(fit_2, ~depression, var = "anxiety", at = list(depression = c(10,30)))
rg
emmip(rg,~depression, CIs = TRUE)
contrast(rg,"pairwise")
test(rg)


