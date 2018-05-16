library(tidyverse)


# Import data -------------------------------------------------------------

# ChickWeight data describes a mixed design: a group of 45 chickens were split
# into 4 groups, each group was given a different diet. Each chick was weighed
# at 6 time points (4 week intervals).
# The sav file has the following variables:
#   Chick - the chich ID
#   Diet - 1-4 the Diet number, with 1 being the standard diet.
#   T0:T20 - weigts at each of the 6 time points.

library(haven)

fname <- choose.files()

# import wide format
df_wide <- read_spss(fname)

head(df_wide)

# make wide data long
df_long <- df_wide %>% 
  gather("Time","weight",T0:T20)


head(df_long)







# repeated measure ANOVA --------------------------------------------------



library(afex)

# fit using `aov_ez`

fit <- aov_ez(id = "Chick", dv = "weight", data = df_long,
              between = c("Diet"), within = "Time")
fit




# get partial eta squared and do not correct GG
fit <- aov_ez(id = "Chick", dv = "weight", data = df_long,
              between = "Diet", within = "Time",
              anova_table = list(es = "pes", correction = "none"))
fit

# Contrasts ---------------------------------------------------------------

library(emmeans)  
afex_options(emmeans_model = "multivariate")
# MSE and DF for between-subject contrasts are computed/estimated using
# the multivariate method (producing the same results as SPSS ans Statistica)


# emmeans gives you much control over how contrasts are calculated.
# But... with great power comes great responsibility...
# (This is probably true for most anything in R)


# simple effects
joint_tests(fit, by = 'Diet')
joint_tests(fit, by = 'Time')




# get means with `emmeans`
rg <- emmeans(fit, ~Time)
rg






# make basic plot with `emmip`
emmip(rg, ~Time)
emmip(rg, ~Time, CIs = TRUE)



# compute contrasts with `contrast`
contrast(rg, "poly")
contrast(rg, "consec") # using built in methods, emmeans knows when contrasts are dependant
contrast(rg, "consec", adjust = "fdr") # or adjust = "tukey"
contrast(rg, "consec", adjust = "none")



my_weights <- list(half_time = c(-1,-1,-1,1,1,1),
                   start_end = c(-1,0,0,0,0,1))
contrast(rg, my_weights)


my_weights <- list(half_time = c(-1,-1,-1,1,1,1)/3, # divide to get un-inflated estimate
                   start_end = c(-1,-0,-0,0,0,1))
contrast(rg, my_weights)






# We can do the same for interactions
rg <- emmeans(fit, ~Diet*Time)
rg




emmip(rg, Diet~Time, CIs = T)




contrast(rg, "consec", by = 'Diet')
contrast(rg, "pairwise", by = 'Time')
contrast(rg, "pairwise", by = 'Time', adjust = 'none')
contrast(rg, interaction = c("pairwise","consec"))



my_weights <- list(half_time_1_vs_all = c(3,-1,-1,-1,3,-1,-1,-1,3,-1,-1,-1,-3,1,1,1,-3,1,1,1,-3,1,1,1))
contrast(rg, my_weights)


# Or....
library(marginC)

# to the diet groups differ from baseline in how they grow?
my_weights <- list(half_time_1_vs_all = mw(Diet = c(-3,1,1,1), Time = c(-1,-1,-1,1,1,1)))
my_weights

contrast(rg, my_weights)



# simple effects' contrasts
my_weights <- list(first_vs_2_3 = mw(Diet = c(-2,1,1,0)),
                   first_vs_all = mw(Diet = c(-3,1,1,1)))
my_weights

contrast(rg, my_weights, by = 'Time')
