library(tidyverse)

# 1 -------------------

library(tidyverse)
options(scipen = 999) # disables scientific notation
diamonds <- diamonds

## A -----------------

ggplot(data = diamonds)+
  aes(x = carat,
      y = price)+
  geom_point()+
  geom_smooth(method = "lm")

# 2 ------------------

reg <- lm(price ~ carat, data = diamonds)

summary(reg)

## B ----------------

library(huxtable)
huxreg(reg)

## D ---------------

library(broom)

reg_tidy <- reg %>% tidy(conf.int = T)

# look at tidy outcome
reg_tidy

# extract & save the confidence interval
CI_endpoints <- reg_tidy %>%
  filter(term == "carat") %>%
  select(conf.low, conf.high)

# look at it
CI_endpoints

# you could have alternatively just found what these are and then saved them manually
CI_alt <- c(7728.855,7783.996)

CI_alt

## E ---------------

# extract and save sample slope as object called beta_1_hat

beta_1_hat <- reg_tidy %>%
  filter(term == "carat") %>%
  select(estimate)

# confirm
beta_1_hat

# you could have alternatively just found what these are and then saved them manually
beta_1_hat_alt <- 7756.43

beta_1_hat_alt

# 3 -----------------

library(infer)

## A ---------------

# save our simulations as an object (I called it "boot")
boot <- diamonds %>% # or whatever you named your dataframe!
  specify(price ~ carat) %>% # our regression model
  generate(reps = 1000, # run 1000 simulations
           type = "bootstrap") %>% # using bootstrap method
  calculate(stat = "slope") # estimate slope in each simulation

# look at it
#boot

## B ---------------

boot %>%
  get_confidence_interval(level = 0.95,
                          type = "se", 
                          point_estimate = beta_1_hat) # or whatever you saved it as

## C ---------------

boot %>%
  visualize()+
  shade_ci(endpoints = CI_endpoints) # or whatever you saved it as

# 4 -----------------

## B ---------------

# save our simulations as an object (I called it "test_sims")
test_sims <- diamonds %>% # or whatever you named your dataframe!
  specify(price ~ carat) %>% # our regression model
  hypothesize(null = "independence") %>% # H_0 is that slope is 0
  generate(reps = 1000, # run 1000 simulations
           type = "permute") %>% # using permute method, centering distr at 0
  calculate(stat = "slope") # estimate slope in each simulation

# look at it
#test_sims

## C -------------

test_sims %>%
  get_p_value(obs_stat = beta_1_hat,
              direction = "both")
# again, instead of beta_1_hat you could type in 7756.426

## D ------------

test_sims %>%
  visualize()

test_sims %>%
  visualize()+
  shade_p_value(obs_stat = beta_1_hat, # or whatever you saved it as
                direction = "both") # for two-sided test

## E -----------

2 * pt(551.3, # our t-statistic
       df = 53938, # the df number
       lower.tail = F) # we'll use the right tail
