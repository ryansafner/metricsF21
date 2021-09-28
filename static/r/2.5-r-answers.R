library(tidyverse)

# 1 -------------------

# read in data

# this assumes you put the data in the working directory, or if using an R Project,
# in the R project directory
congress <- read_csv("congress-trump-score.csv")

# 2 ------------------

# look at data
congress %>%
  glimpse()

# 3 -----------------

# make agree_pct out of 100

congress <- congress %>% # overwrite congress!
  mutate(agree_pct = agree_pct * 100)

# 4 -----------------

# scatterplot

# save as scatter
scatter <- ggplot(data = congress)+
  aes(x = net_trump_vote, y = agree_pct)+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_light()

# view it
scatter


# 5 -----------------

# correlation

# using base R
cor(congress$net_trump_vote, congress$agree_pct)

# using tidyverse
congress %>%
  summarize(correlation = cor(net_trump_vote, agree_pct))

# 6 ----------------

# regression (Base R)

# save as reg
reg <- lm(agree_pct ~ net_trump_vote, data = congress)

# get summary of reg
reg %>%
  summary()

# Note the rest of this question is asking you to interpret the OLS estimates,
# not have you calculate or run any commands in R

# I have commands below that show you how to extract and save these values, 
# if you wanted them for some future uses

## a ---------------

# I just want to show you how to extract these and put it into text with markdown
# This relies on the code in Question 7 Part B
library(broom)
reg_tidy <- reg %>%
  tidy()

beta_0_hat <- reg_tidy %>% # save this as beta_0_hat
  slice(1) %>% # look only at first row (intercept)
  pull(estimate) %>% # extract the value of the estimate
  round(., 2) # round to 2 decimal places

beta_0_hat # look at it

## b ---------------

beta_1_hat <- reg_tidy %>% # save this as beta_0_hat
  slice(2) %>% # look only at second row (net_trump_score)
  pull(estimate) %>% # extract the value of the estimate
  round(., 2) # round to 2 decimal places

beta_1_hat # look at it

## c --------------

# this uses the glance command in question 7 part C. 

r_sq <- glance(reg) %>%
  pull(r.squared) %>%
  round(., 2)

r_sq # look at it

## d ------------

# this uses the glance command in question 7 part C. 

ser <- glance(reg) %>%
  pull(sigma) %>% 
  round(.,2) # round to 2 decimals

ser # look at it

# 7 ----------------

## a ---------------

# install.packages("broom")
library(broom)

## b --------------

reg_tidy <- reg %>%
  tidy()

reg_tidy

## c --------------

reg %>%
  glance()

## d --------------

reg_aug <- reg %>%
  augment()

reg_aug


# 8 ---------------

# calculate R^2 as ratio of variances of predicted to actual Y values
reg_aug %>%
  summarize(var_y_hat = var(.fitted),
            var_y = var(agree_pct),
            R_sq = var_y_hat/var_y)

# using my custom sum of squares function
sum_sq = function(x){sum((x - mean(x))^2)}

# R^2 = ESS/TSS
reg_aug %>%
  summarize(ESS = sum_sq(.fitted),
            TSS = sum_sq(agree_pct),
            R_sq = ESS/TSS)

# R^2 = 1 - (SSE/TSS)
reg_aug %>%
  summarize(SSE = sum(.resid^2),
            TSS = sum_sq(agree_pct),
            R_sq = 1-(SSE/TSS))

# and of course, find R^2 by just squaring correlation coefficient
congress %>%
  summarize(correlation = cor(net_trump_vote, agree_pct),
            R_sq = correlation^2)


# 9 ---------------

#install.packages("huxtable")
library(huxtable)
# basic
huxreg(reg)

# some customization:
huxreg("Agree with President (Proportion)" = reg,
       coefs = c("Intercept" = "(Intercept)",
                 "Net Trump Vote" = "net_trump_vote"),
       statistics = c("n" = "nobs",
                      "R-squared" = "r.squared",
                      "SER" = "sigma"),
       number_format = 4)

# 10 --------------

## a -------------

# residual histogram 
ggplot(data = reg_aug)+
  aes(x = .resid)+
  geom_histogram(color = "white")+
  theme_classic()

## b -------------

# residual plot

ggplot(data = reg_aug)+
  aes(x = net_trump_vote, y = .resid)+
  geom_point()+
  geom_hline(yintercept = 0, color = "red")+
  theme_classic()

# 11 --------------

## b --------------

# install.packages("lmtest")
library(lmtest)
bptest(reg)

## c --------------

#install.packages("estimatr")
library(estimatr)

reg_rse <- lm_robust(agree_pct ~ net_trump_vote, data = congress, se_type="stata")
reg_rse

## d ------------

huxreg("Original" = reg,
       "Robust SEs" = reg_rse,
       number_format = 6) # need to see more decimal places!

# 12 --------------

##  b ------------

# install.packages("car")
library(car)
outlierTest(reg)

## c --------------

congress %>%
  slice(1708)

# 13 --------------

## a -------------

congress %>%
  count(congress)

## b --------------

congress_tidy <- congress %>%
  filter(congress != 0)

## c --------------

congress_tidy <- congress_tidy %>%
  mutate(pol_party = case_when(
    party %in% c("Democrat", "D") ~ "Democrat",
    party %in% c("Republican", "R") ~ "Republican",
    party %in% c("Independent", "I") ~ "Independent"
  ))

congress_tidy %>%
  count(pol_party)

## d --------------

party_colors <- c("Democrat" = "blue",
                  "Republican" = "red",
                  "Independent" = "gray")

p <- ggplot(data = congress_tidy)+
  aes(x = net_trump_vote, y = agree_pct)+
  geom_point(aes(color = pol_party))+
  geom_smooth(method = "lm", color = "black")+
  scale_colour_manual("Parties", values = party_colors)+
  theme_light()
p

## e ----------

p + facet_wrap(~chamber)

# just for kicks
p+facet_grid(chamber~pol_party)
