# Set up -----
library("tidyverse")
library("readxl")
library("lme4")
library("lmerTest")
library("lattice") # used for visual inspection of normal distribution of residuals
library("car") # leveneTest
library("emmeans") # extract estimated marginal means
# library("MuMIn") # for calculating r squared from LMEs

rm(list = ls())
load("./ERP/tidied/erp_tidied.RData")
load("./beh/tidied/beh_data_tidied.RData")

# define categorical KL
beh_data <- beh_data.raw %>%
  drop_na(KL) %>%
  mutate(KL.cat = if_else(KL < 5, "SS", "CP")) 

#
length(unique(beh_data$subj_num))
table(duplicated(beh_data$subj_num))
table(beh_data$KL.cat)
t.test(beh_data$AGE~beh_data$KL.cat)

beh_data %>%
  group_by(KL.cat) %>%
  summarise(mean_age = mean(AGE),
            sd_age = sd(AGE))