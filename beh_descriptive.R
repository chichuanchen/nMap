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
load("./../data/ERP/tidied/erp_tidied.RData")
load("./../data/beh/tidied/beh_data_tidied.RData")

# define categorical KL
beh_data <- beh_data.raw %>%
  drop_na(KL) %>%
  mutate(KL.cat = 
           case_when(
             KL %in% c(0:4) ~ "SS",
             KL %in% c(5:8) ~ "CP",
             TRUE ~ as.character(NA)))

# some descriptive beh stats
range(beh_data$WM, na.rm = T)
range(beh_data$CONFLICT, na.rm = T)
range(beh_data$INHIBIT, na.rm = T)
range(beh_data$VOCAB, na.rm = T)
range(beh_data$VOCAB.SS, na.rm = T)


# drop out rate (only time point 1)?

length(unique(beh_data$subj_num))
table(duplicated(beh_data$subj_num))
table(beh_data$KL.cat) # 53/128 have 2 time points. 128-53=75 dropped out

twotimers <- beh_data %>%
  group_by(subj_num) %>%
  filter(n() > 1)

onetimers <- beh_data %>%
  group_by(subj_num) %>%
  filter(n() == 1)

onlyPRE <- onetimers %>%
  filter(time_point == 1)

onlyPOST <- onetimers %>%
  filter(time_point == 2)


CPchange <- twotimers %>% 
  filter(length(unique(KL.cat))==2) %>% 
  select(subj_num, time_point, KL.cat) %>%
  pivot_wider(names_from = time_point, values_from = KL.cat)


t.test(beh_data$AGE~beh_data$KL.cat)

beh_data %>%
  group_by(KL.cat) %>%
  summarise(mean_age = mean(AGE),
            sd_age = sd(AGE))