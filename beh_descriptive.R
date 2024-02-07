# Notes -----
# outputs:  
# - descriptive (range) behavioral measures (EFs) 
# - number of trials for ERP data
# - KL composition
# - group differences on EFs, vocab, and age

# Behavior data include gender, KL, 3 EFs (WM, inhibition, conflict), vocab, age
# only subjects with ERP data and KL should be included in further analyses 
# (KL info is included in the ERP tibble)

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
load("./data/ERP/tidied/erp_tidied.RData")
load("./data/beh/beh_data_tidied.RData")

# define categorical KL and exclude subjects without KL data 
beh_data <- beh_data.raw %>%
  drop_na(KL) %>%
  mutate(KL.cat =
           case_when(
             KL %in% c(0:4) ~ "SS",
             KL %in% c(5:8) ~ "CP",
             TRUE ~ as.character(NA)))

data_erp_all <- data_erp_all %>%
  # drop_na(KL.cont) %>%
  mutate(KL.cat =
           case_when(
             KL.cont %in% c(0:4) ~ "SS",
             KL.cont %in% c(5:8) ~ "CP",
             TRUE ~ as.character(NA))) %>%
  drop_na(KL.cat)

# get list of subjects that have both KL and ERP data
subj_list.erp <- sort(unique(data_erp_all$subj_num))
subj_list.beh <- sort(unique(beh_data$subj_num))

identical(subj_list.erp, subj_list.beh)

# list of sessions
session_list.erp <- data_erp_all %>%
  distinct(subj_num, time_point) %>%
  arrange(subj_num, time_point)
# number of sessions: 75 subjects contributed 1 session and 53 subjects contributed 2 sessions
data_erp_all %>% 
  distinct(subj_num, time_point) %>% 
  group_by(subj_num) %>% 
  summarise(nT=n()) %>% 
  group_by(nT) %>% 
  summarise(n=n())

# number of SS and CP knowers
data_erp_all %>% 
  group_by(subj_num, time_point, KL.cat) %>% 
  summarise(nT=n()) %>% 
  group_by(KL.cat) %>% 
  summarise(n=n())

data_erp_all %>% 
  group_by(subj_num, time_point, KL.cont) %>% 
  summarise(nT=n()) %>% 
  group_by(KL.cont) %>% 
  summarise(n=n())
# some descriptive beh stats

range(beh_data$WM, na.rm = T)
mean(beh_data$WM, na.rm = T)
sd(beh_data$WM, na.rm = T)

range(beh_data$CONFLICT, na.rm = T)
mean(beh_data$CONFLICT, na.rm = T)
sd(beh_data$CONFLICT, na.rm = T)


range(beh_data$INHIBIT, na.rm = T)
mean(beh_data$INHIBIT, na.rm = T)
sd(beh_data$INHIBIT, na.rm = T)

# range(beh_data$VOCAB, na.rm = T)
range(beh_data$VOCAB.SS, na.rm = T)
mean(beh_data$VOCAB.SS, na.rm = T)
sd(beh_data$VOCAB.SS, na.rm = T)


# drop out rate (only time point 1)? no

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

###
beh_data.CP <- beh_data %>% subset(KL.cat == "CP")
beh_data.SS <- beh_data %>% subset(KL.cat == "SS")
#### Age and EF and vocab difference between groups? -----

t.test(beh_data.CP$age.days, beh_data.SS$age.days)
t.test(beh_data.CP$WM, beh_data.SS$WM)
t.test(beh_data.CP$CONFLICT, beh_data.SS$CONFLICT)
t.test(beh_data.CP$INHIBIT, beh_data.SS$INHIBIT)
# t.test(beh_data.CP$VOCAB, beh_data.SS$VOCAB)
t.test(beh_data.CP$VOCAB.SS, beh_data.SS$VOCAB.SS)


beh_data %>%
  group_by(KL.cat) %>%
  summarise(mean_age = mean(age.days)/30/12,
            sd_age = sd(age.days)/30/12)

beh_data %>%
  distinct(subj_num, time_point, age.days) %>%
  # group_by(KL.cat) %>%
  summarise(mean_age = mean(age.days)/30/12,
            sd_age = sd(age.days)/30/12) %>% 
  as.data.frame()