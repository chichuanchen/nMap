# Set up
library("tidyverse")
library("readxl")

rm(list=ls())
setwd(dir="../data/")

age_raw <- read_xlsx("TRNagesprepost.xlsx", 
                     col_types = c("text", "text", "date", "date", "numeric", "date", "numeric")) 


age_df <- age_raw %>%
  select(-NEWSUB) %>%
  rename(subj_num = OLDSUB) %>%
  mutate(t1_agedays = (BIRTHDATE %--% DATEPRE) / days(1),
         t2_agedays = (BIRTHDATE %--% DATEPOST) / days(1)) %>%
  pivot_longer(cols = ends_with("agedays"), names_to = "time_point", values_to = "age.days", values_drop_na = TRUE) %>%
  mutate(time_point = as.numeric(str_extract(time_point, "\\d+"))) %>%
  select(subj_num, time_point, age.days)