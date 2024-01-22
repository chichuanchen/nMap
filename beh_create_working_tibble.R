#######################
# This script reads in behavioral data (language, EFs) given by Dan
# and outputs data frames with individual performance in both long and wide format
#######################

# Set up
library("tidyverse")
library("readxl")
library("lubridate")

rm(list=ls())
setwd(dir="../data/beh/")

############################


#########################
# variables of interest
my_vars <- c("PREPOST", "SUB", "SEX", "KL",
             "WMPRE", "CONFLICTPRE", "INHIBITPRE", "VOCABRAWPRE",
             "WMPOST", "CONFLICTPOST", "INHIBITPOST")

data.raw <- read_excel("newseg_medAD_4.5.23.xlsx") %>%
  select(all_of(my_vars)) %>%
  rename(subj_num = SUB,
         time_point = PREPOST)


age_raw <- read_xlsx("TRNagesprepost.xlsx", 
                     col_types = c("text", "text", "date", "date", "numeric", "date", "numeric"),
                     na = "N/A",) 

age_df <- age_raw %>%
  select(-NEWSUB) %>%
  rename(subj_num = OLDSUB) %>%
  mutate(t1_agedays = (BIRTHDATE %--% DATEPRE) / days(1),
         t2_agedays = (BIRTHDATE %--% DATEPOST) / days(1),
         subj_num = as.numeric(subj_num)) %>%
  pivot_longer(cols = ends_with("agedays"), names_to = "time_point", values_to = "age.days", values_drop_na = TRUE) %>%
  mutate(time_point = as.numeric(str_extract(time_point, "\\d+"))) %>%
  select(subj_num, time_point, age.days)

vocab.ss <- read_xlsx("TRN4.20.21.USonlyCCshare.xlsx") %>%
  select(subjnumber, vocabPREss) %>%
  rename(subj_num = subjnumber,
         VOCAB.SS = vocabPREss) %>%
  mutate(time_point = 1)

# tidy - 
data.tidy <- data.raw %>%
  
  mutate(WMPOST = parse_number(WMPOST, na = "#NULL!"), # automatically turn non-numeric into NA
         CONFLICTPOST = parse_number(CONFLICTPOST, na = "#NULL!"),
         INHIBITPOST = parse_number(INHIBITPOST, na = "#NULL!")) %>%
  mutate(
    WM = case_when(time_point == 1 ~ WMPRE,
                   time_point == 2 ~ WMPOST),
    CONFLICT = case_when(time_point == 1 ~ CONFLICTPRE,
                         time_point == 2 ~ CONFLICTPOST),
    INHIBIT = case_when(time_point == 1 ~ INHIBITPRE,
                         time_point == 2 ~ INHIBITPOST),
    VOCAB = case_when(time_point == 1 ~ VOCABRAWPRE)) %>%
  select(!ends_with(c("PRE", "POST"))) %>%
  left_join(age_df, by = c("subj_num", "time_point")) %>%
  left_join(vocab.ss, by = c("subj_num", "time_point"))

glimpse(data.tidy)

# Save and export ----
## rename
beh_data.raw <- data.tidy
write.csv(beh_data.raw, file="beh_tidied.csv")
save(beh_data.raw, file="./beh_data_tidied.RData")
