#######################
# This script reads in behavioral data (language, EFs) given by Dan
# and outputs data frames with individual performance in both long and wide format
#######################

# Set up
library("tidyverse")
library("readxl")

rm(list=ls())
setwd(dir="../data/beh/")

############################


#########################
# variables of interest
my_vars <- c("PREPOST", "SUB", "SEX", "AGEPRE", "KL",
             "WMPRE", "CONFLICTPRE", "INHIBITPRE", "VOCABRAWPRE",
             "WMPOST", "CONFLICTPOST", "INHIBITPOST")

data.raw <- read_excel("newseg_medAD_4.5.23.xlsx") %>%
  select(all_of(my_vars)) %>%
  rename(subj_num = SUB,
         time_point = PREPOST)

# tidy - 
data.tidy <- data.raw %>%
  mutate(WMPOST = parse_number(WMPOST))
  str_extract(WMPOST,"\\[[^]]*\\]")
  mutate(WMPOST = as.numeric(WMPOST), # automatically turn non-numeric into NA
         CONFLICTPOST = as.numeric(CONFLICTPOST),
         INHIBITPOST = as.numeric(INHIBITPOST)) %>%
  mutate(
    WM = case_when(time_point == 1 ~ WMPRE,
                   time_point == 2 ~ WMPOST),
    CONFLICT = case_when(time_point == 1 ~ CONFLICTPRE,
                         time_point == 2 ~ CONFLICTPOST),
    INHIBIT = case_when(time_point == 1 ~ INHIBITPRE,
                         time_point == 2 ~ INHIBITPOST),
    VOCAB = case_when(time_point == 1 ~ VOCABRAWPRE)) %>%
  select(!ends_with(c("PRE", "POST")))

glimpse(data.tidy)

# Save and export ----
## rename
beh_data.raw <- data.tidy
write.csv(beh_data.raw, file="beh_tidied.csv")
save(beh_data.raw, file="./beh_data_tidied.RData")
