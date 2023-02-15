#######################
# This script reads in behavioral data (language, EFs) given by Dan
# and outputs data frames with individual performance in both long and wide format
#######################

# Set up
library("tidyverse")
library("readxl")

rm(list=ls())
setwd(dir="../")

############################


#########################
# variables of interest
my_vars <- c("PREPOST", "SUB", "SEX", "AGE", "KL",
             "WM", "CONFLICT", "INHIBIT", "VOCABRAW")

data.raw <- read_excel("newseg_medAD_11.16.22.xlsx") %>%
  select(all_of(my_vars)) %>%
  rename(subj_num = SUB,
         time_point = PREPOST)


# Save and export ----
## rename
beh_data.raw <- data.raw

save(beh_data.raw, file="./beh/tidied/beh_data_tidied.RData")
