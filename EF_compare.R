rm(list=ls())
load("./beh/beh_data_tidied.RData")

CCshare <- read_xlsx("TRN4.20.21.USonlyCCshare.xlsx") # this one
# plosone <- read_xlsx("TRN_DATASET_PLOS_SHARE10.29.21.xlsx") 

rejected_subs <- read_xlsx("ERPrejectedSUBS.xlsx") %>%
  rename(subj_num = SUBNUMB,
         time_point = SESSION) %>%
  select(subj_num, time_point)

my_vars <- c("subjnumber", "sex", "agemonths", 
             "HAVEPOST", 
             "WMpreALL", "ConflictPRE", "InhibitPRE", 
             "WMpostALL", "ConflictPOST", "InhibitPOST")

EF_df <- CCshare %>% 
  select(all_of(my_vars))

EF_df_tidied <- EF_df %>% 
  rename(subj_num = subjnumber,
         WM_pre = WMpreALL,
         conflict_pre = ConflictPRE,
         inhibit_pre = InhibitPRE,
         WM_post = WMpostALL,
         conflict_post = ConflictPOST,
         inhibit_post = InhibitPOST) %>%
  pivot_longer(cols = -c(subj_num, -sex, agemonths, -HAVEPOST),
               names_to = c(".value", "time_point"),
               names_pattern = "(.+)_(.+)") %>%
  drop_na() %>%
  mutate(time_point = as.numeric(factor(time_point, levels = c("pre", "post"))))
  
glimpse(EF_df_tidied)


unmatched_rows <- anti_join(rejected_subs, EF_df_tidied)

EF_rejected <- merge(rejected_subs, EF_df_tidied)
EF_accepted <- beh_data %>% select(subj_num, time_point, WM, CONFLICT, INHIBIT, age.days) %>%
  mutate(agemonths = age.days/30)

t.test(EF_rejected$agemonths, EF_accepted$agemonths)

t.test(EF_rejected$WM, EF_accepted$WM, var.equal = T)
t.test(EF_rejected$conflict, EF_accepted$CONFLICT, var.equal = T)
t.test(EF_rejected$inhibit, EF_accepted$INHIBIT, var.equal = T)
