# SETUP
library("lme4")
library("car")


rm(list = ls())
load("../data/ERPbeh/tidied/nmap_data_tidied.RData") 
load("../data/beh/beh_data_tidied.RData")

# wide format acc already calculated in a tibble, now output
write.csv(nmap_acc.wide, file = "acc_subj_6cond_wide.csv", row.names = FALSE)
#

acc.by.subj.cond <- nmap_data.long%>%
  group_by(Subject, Session, cue, probe) %>%
  summarise(individual_acc = mean(correct_or_not, na.rm=T))

write.csv(acc.by.subj.cond, file = "acc_subj_6cond.csv")


acc.by.subj <- nmap_data.long%>%
  group_by(Subject, Session) %>%
  summarise(individual_acc = mean(correct_or_not, na.rm=T))

write.csv(acc.by.subj, file = "acc_subj_overall.csv")


# combine ERP acc with subj info

ERPbeh_info <- acc.by.subj %>%
  rename(subj_num = Subject,
         time_point = Session) %>%
  right_join(beh_data.raw, by = c("subj_num", "time_point")) %>%
  mutate(KL.cat = 
           case_when(
             KL %in% c(1:4) ~ "SS",
             KL %in% c(5:8) ~ "CP",
             TRUE ~ as.character(NA)))
  

ERPbeh_info %>%
  group_by(KL.cat) %>%
  summarise(mean.acc.KL = mean(individual_acc, na.rm=T),
            sd.acc.KL = sd(individual_acc, na.rm=T))

ERPbeh_info.SS <- ERPbeh_info %>%
  filter(KL.cat == "SS") %>%
  ungroup() 

t.test(ERPbeh_info.SS$individual_acc, mu=.5)

ERPbeh_info.CP <- ERPbeh_info %>%
  filter(KL.cat == "CP") %>%
  ungroup() 

t.test(ERPbeh_info.CP$individual_acc, mu=.5)


ERPbeh_info %>%
  t.test(individual_acc ~ KL.cat, data = .)
  
