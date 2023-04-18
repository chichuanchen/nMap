# SETUP
library("lme4")
library("car")


rm(list = ls())
load("../data/ERPbeh/tidied/nmap_data_tidied.RData") 

# wide format acc already calculated in a tibble, now output
write.csv(nmap_acc.wide, file = "acc_subj_6cond_wide.csv", row.names = FALSE)
#

acc.by.subj.cond <- nmap_data.long%>%
  group_by(Subject, cond) %>%
  summarise(individual_acc = mean(correct_or_not, na.rm=T))

write.csv(acc.by.subj.cond, file = "acc_subj_cond.csv")


acc.by.subj.cond %>%
  ggplot(aes(x=cond, y=individual_acc)) +
  geom_jitter()

acc.by.subj.cond %>%
  group_by(cond) %>%
  summarise(group_acc = mean(individual_acc)) %>%
  ungroup() %>%
  mutate(cond = factor(cond, levels = c("close", "med", "far"))) %>%
  ggplot(aes(x=cond, y=group_acc, group = 1)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks=seq(0.0, 1, 0.1), limits=c(0.3,0.7))
  


mym <- glmer(correct_or_not ~ cond + (1|Subject), family = "binomial", data = nmap_data.long)
summary(mym)
Anova(mym)

