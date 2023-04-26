# Set up -----
library("tidyverse")
library("readxl")
library("lme4")
library("lmerTest")
library("lattice") # used for visual inspection of normal distribution of residuals
library("car") # leveneTest
library("emmeans") # extract estimated marginal means
library("langcog") # for non-paramatric bootstrap CI

rm(list = ls())
load("../data/ERP/tidied/erp_tidied.RData")

theme_set(theme_bw())

# Explicit Data type -----
data_erp_all <- data_erp_all %>%
  mutate(KL.cat = 
           case_when(
             KL.cont %in% c(1:4) ~ "SS",
             KL.cont %in% c(5:8) ~ "CP",
             TRUE ~ as.character(NA)))
         
data_erp_all$subj_num <- as.factor(data_erp_all$subj_num)
data_erp_all$time_point <- as.factor(data_erp_all$time_point)

data_erp_all$ratio <- factor(data_erp_all$ratio, levels = c("close", "med", "far")) 
data_erp_all$distance <- as.numeric(data_erp_all$distance)

glimpse(data_erp_all)

# Data summarization -----

# number of unique subjects
length(unique(data_erp_all$subj_num))

# number of sessions
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

# FIT LME MODEL -----
## N1 -----
data.n1 <- data_erp_all %>% 
  filter(component == "n1") 
  
glimpse(data.n1)
### Cardinal -----
model.n1.cardinal.full <- lmerTest::lmer(amp ~ cardinal + KL.cat + cardinal:KL.cat + time_point + (1|subj_num), # correlated slope & intercept
                                         data = data.n1, REML = T)
# cardinal as factor
model.n1.cardinal.full.factor <- data.n1 %>%
  mutate(cardinal = factor(cardinal)) %>%
  lmerTest::lmer(data = ., amp ~ cardinal + KL.cat + cardinal:KL.cat + time_point + (1|subj_num), # correlated slope & intercept
                                         REML = T)
# model.n1.cardinal.SS <- lmerTest::lmer(cond.mean.amp ~ cardinal + time_point + (time_point|subj_num), # resulted in singular fit
#                                      data = subset(data.n1, KL.cat == "SS"), REML = T)
model.n1.cardinal.SS <- lmerTest::lmer(amp ~ cardinal + time_point + (1|subj_num), # random subject intercept
                                       data = subset(data.n1, KL.cat == "SS"), REML = T)

model.n1.cardinal.CP <- lmerTest::lmer(amp ~ cardinal + time_point + (1|subj_num), # random subject intercept
                                       data = subset(data.n1, KL.cat == "CP"), REML = T)
#### test model assumption -----
# plot(resid(model.n1.cardinal.full), data.n1$amp) # Linearity (visual inspection)
# qqmath(model.n1.cardinal.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.n1.cardinal.full) ~ data.n1$KL.cat) # Homoscedasticity
# 
## ### read model output -----
ranova(model.n1.cardinal.full)
ranova(model.n1.cardinal.SS)
ranova(model.n1.cardinal.CP)

summary(model.n1.cardinal.full) # significant cardinal effect
summary(model.n1.cardinal.SS) # significant cardinal effect
summary(model.n1.cardinal.CP) # significant cardinal effect


# #### Data for plot: emmeans & pred -----
emmean.n1.cardinal <- emmeans(model.n1.cardinal.full.factor, pairwise~cardinal|KL.cat, # within group comparison: compare levels of ratio within each level of KL
                              mode = "satterthwaite",
                              lmerTest.limit = 240000)
# emmean.n1.cardinal$contrasts %>% data.frame()
# data.emmean.n1.cardinal <- emmean.n1.cardinal$emmeans %>% data.frame() # used for plot

# data.n1.pred <- tibble(pred.y = predict(model.n1.cardinal.full),
#                        cardinal = data.n1$cardinal,
#                        KL.cat = data.n1$KL.cat,
#                        time_point = data.n1$time_point,
#                        subj_num = data.n1$subj_num) %>%
#   mutate(KL.cat = factor(KL.cat, levels = c("SS", "CP")),
#          cardinal = factor(cardinal))

#### plots-----
# ggplot(data = data.emmean.n1.cardinal, aes(x=cardinal, y=emmean, color = KL.cat, group = KL.cat)) + 
#   geom_point(position=position_dodge(.1), size = 2) +
#   geom_line() +
#   geom_errorbar(aes(x = cardinal, ymin=lower.CL, ymax=upper.CL, color = KL.cat),
#                 width = .2, linewidth=.6, position=position_dodge(.1)) +
#   labs(x = "Visual Cardinal Values", y = "Estimated marginal means of N1 amplitude (mV)", color = "CP status")
  
data.n1 %>%
  mutate(cardinal = factor(cardinal)) %>%
  group_by(cardinal, KL.cat) %>%
  multi_boot_standard("amp") %>%
  drop_na() %>%
  ggplot(aes(x=cardinal, y=mean, group=KL.cat, color=KL.cat)) +
  geom_point(position=position_dodge(.1), size = 2.5) + 
  geom_line(position=position_dodge(.1), linewidth=.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                 position=position_dodge(.1), width=.2, linewidth=.6) +
  labs(x = "Visual Cardinal Values", y = "Estimated marginal means of N1 amplitude (mV)", color = "CP status")



## P2p -----
data.p2p <- data_erp_all %>% filter(component == "p2p") %>%
  mutate(ratio.num = case_when(
    ratio == "far" ~ 3,
    ratio == "med" ~ 2,
    ratio == "close" ~ 1.5
  ))
glimpse(data.p2p)

### Ratio -----

model.p2p.ratio.full.factor <- lmerTest::lmer(amp ~ ratio * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                               data = data.p2p, REML = T)
model.p2p.ratio.full <- lmerTest::lmer(amp ~ ratio.num * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                       data = data.p2p, REML = T)

#### test model assumption -----
# plot(resid(model.p2p.ratio.full), data.p2p$amp) # Linearity (visual inspection)
# qqmath(model.p2p.ratio.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.p2p.ratio.full) ~ data.p2p$ratio) # Homoscedasticity


# break down KL levels and test linear effect of ratio
model.p2p.ratio.SS <- lmerTest::lmer(amp ~ ratio.num + time_point + (1|subj_num), # correlated slope & intercept
               data = subset(data.p2p, KL.cat == "SS"), REML = T)
model.p2p.ratio.CP <- lmerTest::lmer(amp ~ ratio.num + time_point + (1|subj_num), # correlated slope & intercept
                               data = subset(data.p2p, KL.cat == "CP"), REML = T)


#### read model output -----
summary(model.p2p.ratio.full)
summary(model.p2p.ratio.full.factor)
summary(model.p2p.ratio.SS)
summary(model.p2p.ratio.CP)

#### Data for plot: emmeans & pred -----
# emmean.p2p.ratio <- emmeans(model.p2p.ratio.full.factor, pairwise~ratio|KL.cat, # within group comparison: compare levels of ratio within each level of KL
#                             mode = "satterthwaite",
#                             lmerTest.limit = 240000)
# emmean.p2p.ratio$contrasts %>% data.frame()
# data.emmean.p2p.ratio <- emmean.p2p.ratio$emmeans %>% 
#   data.frame() %>% 
#   mutate(ratio.num = case_when(
#     ratio == "far" ~ 3,
#     ratio == "med" ~ 2,
#     ratio == "close" ~ 1.5)) # used for plot
# 
# data.p2p.pred <- tibble(pred.y = predict(model.p2p.ratio.full),
#                         ratio.num = data.p2p$ratio.num,
#                         KL.cat = data.p2p$KL.cat,
#                         time_point = data.p2p$time_point,
#                         subj_num = data.p2p$subj_num) %>%
#   mutate(KL.cat = factor(KL.cat, levels = c("SS", "CP")))

#### plots-----
# ggplot() + 
#   geom_smooth(data=data.p2p.pred, aes(x = ratio.num, y = pred.y,
#                                      group = KL.cat, color = KL.cat), 
#               method = "lm", se = FALSE, linewidth = 1.2) +
#   geom_point(aes(x=ratio.num, y=emmean, color = KL.cat), data=data.emmean.p2p.ratio, 
#              position=position_dodge(.1), size = 2) +
#   geom_errorbar(data = data.emmean.p2p.ratio, aes(x = ratio.num, ymin=lower.CL, ymax=upper.CL, color = KL.cat),
#                 width = .2, linewidth=.6, position=position_dodge(.1)) +
#   labs(x = "Numerical ratio", y = "Estimated marginal means of P2p amplitude (mV)", color = "CP status")
# 

data.p2p %>%
  # mutate(cardinal = factor(cardinal)) %>%
  group_by(ratio.num, KL.cat) %>%
  multi_boot_standard("amp") %>%
  drop_na() %>%
  mutate(distance = case_when(
    ratio.num == 1.5 ~ 1,
    ratio.num == 2 ~ 1,
    ratio.num == 3 ~ 2)) %>%
  ggplot(aes(x=ratio.num, y=mean, group=KL.cat, color=KL.cat)) +
  geom_point(position=position_dodge(.1), size = 2.5) + 
  geom_line(position=position_dodge(.1), linewidth=.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position=position_dodge(.1), width=.2, linewidth=.6) +
  labs(x = "Numerical ratio", y = "Estimated marginal means of P2p amplitude (mV)", color = "CP status")

### Distance -----
model.p2p.distance.full <- lmerTest::lmer(amp ~ distance * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                       data = data.p2p, REML = T)

# break down KL levels and test linear effect of distance
model.p2p.distance.SS <- lmerTest::lmer(amp ~ distance + time_point + (1|subj_num), # correlated slope & intercept
                                     data = subset(data.p2p, KL.cat == "SS"), REML = T)
model.p2p.distance.CP <- lmerTest::lmer(amp ~ distance + time_point + (1|subj_num), # correlated slope & intercept
                                     data = subset(data.p2p, KL.cat == "CP"), REML = T)
#### test model assumption -----
# plot(resid(model.p2p.distance.full), data.p2p$amp) # Linearity (visual inspection)
# qqmath(model.p2p.distance.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.p2p.distance.full) ~ data.p2p$distance) # Homoscedasticity
# 
## ### read model output -----

anova(model.p2p.distance.full)
anova(model.p2p.distance.SS)
anova(model.p2p.distance.CP)
summary(model.p2p.distance.full)
summary(model.p2p.distance.SS)
summary(model.p2p.distance.CP)


# #### Data for plot: emmeans & pred -----
# emmean.p2p.distance <- emmeans(model.p2p.distance.full, pairwise~distance|KL.cat, # within group comparison: compare levels of distance within each level of KL
#                             mode = "satterthwaite",
#                             lmerTest.limit = 240000)
# emmean.p2p.distance$contrasts %>% data.frame()
# data.emmean.p2p.distance <- emmean.p2p.distance$emmeans %>% 
#   data.frame()  # used for plot
# 
# data.p2p.pred <- tibble(pred.y = predict(model.p2p.distance.full),
#                         distance = data.p2p$distance,
#                         KL.cat = data.p2p$KL.cat,
#                         time_point = data.p2p$time_point,
#                         subj_num = data.p2p$subj_num) %>%
#   mutate(KL.cat = factor(KL.cat, levels = c("SS", "CP")))

#### plots-----
# ggplot() + 
#   geom_smooth(data=data.p2p.pred, aes(x = distance, y = pred.y,
#                                       group = KL.cat, color = KL.cat), 
#               method = "lm", se = FALSE, linewidth = 1.2) +
#   geom_point(aes(x=distance, y=emmean, color = KL.cat), data=data.emmean.p2p.distance, 
#              position=position_dodge(.1), size = 2) +
#   geom_errorbar(data = data.emmean.p2p.distance, aes(x = distance, ymin=lower.CL, ymax=upper.CL, color = KL.cat),
#                 width = .2, linewidth=.6, position=position_dodge(.1)) +
#   labs(x = "Absolute distance", y = "Estimated marginal means of P2p amplitude (mV)", color = "CP status") +
#   scale_x_continuous(breaks=c(1, 2))
  

data.p2p %>%
  mutate(distance = factor(distance)) %>%
  group_by(distance, KL.cat) %>%
  multi_boot_standard("amp") %>%
  drop_na() %>%
  ggplot(aes(x=distance, y=mean, group=KL.cat, color=KL.cat)) +
  geom_point(position=position_dodge(.1), size = 2.5) + 
  geom_line(position=position_dodge(.1), linewidth=.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position=position_dodge(.1), width=.2, linewidth=.6) +
  labs(x = "Numerical distance", y = "Estimated marginal means of P2p amplitude (mV)", color = "CP status")

## N2 -----
data.n2 <- data_erp_all %>% filter(component == "n2") %>%
  mutate(ratio.num = case_when(
    ratio == "far" ~ 3,
    ratio == "med" ~ 2,
    ratio == "close" ~ 1.5
  )) 
glimpse(data.n2)

#

model.n2. <- lmerTest::lmer(amp ~ distance + ratio.num + KL.cat + distance:KL.cat + ratio.num:KL.cat  + time_point + (time_point|subj_num), # correlated slope & intercept
                                         data = data.n2, REML = T)
model.n2.noKLmain <- lmerTest::lmer(amp ~ distance + ratio.num + distance:KL.cat + ratio.num:KL.cat  + time_point + (time_point|subj_num), # correlated slope & intercept
                            data = data.n2, REML = T)
model.n2.CP <- lmerTest::lmer(amp ~ distance + ratio.num + time_point + (time_point|subj_num), # correlated slope & intercept
                            data = subset(data.n2, KL.cat == "CP"), REML = T)

model.n2.SS <- lmerTest::lmer(amp ~ distance + ratio.num + time_point + (time_point|subj_num), # correlated slope & intercept
                            data = subset(data.n2, KL.cat == "SS"), REML = T)

model.n2.reducedistance <- lmerTest::lmer(amp ~ ratio.num + KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                            data = data.n2, REML = T)
model.n2.reduceratio <- lmerTest::lmer(amp ~ distance + KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                          data = data.n2, REML = T)
summary(model.n2.)
summary(model.n2.CP)
summary(model.n2.SS)

summary(model.n2.reducedistance)
summary(model.n2.reduceratio)
anova(model.n2., model.n2.noKLmain)

data.n2.nooutlier <- data.n2 %>% filter(!amp < -100)
### Ratio -----
model.n2.ratio.full.factor <- lmerTest::lmer(amp ~ ratio * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                              data = data.n2, REML = T)
model.n2.ratio.full <- lmerTest::lmer(amp ~ ratio.num * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                       data = data.n2, REML = T)
#### test model assumption -----
# plot(resid(model.n2.ratio.full), data.n2.nooutlier$amp) # Linearity (visual inspection)
# qqmath(model.n2.ratio.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.n2.ratio.full) ~ data.n2.nooutlier$ratio) # Homoscedasticity

# break down KL levels and test linear effect of ratio

model.n2.ratio.SS <- lmerTest::lmer(amp ~ ratio.num + time_point + (time_point|subj_num), # correlated slope & intercept
                                     data = subset(data.n2, KL.cat == "SS"), REML = T)
model.n2.ratio.CP <- lmerTest::lmer(amp ~ ratio.num + time_point + (time_point|subj_num), # correlated slope & intercept
                                     data = subset(data.n2, KL.cat == "CP"), REML = T)

model.n2.ratio.SS.factor <- lmerTest::lmer(amp ~ ratio + time_point + (time_point|subj_num), # correlated slope & intercept
                                    data = subset(data.n2, KL.cat == "SS"), REML = T)
model.n2.ratio.CP.factor <- lmerTest::lmer(amp ~ ratio + time_point + (time_point|subj_num), # correlated slope & intercept
                                    data = subset(data.n2, KL.cat == "CP"), REML = T)

# plot(resid(model.n2.ratio.SS), subset(data.n2.nooutlier, KL.cat == "SS")$amp) # Linearity (visual inspection)
# qqmath(model.n2.ratio.SS) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.n2.ratio.SS) ~ subset(data.n2.nooutlier, KL.cat == "SS")$ratio) # Homoscedasticity
# 
# plot(resid(model.n2.ratio.CP), subset(data.n2.nooutlier, KL.cat == "CP")$amp) # Linearity (visual inspection)
# qqmath(model.n2.ratio.CP) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.n2.ratio.CP) ~ subset(data.n2.nooutlier, KL.cat == "CP")$ratio) # Homoscedasticity
# 
#### read model output -----

anova(model.n2.ratio.full)
anova(model.n2.ratio.SS)
anova(model.n2.ratio.CP)
summary(model.n2.ratio.full)
summary(model.n2.ratio.SS)
summary(model.n2.ratio.CP)

# summary(model.n2.ratio.full.factor)
# summary(model.n2.ratio.SS.factor)
# summary(model.n2.ratio.CP.factor)


# #### Data for plot: emmeans & pred -----
# emmean.n2.ratio <- emmeans(model.n2.ratio.full.factor, pairwise~ratio|KL.cat, # within group comparison: compare levels of ratio within each level of KL
#                             mode = "satterthwaite",
#                             lmerTest.limit = 240000)
# emmean.n2.ratio$contrasts %>% data.frame()
# data.emmean.n2.ratio <- emmean.n2.ratio$emmeans %>% 
#   data.frame() %>% 
#   mutate(ratio.num = case_when(
#     ratio == "far" ~ 3,
#     ratio == "med" ~ 2,
#     ratio == "close" ~ 1.5)) # used for plot
# 
# data.n2.pred <- tibble(pred.y = predict(model.n2.ratio.full),
#                         ratio.num = data.n2$ratio.num,
#                         KL.cat = data.n2$KL.cat,
#                         time_point = data.n2$time_point,
#                         subj_num = data.n2$subj_num) %>%
#   mutate(KL.cat = factor(KL.cat, levels = c("SS", "CP")))

#### plots-----
ggplot() + 
  geom_smooth(data=data.n2.pred, aes(x = ratio.num, y = pred.y,
                                      group = KL.cat, color = KL.cat), 
              method = "lm", se = FALSE, linewidth = 1.2) +
  geom_point(aes(x=ratio.num, y=emmean, color = KL.cat), data=data.emmean.n2.ratio, 
             position=position_dodge(.1), size = 2) +
  geom_errorbar(data = data.emmean.n2.ratio, aes(x = ratio.num, ymin=lower.CL, ymax=upper.CL, color = KL.cat),
                width = .2, linewidth=.6, position=position_dodge(.1)) +
  labs(x = "Numerical ratio", y = "Estimated marginal means of n2 amplitude (mV)", color = "CP status")

data.n2 %>%
  group_by(ratio.num, KL.cat) %>%
  multi_boot_standard("amp") %>%
  drop_na() %>%
  ggplot(aes(x=ratio.num, y=mean, group=KL.cat, color=KL.cat)) +
  geom_point(position=position_dodge(.1), size = 2.5) + 
  geom_line(position=position_dodge(.1), linewidth=.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position=position_dodge(.1), width=.2, linewidth=.6) +
  labs(x = "Numerical ratio", y = "Estimated marginal means of N2 amplitude (mV)", color = "CP status")
### Distance -----

model.n2.distance.full <- lmerTest::lmer(amp ~ distance * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                              data = data.n2, REML = T)


#### test model assumption -----
# plot(resid(model.n2.distance.full), data.n2.nooutlier$amp) # Linearity (visual inspection)
# qqmath(model.n2.distance.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.n2.distance.full) ~ data.n2.nooutlier$distance) # Homoscedasticity
# plot(model.n2.distance.full)
# boxplot(data=data.n2, amp ~ distance)

# break down KL levels and test linear effect of distance
model.n2.distance.SS <- lmerTest::lmer(amp ~ distance + time_point + (time_point|subj_num), # correlated slope & intercept
                                    data = subset(data.n2, KL.cat == "SS"), REML = T)
model.n2.distance.CP <- lmerTest::lmer(amp ~ distance + time_point + (time_point|subj_num), # correlated slope & intercept
                                    data = subset(data.n2, KL.cat == "CP"), REML = T)
#### test model assumption -----
# plot(resid(model.n2.full), data_sample$erp.n2) # Linearity (visual inspection)
# qqmath(model.n2.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.n2.full) ~ data_sample$distance) # Homoscedasticity
# 
# plot(resid(model.n2.distance.SS), subset(data.n2.nooutlier, KL.cat == "SS")$amp) # Linearity (visual inspection)
# qqmath(model.n2.distance.SS) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.n2.distance.SS) ~ subset(data.n2.nooutlier, KL.cat == "SS")$ratio) # Homoscedasticity
# 
# plot(resid(model.n2.distance.CP), subset(data.n2.nooutlier, KL.cat == "CP")$amp) # Linearity (visual inspection)
# qqmath(model.n2.distance.CP) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.n2.distance.CP) ~ subset(data.n2.nooutlier, KL.cat == "CP")$ratio) # Homoscedasticity
# 

## ### read model output -----

anova(model.n2.distance.full)
anova(model.n2.distance.SS)
anova(model.n2.distance.CP)
summary(model.n2.distance.full)
summary(model.n2.distance.SS)
summary(model.n2.distance.CP)


#### Data for plot: emmeans & pred -----
emmean.n2.distance <- emmeans(model.n2.distance.full, pairwise~distance|KL.cat, # within group comparison: compare levels of distance within each level of KL
                               mode = "satterthwaite",
                               lmerTest.limit = 240000)
emmean.n2.distance$contrasts %>% data.frame()
data.emmean.n2.distance <- emmean.n2.distance$emmeans %>% 
  data.frame()  # used for plot

data.n2.pred <- tibble(pred.y = predict(model.n2.distance.full),
                        distance = data.n2$distance,
                        KL.cat = data.n2$KL.cat,
                        time_point = data.n2$time_point,
                        subj_num = data.n2$subj_num) %>%
  mutate(KL.cat = factor(KL.cat, levels = c("SS", "CP")))

#### plots-----
ggplot() + 
  geom_smooth(data=data.n2.pred, aes(x = distance, y = pred.y,
                                      group = KL.cat, color = KL.cat), 
              method = "lm", se = FALSE, linewidth = 1.2) +
  geom_point(aes(x=distance, y=emmean, color = KL.cat), data=data.emmean.n2.distance, 
             position=position_dodge(.1), size = 2) +
  geom_errorbar(data = data.emmean.n2.distance, aes(x = distance, ymin=lower.CL, ymax=upper.CL, color = KL.cat),
                width = .2, linewidth=.6, position=position_dodge(.1)) +
  labs(x = "Absolute distance", y = "Estimated marginal means of n2 amplitude (mV)", color = "CP status") +
  scale_x_continuous(breaks=c(1, 2))

