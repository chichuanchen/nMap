# Set up -----
library("tidyverse")
library("readxl")
library("lme4")
library("lmerTest")
library("lattice") # used for visual inspection of normal distribution of residuals
library("car") # leveneTest
library("emmeans") # extract estimated marginal means

rm(list = ls())
load("../data/ERP/tidied/erp_tidied.RData")

# test
# Explicit Data type -----
data_erp_all <- data_erp_all %>%
  mutate(KL.cat = factor(if_else(KL.cont %in% c(1:4), "SS", "CP")))
data_erp_all$subj_num <- as.factor(data_erp_all$subj_num)
data_erp_all$time_point <- as.factor(data_erp_all$time_point)

data_erp_all$ratio <- factor(data_erp_all$ratio, levels = c("close", "med", "far")) 
data_erp_all$distance <- as.numeric(data_erp_all$distance)

glimpse(data_erp_all)


# FIT LME MODEL -----
## N1 -----
data.n1 <- data_erp_all %>% filter(component == "n1")
glimpse(data.n1)
### Cardinal -----
model.n1.cardinal.full <- lmerTest::lmer(amp ~ cardinal + KL.cat + cardinal:KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                         data = data.n1, REML = T)

# model.n1.cardinal.SS <- lmerTest::lmer(amp ~ cardinal + time_point + (time_point|subj_num), # resulted in singular fit
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

anova(model.n1.cardinal.full)
Anova(model.n1.cardinal.full, type="III")

anova(model.n1.cardinal.SS)
anova(model.n1.cardinal.CP)
summary(model.n1.cardinal.full)
summary(model.n1.cardinal.SS)
summary(model.n1.cardinal.CP)

#### emmeans -----
emmean.n1.cardinal <- emmeans(model.n1.cardinal.full, pairwise~cardinal|KL.cat, # within group comparison: compare levels of ratio within each level of KL
                              mode = "satterthwaite", 
                              lmerTest.limit = 240000)
emmean.n1.cardinal$contrasts %>% data.frame()
data.emmean.n1.cardinal <- emmean.n1.cardinal$emmeans %>% data.frame() # used for plot

#### plots-----

data_plot.indi <- data.n1 %>%
  group_by(cardinal, KL.cat, subj_num, time_point) %>%
  summarise(ind.mean_erp.n1 = mean(amp, na.rm=T)) 

ggplot(data = data.emmean.n1.cardinal,
       aes(x=cardinal, y=emmean, color=KL.cat, group=KL.cat)) +
  geom_point(position=position_dodge(.3)) +
  geom_line(position=position_dodge(.3)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                width = .2, linewidth=.8, position=position_dodge(.3)) +
  labs(x = "Cardinal Value of Visual Quantity", y="Estimated Marginal Means of N1 Amplitude (mV)",
       title = "Error Bars = 95% C.I.", color = "Knower-level") 
# +
#   
#   geom_jitter(data = data_plot.indi,
#   aes(x=cardinal, y=ind.mean_erp.n1, shape=time_point),position=position_dodge(.9))


## P2p -----
data.p2p <- data_erp_all %>% filter(component == "p2p")
### Ratio -----

model.p2p.ratio.full <- lmerTest::lmer(amp ~ ratio * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                               data = data.p2p, REML = T)


#### test model assumption -----
# plot(resid(model.p2p.ratio.full), data.p2p$amp) # Linearity (visual inspection)
# qqmath(model.p2p.ratio.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.p2p.ratio.full) ~ data.p2p$ratio) # Homoscedasticity


# break down KL levels and test linear effect of ratio
model.p2p.ratio.SS <- lmerTest::lmer(amp ~ ratio + time_point + (time_point|subj_num), # correlated slope & intercept
               data = subset(data.p2p, KL.cat == "SS"), REML = T)
model.p2p.ratio.CP <- lmerTest::lmer(amp ~ ratio + time_point + (time_point|subj_num), # correlated slope & intercept
                               data = subset(data.p2p, KL.cat == "CP"), REML = T)


#### read model output -----

anova(model.p2p.ratio.full)
anova(model.p2p.ratio.SS)
anova(model.p2p.ratio.CP)

summary(model.p2p.ratio.full)
summary(model.p2p.ratio.SS)
summary(model.p2p.ratio.CP)

# fixef(model.p2p.ratio.CP)
# confint(model.p2p.ratio.CP, oldNames = FALSE)
#### Linearity of ratio effect -----
contrasts(data.p2p$ratio) # check contrast

# define linear contrast from close, med, to far, and reverse
contrasts(data.p2p$ratio) <- matrix(c(-1,0,1,1,0,-1), ncol = 2)
contrasts(data_N2_P2p_375_475$ratio) <- matrix(c(0,1,2,2,1,0), ncol = 2)
model.p2p.ratio.CP.linear <- lmerTest::lmer(amp ~ ratio + time_point + (time_point|subj_num), # correlated slope & intercept
                                     data = subset(data.p2p, KL.cat == "CP"), REML = T)
summary(model.p2p.ratio.CP.linear) # not sig

# #### emmeans 
emmean.p2p.ratio <- emmeans(model.p2p.ratio.full, pairwise~ratio|KL.cat, # within group comparison: compare levels of ratio within each level of KL
                            mode = "satterthwaite", 
                            lmerTest.limit = 240000)
emmean.p2p.ratio$contrasts %>% data.frame()
data.emmean.p2p.ratio <- emmean.p2p.ratio$emmeans %>% data.frame() # used for plot

#### plots-----
data_plot.indi <- data.p2p %>%
  group_by(ratio, KL.cat, subj_num, time_point) %>%
  summarise(ind.mean_erp.p2p = mean(amp, na.rm=T)) 

ggplot(data = data.emmean.p2p.ratio,
       aes(x=ratio, y=emmean, color=KL.cat, group=KL.cat)) +
  geom_point(position=position_dodge(.3)) +
  geom_line(position=position_dodge(.3)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                width = .2, linewidth=.8, position=position_dodge(.3)) +
  labs(x = "Numerical Ratio between Number Word and Quantity", y="Estimated Marginal Means of P2p Amplitude (mV)",
       title = "Error Bars = 95% C.I.", color = "Knower-level")
  # geom_jitter(data = data_plot.indi, 
  #            aes(x=ratio, y=ind.mean_erp.p2p, shape=time_point),position=position_dodge(.9))


### Distance -----
model.p2p.distance.full <- lmerTest::lmer(amp ~ distance * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                       data = data.p2p, REML = T)

# break down KL levels and test linear effect of distance
model.p2p.distance.SS <- lmerTest::lmer(amp ~ distance + time_point + (time_point|subj_num), # correlated slope & intercept
                                     data = subset(data.p2p, KL.cat == "SS"), REML = T)
model.p2p.distance.CP <- lmerTest::lmer(amp ~ distance + time_point + (time_point|subj_num), # correlated slope & intercept
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


# #### emmeans 
emmean.p2p.distance <- emmeans(model.p2p.distance.full, pairwise~distance|KL.cat, # within group comparison: compare levels of distance within each level of KL
                            mode = "satterthwaite", 
                            lmerTest.limit = 240000)
emmean.p2p.distance$contrasts %>% data.frame()
data.emmean.p2p.distance <- emmean.p2p.distance$emmeans %>% data.frame() # used for plot

#### plots-----
data_plot.indi <- data.p2p %>%
  group_by(distance, KL.cat, subj_num, time_point) %>%
  summarise(ind.mean_erp.p2p = mean(amp, na.rm=T)) 

ggplot(data = data.emmean.p2p.distance,
       aes(x=distance, y=emmean, color=KL.cat, group=KL.cat)) +
  geom_point(position=position_dodge(.3)) +
  geom_line(position=position_dodge(.3)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                width = .2, linewidth=.8, position=position_dodge(.3)) +
  labs(x = "Numerical Distance between Number Word and Quantity", y="Estimated Marginal Means of P2p Amplitude (mV)",
       title = "Error Bars = 95% C.I.", color = "Knower-level")
  

## N2 -----
data.n2 <- data_erp_all %>% filter(component == "n2")
data.n2.nooutlier <- data.n2 %>% filter(!amp < -100)
### Ratio -----
model.n2.ratio.full <- lmerTest::lmer(amp ~ ratio * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                      data = data.n2.nooutlier, REML = T)

#### test model assumption -----
# plot(resid(model.n2.ratio.full), data.n2.nooutlier$amp) # Linearity (visual inspection)
# qqmath(model.n2.ratio.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.n2.ratio.full) ~ data.n2.nooutlier$ratio) # Homoscedasticity

# break down KL levels and test linear effect of ratio

model.n2.ratio.SS <- lmerTest::lmer(amp ~ ratio + time_point + (time_point|subj_num), # correlated slope & intercept
                                     data = subset(data.n2.nooutlier, KL.cat == "SS"), REML = T)
model.n2.ratio.CP <- lmerTest::lmer(amp ~ ratio + time_point + (time_point|subj_num), # correlated slope & intercept
                                     data = subset(data.n2.nooutlier, KL.cat == "CP"), REML = T)

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

#### emmeans 
emmean.n2.ratio <- emmeans(model.n2.ratio.full, pairwise~ratio|KL.cat, # within group comparison: compare levels of ratio within each level of KL
                            mode = "satterthwaite", 
                            lmerTest.limit = 240000)
emmean.n2.ratio$contrasts %>% data.frame()
data.emmean.n2.ratio <- emmean.n2.ratio$emmeans %>% data.frame() # used for plot

#### plots-----
data_plot.indi <- data.n2 %>%
  group_by(ratio, KL.cat, subj_num, time_point) %>%
  summarise(ind.mean_erp.n2 = mean(amp, na.rm=T)) 

ggplot(data = data.emmean.n2.ratio,
       aes(x=ratio, y=emmean, color=KL.cat, group=KL.cat)) +
  geom_point(position=position_dodge(.3)) +
  geom_line(position=position_dodge(.3)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                width = .2, linewidth=.8, position=position_dodge(.3)) +
  labs(x = "Numerical Ratio between Number Word and Quantity", y="Estimated Marginal Means of N2 Amplitude (mV)",
       title = "Error Bars = 95% C.I.", color = "Knower-level")


### Distance -----

model.n2.distance.full <- lmerTest::lmer(amp ~ distance * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                              data = data.n2.nooutlier, REML = T)


#### test model assumption -----
# plot(resid(model.n2.distance.full), data.n2.nooutlier$amp) # Linearity (visual inspection)
# qqmath(model.n2.distance.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.n2.distance.full) ~ data.n2.nooutlier$distance) # Homoscedasticity
# plot(model.n2.distance.full)
# boxplot(data=data.n2, amp ~ distance)

# break down KL levels and test linear effect of distance
model.n2.distance.SS <- lmerTest::lmer(amp ~ distance + time_point + (time_point|subj_num), # correlated slope & intercept
                                    data = subset(data.n2.nooutlier, KL.cat == "SS"), REML = T)
model.n2.distance.CP <- lmerTest::lmer(amp ~ distance + time_point + (time_point|subj_num), # correlated slope & intercept
                                    data = subset(data.n2.nooutlier, KL.cat == "CP"), REML = T)
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

#### emmeans -----
emmean.n2.distance <- emmeans(model.n2.distance.full, pairwise~distance|KL.cat, # within group comparison: compare levels of ratio within each level of KL
                           mode = "satterthwaite", 
                           lmerTest.limit = 240000)
emmean.n2.distance$contrasts %>% data.frame()
data.emmean.n2.distance <- emmean.n2.distance$emmeans %>% data.frame() # used for plot


#### plots-----

data_plot.indi <- data.n2 %>%
  group_by(distance, KL.cat, subj_num, time_point) %>%
  summarise(ind.mean_erp.n2 = mean(amp, na.rm=T)) 

ggplot(data = data.emmean.n2.distance,
       aes(x=distance, y=emmean, color=KL.cat, group=KL.cat)) +
  geom_point(position=position_dodge(.3)) +
  geom_line(position=position_dodge(.3)) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL),
                width = .2, linewidth=.8, position=position_dodge(.3)) +
  labs(x = "Numerical Distance between Number Word and Quantity", y="Estimated Marginal Means of N2 Amplitude (mV)",
       title = "Error Bars = 95% C.I.", color = "Knower-level")





