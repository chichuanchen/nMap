# Set up -----
library("tidyverse")
library("readxl")
library("lme4")
library("lmerTest")
library("lattice") # used for visual inspection of normal distribution of residuals
library("car") # leveneTest
library("emmeans") # extract estimated marginal means

rm(list = ls())
load("../tidied/erp_tidied.RData")

# Define categorical KL
data_N2_P2p_375_475 <- data_N2_P2p_375_475 %>%
  mutate(KL.cat = if_else(KL.cont %in% c(1:4), "SS", "CP"))

data_N1_P2a <- data_N1_P2a %>%
  mutate(KL.cat = if_else(KL.cont %in% c(1:4), "SS", "CP"))

# Explicit Data type -----
data_N2_P2p_375_475$subj_num <- as.factor(data_N2_P2p_375_475$subj_num)
data_N2_P2p_375_475$time_point <- as.factor(data_N2_P2p_375_475$time_point)
data_N2_P2p_375_475$ratio <- factor(data_N2_P2p_375_475$ratio, levels = c("close", "med", "far")) 
data_N2_P2p_375_475$KL.cat <- factor(data_N2_P2p_375_475$KL.cat, levels = c("SS", "CP")) 
data_N2_P2p_375_475$distance <- factor(data_N2_P2p_375_475$distance, levels = c("1", "2"))

data_N1_P2a$subj_num <- as.factor(data_N1_P2a$subj_num)
data_N1_P2a$time_point <- as.factor(data_N1_P2a$time_point)
data_N1_P2a$KL.cat <- factor(data_N1_P2a$KL.cat, levels = c("SS", "CP")) 
data_N1_P2a$cardinal <- factor(data_N1_P2a$cardinal, levels = c("1", "2","3"))

#
# data_sample$ratio <- factor(data_sample$ratio, levels = c("med", "far", "close")) 
# contrasts(data_sample$ratio) <- contr.Sum(levels(data_sample$ratio)) 
# contrasts(data_sample$distance) <- contr.Sum(levels(data_sample$distance)) 

# FIT LME MODEL -----

## P2p -----

### Ratio -----

model.p2p.ratio.full <- lmerTest::lmer(erp.p2p ~ ratio * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                               data = data_sample, REML = T)
# model.p2p.ratio.full2 <- lmerTest::lmer(erp.p2p ~ ratio * KL.cat * time_point + (time_point|subj_num), # correlated slope & intercept
#                                        data = data_sample, REML = T)
# model.p2p.ratio.reduced <- lmerTest::lmer(erp.p2p ~ ratio + KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
#                                  data = data_sample, REML = T)
#### test model assumption -----
# plot(resid(model.p2p.full), data_sample$erp.p2p) # Linearity (visual inspection)
# qqmath(model.p2p.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.p2p.full) ~ data_sample$ratio) # Homoscedasticity


# break down KL levels and test linear effect of ratio
model.p2p.ratio.SS <- lmerTest::lmer(erp.p2p ~ ratio + time_point + (time_point|subj_num), # correlated slope & intercept
               data = subset(data_sample, KL.cat == "SS"), REML = T)
model.p2p.ratio.CP <- lmerTest::lmer(erp.p2p ~ ratio + time_point + (time_point|subj_num), # correlated slope & intercept
                               data = subset(data_sample, KL.cat == "CP"), REML = T)


#### read model output -----

anova(model.p2p.ratio.full)
anova(model.p2p.ratio.SS)
anova(model.p2p.ratio.CP)

summary(model.p2p.ratio.SS)
summary(model.p2p.ratio.CP)
summary(model.p2p.ratio.full)

#### Linearity of ratio effect -----
contrasts(data_sample$ratio) # check contrast

# define linear contrast from close, med, to far, and reverse
contrasts(data_sample$ratio) <- matrix(c(-1,0,1,1,0,-1), ncol = 2)
contrasts(data_sample$ratio) <- matrix(c(0,1,2,2,1,0), ncol = 2)
model.p2p.ratio.CP.linear <- lmerTest::lmer(erp.p2p ~ ratio + time_point + (time_point|subj_num), # correlated slope & intercept
                                     data = subset(data_sample, KL.cat == "CP"), REML = T)
summary(model.p2p.ratio.CP.linear) # not sig

# #### emmeans 
# mLMEPop <- emmeans(model.p2p.full, pairwise~KL.cat:ratio, mode = "satterthwaite",
#                    lmerTest.limit = 240000
#                    # , at = list(ratio = c("close"))
# )
# # Estimated marginal means for each emotion condition
# summary(mLMEPop, infer = c(TRUE, TRUE))$emmeans 
# # Estimated marginal means for condition difference pairwise comparison
# summary(mLMEPop, infer = c(TRUE, TRUE))$contrasts
# 
# margMeansLMEPop <- summary(mLMEPop)$emmeans


#### plots-----
# data_sample$ratio <- factor(data_sample$ratio, levels = c("close", "med", "far"))
data_plot <- data_sample %>%
  group_by(KL.cat, ratio, subj_num) %>%
  summarise(ind.mean_erp.p2p = mean(erp.p2p, na.rm=T),
            ind.mean_erp.n2 = mean(erp.n2, na.rm=T)) %>%
  group_by(KL.cat, ratio) %>%
  summarise(mean_erp.p2p = mean(ind.mean_erp.p2p, na.rm=T),
            sd_erp.p2p = sd(ind.mean_erp.p2p, na.rm=T),
            n_erp.p2p = n(),
            mean_erp.n2 = mean(ind.mean_erp.n2, na.rm=T),
            sd_erp.n2 = sd(ind.mean_erp.n2, na.rm=T),
            n_erp.n2 = n()) 

ggplot(data_plot, aes(x=ratio, y=mean_erp.p2p, color = KL.cat, group=KL.cat)) +
  geom_point(aes(x=ratio, y=mean_erp.p2p, color = KL.cat, group = KL.cat), size=1.5, position=position_dodge(.9)) +
  geom_line(aes(x=ratio, y=mean_erp.p2p, color = KL.cat, group = KL.cat), linewidth = 0.8, position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=mean_erp.p2p-(sd_erp.p2p/sqrt(n_erp.p2p)),
                    ymax=mean_erp.p2p+(sd_erp.p2p/sqrt(n_erp.p2p))),
                width = .2, linewidth=.8, position=position_dodge(.9)) 
  # geom_hline(yintercept=12.4807) # intercept at close

### Distance -----
model.p2p.distance.full <- lmerTest::lmer(erp.p2p ~ distance * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                       data = data_sample, REML = T)

# break down KL levels and test linear effect of distance
model.p2p.distance.SS <- lmerTest::lmer(erp.p2p ~ distance + time_point + (time_point|subj_num), # correlated slope & intercept
                                     data = subset(data_sample, KL.cat == "SS"), REML = T)
model.p2p.distance.CP <- lmerTest::lmer(erp.p2p ~ distance + time_point + (time_point|subj_num), # correlated slope & intercept
                                     data = subset(data_sample, KL.cat == "CP"), REML = T)
#### test model assumption -----
# plot(resid(model.p2p.full), data_sample$erp.p2p) # Linearity (visual inspection)
# qqmath(model.p2p.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.p2p.full) ~ data_sample$distance) # Homoscedasticity
# 
## ### read model output -----
# anova(model.p2p.distance,model.p2p.full)
# anova(model.p2p.KL,model.p2p.full)
# anova(model.p2p.reduced,model.p2p.full)
anova(model.p2p.distance.full)
anova(model.p2p.distance.SS)
anova(model.p2p.distance.CP)
summary(model.p2p.distance.full)
summary(model.p2p.distance.SS)
summary(model.p2p.distance.CP)


# #### emmeans -----
# mLMEPop <- emmeans(model.p2p.distance.full, pairwise~KL.cat:distance, mode = "satterthwaite",
#                    lmerTest.limit = 240000
#                    # , at = list(ratio = c("close"))
# )
# # Estimated marginal means for each emotion condition
# summary(mLMEPop, infer = c(TRUE, TRUE))$emmeans 
# # Estimated marginal means for condition difference pairwise comparison
# summary(mLMEPop, infer = c(TRUE, TRUE))$contrasts
# 
# margMeansLMEPop <- summary(mLMEPop)$emmeans

#### plots-----

data_plot <- data_sample %>%
  group_by(KL.cat, distance, subj_num) %>%
  summarise(ind.mean_erp.p2p = mean(erp.p2p, na.rm=T),
            ind.mean_erp.n2 = mean(erp.n2, na.rm=T)) %>%
  group_by(KL.cat, distance) %>%
  summarise(mean_erp.p2p = mean(ind.mean_erp.p2p, na.rm=T),
            sd_erp.p2p = sd(ind.mean_erp.p2p, na.rm=T),
            n_erp.p2p = n(),
            mean_erp.n2 = mean(ind.mean_erp.n2, na.rm=T),
            sd_erp.n2 = sd(ind.mean_erp.n2, na.rm=T),
            n_erp.n2 = n()) 

ggplot(data_plot, aes(x=distance, y=mean_erp.p2p, color = KL.cat, group=KL.cat)) +
  # geom_point(aes(shape=time_point), position=position_dodge(.9)) +

  # geom_line(aes(linetype=time_point), position=position_dodge(.9)) +
  geom_point(aes(x=distance, y=mean_erp.p2p, color = KL.cat, group = KL.cat), size=1.5) +
  geom_line(aes(x=distance, y=mean_erp.p2p, color = KL.cat, group = KL.cat), linewidth = 0.8) +
  geom_errorbar(aes(ymin=mean_erp.p2p-(sd_erp.p2p/sqrt(n_erp.p2p)),
                    ymax=mean_erp.p2p+(sd_erp.p2p/sqrt(n_erp.p2p))),
                width = .2, size=0.8) 



  

## N2 -----

### Ratio -----
model.n2.ratio.full <- lmerTest::lmer(erp.n2 ~ ratio * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                       data = data_sample, REML = T)

# break down KL levels and test linear effect of ratio
model.n2.ratio.SS <- lmerTest::lmer(erp.n2 ~ ratio + time_point + (time_point|subj_num), # correlated slope & intercept
                                     data = subset(data_sample, KL.cat == "SS"), REML = T)
model.n2.ratio.CP <- lmerTest::lmer(erp.n2 ~ ratio + time_point + (time_point|subj_num), # correlated slope & intercept
                                     data = subset(data_sample, KL.cat == "CP"), REML = T)
#### test model assumption -----
# plot(resid(model.n2.full), data_sample$erp.n2) # Linearity (visual inspection)
# qqmath(model.n2.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.n2.full) ~ data_sample$ratio) # Homoscedasticity
# 
## ### read model output -----
# anova(model.n2.ratio,model.n2.full)
# anova(model.n2.KL,model.n2.full)
# anova(model.n2.reduced,model.n2.full)
anova(model.n2.ratio.full)
anova(model.n2.ratio.SS)
anova(model.n2.ratio.CP)
summary(model.n2.ratio.full)
summary(model.n2.ratio.SS)
summary(model.n2.ratio.CP)

# #### emmeans -----
# mLMEPop <- emmeans(model.n2.full, pairwise~KL.cat:ratio, mode = "satterthwaite",
#                    lmerTest.limit = 240000
#                    # , at = list(ratio = c("close"))
# )
# # Estimated marginal means for each emotion condition
# summary(mLMEPop, infer = c(TRUE, TRUE))$emmeans 
# # Estimated marginal means for condition difference pairwise comparison
# summary(mLMEPop, infer = c(TRUE, TRUE))$contrasts
# 
# margMeansLMEPop <- summary(mLMEPop)$emmeans


#### plots-----
# data_sample$ratio <- factor(data_sample$ratio, levels = c("close", "med", "far"))
data_plot <-  data_sample %>%
  group_by(KL.cat, ratio, subj_num) %>%
  summarise(ind.mean_erp.n2 = mean(erp.n2, na.rm=T),
            ind.mean_erp.n2 = mean(erp.n2, na.rm=T)) %>%
  group_by(KL.cat, ratio) %>%
  summarise(mean_erp.n2 = mean(ind.mean_erp.n2, na.rm=T),
            sd_erp.n2 = sd(ind.mean_erp.n2, na.rm=T),
            n_erp.n2 = n(),
            mean_erp.n2 = mean(ind.mean_erp.n2, na.rm=T),
            sd_erp.n2 = sd(ind.mean_erp.n2, na.rm=T),
            n_erp.n2 = n()) 

ggplot(data_plot, aes(x=ratio, y=mean_erp.n2, color = KL.cat, group=KL.cat)) +
  # geom_point(aes(shape=time_point), position=position_dodge(.9)) +

  # geom_line(aes(linetype=time_point), position=position_dodge(.9)) +
  geom_point(aes(x=ratio, y=mean_erp.n2, color = KL.cat, group = KL.cat), size=3, position=position_dodge(.9)) +
  geom_line(aes(x=ratio, y=mean_erp.n2, color = KL.cat, group = KL.cat), linewidth = 0.8, position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=mean_erp.n2-(sd_erp.n2/sqrt(n_erp.n2)),
                    ymax=mean_erp.n2+(sd_erp.n2/sqrt(n_erp.n2))),
                width = .2, size=0.8, position=position_dodge(.9)) 

### Distance -----
model.n2.distance.full <- lmerTest::lmer(erp.n2 ~ distance * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                      data = data_sample, REML = T)


# break down KL levels and test linear effect of distance
model.n2.distance.SS <- lmerTest::lmer(erp.n2 ~ distance + time_point + (time_point|subj_num), # correlated slope & intercept
                                    data = subset(data_sample, KL.cat == "SS"), REML = T)
model.n2.distance.CP <- lmerTest::lmer(erp.n2 ~ distance + time_point + (time_point|subj_num), # correlated slope & intercept
                                    data = subset(data_sample, KL.cat == "CP"), REML = T)
#### test model assumption -----
# plot(resid(model.n2.full), data_sample$erp.n2) # Linearity (visual inspection)
# qqmath(model.n2.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.n2.full) ~ data_sample$distance) # Homoscedasticity
# 
## ### read model output -----
# anova(model.n2.distance,model.n2.full)
# anova(model.n2.KL,model.n2.full)
# anova(model.n2.reduced,model.n2.full)
anova(model.n2.distance.full)
anova(model.n2.distance.SS)
anova(model.n2.distance.CP)
summary(model.n2.distance.full)
summary(model.n2.distance.SS)
summary(model.n2.distance.CP)

#### emmeans -----
mLMEPop <- emmeans(model.n2.full, pairwise~KL.cat:distance, mode = "satterthwaite",
                   lmerTest.limit = 240000
                   # , at = list(distance = c("close"))
)
# Estimated marginal means for each emotion condition
summary(mLMEPop, infer = c(TRUE, TRUE))$emmeans 
# Estimated marginal means for condition difference pairwise comparison
summary(mLMEPop, infer = c(TRUE, TRUE))$contrasts

margMeansLMEPop <- summary(mLMEPop)$emmeans


#### plots-----
data_sample$distance <- factor(data_sample$distance, levels = c("1", "2"))
data_sample$ratio <- factor(data_sample$ratio, levels = c("close","med", "far"))

data_plot <- data_sample %>%
  group_by(KL.cat, distance, subj_num) %>%
  summarise(ind.mean_erp.n2 = mean(erp.n2, na.rm=T),
            ind.mean_erp.n2 = mean(erp.n2, na.rm=T),
            ratio = ratio) %>%
  group_by(KL.cat, distance) %>%
  summarise(ratio=ratio,
            mean_erp.n2 = mean(ind.mean_erp.n2, na.rm=T),
            sd_erp.n2 = sd(ind.mean_erp.n2, na.rm=T),
            n_erp.n2 = n(),
            mean_erp.n2 = mean(ind.mean_erp.n2, na.rm=T),
            sd_erp.n2 = sd(ind.mean_erp.n2, na.rm=T),
            n_erp.n2 = n()) 

ggplot(data_plot, aes(x=distance, y=mean_erp.n2, color = KL.cat, group=KL.cat)) +
  # geom_point(aes(shape=time_point), position=position_dodge(.9)) +

  # geom_line(aes(linetype=time_point), position=position_dodge(.9)) +
  geom_point(aes(x=distance, y=mean_erp.n2, color = KL.cat, group = KL.cat), size=3, position=position_dodge(.9)) +
  geom_line(aes(x=distance, y=mean_erp.n2, color = KL.cat, group = KL.cat), linewidth = 0.8, position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=mean_erp.n2-(sd_erp.n2/sqrt(n_erp.n2)),
                    ymax=mean_erp.n2+(sd_erp.n2/sqrt(n_erp.n2))),
                width = .2, size=0.8, position=position_dodge(.9)) 

ggplot(data_plot, aes(x=ratio, y=mean_erp.n2, color = KL.cat, group=interaction(time_point, KL.cat))) +
  geom_point(aes(shape=time_point), position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=mean_erp.n2-(sd_erp.n2/sqrt(n_erp.n2)),
                    ymax=mean_erp.n2+(sd_erp.n2/sqrt(n_erp.n2))),
                width = .2, size=.65, position=position_dodge(.9)) +
  geom_line(aes(linetype=time_point), position=position_dodge(.9)) +
  geom_point(data = data_plot_no_timepoint, aes(x=ratio, y=mean_erp.n2, color = KL.cat, group = KL.cat), size=3) +
  geom_line(data = data_plot_no_timepoint, aes(x=ratio, y=mean_erp.n2, color = KL.cat, group = KL.cat), linewidth = 1.5) 



## N1 -----

### Cardinal -----
model.n1.cardinal.full <- lmerTest::lmer(erp.n1 ~ cardinal * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                      data = data_N1_P2a, REML = T)

# break down KL levels and test linear effect of cardinal
model.n1.cardinal.SS <- lmerTest::lmer(erp.n1 ~ cardinal + time_point + (time_point|subj_num), # correlated slope & intercept
                                    data = subset(data_N1_P2a, KL.cat == "SS"), REML = T)
model.n1.cardinal.CP <- lmerTest::lmer(erp.n1 ~ cardinal + time_point + (time_point|subj_num), # correlated slope & intercept
                                    data = subset(data_N1_P2a, KL.cat == "CP"), REML = T)
#### test model assumption -----
# plot(resid(model.n1.full), data_N1_P2a$erp.n1) # Linearity (visual inspection)
# qqmath(model.n1.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.n1.full) ~ data_N1_P2a$cardinal) # Homoscedasticity
# 
## ### read model output -----
# anova(model.n1.cardinal,model.n1.full)
# anova(model.n1.KL,model.n1.full)
# anova(model.n1.reduced,model.n1.full)
anova(model.n1.cardinal.full)
anova(model.n1.cardinal.SS)
anova(model.n1.cardinal.CP)
summary(model.n1.cardinal.full)
summary(model.n1.cardinal.SS)
summary(model.n1.cardinal.CP)

# #### emmeans -----
# mLMEPop <- emmeans(model.n1.full, pairwise~KL.cat:cardinal, mode = "satterthwaite",
#                    lmerTest.limit = 240000
#                    # , at = list(cardinal = c("close"))
# )
# # Estimated marginal means for each emotion condition
# summary(mLMEPop, infer = c(TRUE, TRUE))$emmeans 
# # Estimated marginal means for condition difference pairwise comparison
# summary(mLMEPop, infer = c(TRUE, TRUE))$contrasts
# 
# margMeansLMEPop <- summary(mLMEPop)$emmeans


#### plots-----
# data_N1_P2a$cardinal <- factor(data_N1_P2a$cardinal, levels = c("close", "med", "far"))
data_plot <-  data_N1_P2a %>%
  group_by(KL.cat, cardinal, subj_num) %>%
  summarise(ind.mean_erp.n1 = mean(erp.n1, na.rm=T),
            ind.mean_erp.n1 = mean(erp.n1, na.rm=T)) %>%
  group_by(KL.cat, cardinal) %>%
  summarise(mean_erp.n1 = mean(ind.mean_erp.n1, na.rm=T),
            sd_erp.n1 = sd(ind.mean_erp.n1, na.rm=T),
            n_erp.n1 = n(),
            mean_erp.n1 = mean(ind.mean_erp.n1, na.rm=T),
            sd_erp.n1 = sd(ind.mean_erp.n1, na.rm=T),
            n_erp.n1 = n()) 

ggplot(data_plot, aes(x=cardinal, y=mean_erp.n1, color = KL.cat, group=KL.cat)) +
  # geom_point(aes(shape=time_point), position=position_dodge(.9)) +
  
  # geom_line(aes(linetype=time_point), position=position_dodge(.9)) +
  geom_point(aes(x=cardinal, y=mean_erp.n1, color = KL.cat, group = KL.cat), size=3, position=position_dodge(.9)) +
  geom_line(aes(x=cardinal, y=mean_erp.n1, color = KL.cat, group = KL.cat), linewidth = 0.8, position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=mean_erp.n1-(sd_erp.n1/sqrt(n_erp.n1)),
                    ymax=mean_erp.n1+(sd_erp.n1/sqrt(n_erp.n1))),
                width = .2, size=0.8, position=position_dodge(.9)) 
