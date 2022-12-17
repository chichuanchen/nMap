# Set up -----
library("tidyverse")
library("readxl")
library("lme4")
library("lmerTest")
library("lattice") # used for visual inspection of normal distribution of residuals
library("car") # leveneTest
library("emmeans") # extract estimated marginal means

rm(list = ls())
load("../data/ERP/tidied/erp_sample_tidied.RData")

# Define catogeorical KL
data_sample <- data_sample %>%
  mutate(KL.cat = if_else(KL.cont %in% c(1:4), "SS", "CP"))

# Specify data type for subsequent analysis -----
data_sample$subj_num <- as.factor(data_sample$subj_num)
data_sample$time_point <- as.factor(data_sample$time_point)
# data_sample$ratio <- factor(data_sample$ratio, levels = c("close", "med", "far")) 
data_sample$KL.cat <- factor(data_sample$KL.cat, levels = c("SS", "CP")) # for easier interpretation
data_sample$distance <- factor(data_sample$distance, levels = c("2", "1"))
#
data_sample$ratio <- factor(data_sample$ratio, levels = c("med", "far", "close")) 
contrasts(data_sample$ratio) <- contr.Sum(levels(data_sample$ratio)) 
contrasts(data_sample$distance) <- contr.Sum(levels(data_sample$distance)) 

# FIT LME MODEL -----

## P2p -----

### Ratio -----
# model.p2p.ratio <- lmerTest::lmer(erp.p2p ~ ratio * time_point + (1|subj_num/time_point),
#                             data = data_sample, REML = T)
# model.p2p.KL <- lmerTest::lmer(erp.p2p ~ KL.cat * time_point + (1|subj_num/time_point),
#                             data = data_sample, REML = T)
model.p2p.ratio.full <- lmerTest::lmer(erp.p2p ~ ratio * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                               data = data_sample, REML = T)
model.p2p.ratio.reduced <- lmerTest::lmer(erp.p2p ~ ratio + KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                 data = data_sample, REML = T)

# break down KL levels and test linear effect of ratio
model.p2p.ratio.SS <- lmerTest::lmer(erp.p2p ~ ratio * time_point + (time_point|subj_num), # correlated slope & intercept
               data = subset(data_sample, KL.cat == "SS"), REML = T)
model.p2p.ratio.CP <- lmerTest::lmer(erp.p2p ~ ratio * time_point + (time_point|subj_num), # correlated slope & intercept
                               data = subset(data_sample, KL.cat == "CP"), REML = T)
#### test model assumption -----
# plot(resid(model.p2p.full), data_sample$erp.p2p) # Linearity (visual inspection)
# qqmath(model.p2p.full) # Normal distribution of residuals (visual inspection for sample N > 5000)
# leveneTest(residuals(model.p2p.full) ~ data_sample$ratio) # Homoscedasticity
# 
## ### read model output -----
# anova(model.p2p.ratio,model.p2p.full)
# anova(model.p2p.KL,model.p2p.full)
# anova(model.p2p.reduced,model.p2p.full)
anova(model.p2p.ratio.full)
anova(model.p2p.ratio.SS)
anova(model.p2p.ratio.CP)
summary(model.p2p.ratio.full)
summary(model.p2p.ratio.SS)
summary(model.p2p.ratio.CP)

#### emmeans -----
mLMEPop <- emmeans(model.p2p.full, pairwise~KL.cat:ratio, mode = "satterthwaite",
                   lmerTest.limit = 240000
                   # , at = list(ratio = c("close"))
)
# Estimated marginal means for each emotion condition
summary(mLMEPop, infer = c(TRUE, TRUE))$emmeans 
# Estimated marginal means for condition difference pairwise comparison
summary(mLMEPop, infer = c(TRUE, TRUE))$contrasts

margMeansLMEPop <- summary(mLMEPop)$emmeans


#### plots-----
data_sample$ratio <- factor(data_sample$ratio, levels = c("close", "med", "far"))
data_plot <- data_sample %>%
  group_by(KL.cat, ratio, subj_num, time_point) %>%
  summarise(ind.mean_erp.p2p = mean(erp.p2p, na.rm=T),
            ind.mean_erp.n2 = mean(erp.n2, na.rm=T)) %>%
  group_by(KL.cat, ratio, time_point) %>%
  summarise(mean_erp.p2p = mean(ind.mean_erp.p2p, na.rm=T),
            sd_erp.p2p = sd(ind.mean_erp.p2p, na.rm=T),
            n_erp.p2p = n(),
            mean_erp.n2 = mean(ind.mean_erp.n2, na.rm=T),
            sd_erp.n2 = sd(ind.mean_erp.n2, na.rm=T),
            n_erp.n2 = n()) 

data_plot_no_timepoint <- data_sample %>%
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

ggplot(data_plot, aes(x=ratio, y=mean_erp.p2p, color = KL.cat, group=interaction(time_point, KL.cat))) +
  geom_point(aes(shape=time_point), position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=mean_erp.p2p-(sd_erp.p2p/sqrt(n_erp.p2p)),
                    ymax=mean_erp.p2p+(sd_erp.p2p/sqrt(n_erp.p2p))),
                width = .2, size=.65, position=position_dodge(.9)) +
  geom_line(aes(linetype=time_point), position=position_dodge(.9)) +
  geom_point(data = data_plot_no_timepoint, aes(x=ratio, y=mean_erp.p2p, color = KL.cat, group = KL.cat), size=3) +
  geom_line(data = data_plot_no_timepoint, aes(x=ratio, y=mean_erp.p2p, color = KL.cat, group = KL.cat), linewidth = 1.5) +
  # scale_x_discrete(limits=rev) +
  geom_hline(yintercept=12.4807) # intercept at close

### Distance -----
# model.p2p.distance <- lmerTest::lmer(erp.p2p ~ distance * time_point + (1|subj_num/time_point),
#                             data = data_sample, REML = T)
# model.p2p.KL <- lmerTest::lmer(erp.p2p ~ KL.cat * time_point + (1|subj_num/time_point),
#                             data = data_sample, REML = T)
model.p2p.distance.full <- lmerTest::lmer(erp.p2p ~ distance * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                       data = data_sample, REML = T)
model.p2p.distance.reduced <- lmerTest::lmer(erp.p2p ~ distance + KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
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


#### emmeans -----
mLMEPop <- emmeans(model.p2p.distance.full, pairwise~KL.cat:distance, mode = "satterthwaite",
                   lmerTest.limit = 240000
                   # , at = list(ratio = c("close"))
)
# Estimated marginal means for each emotion condition
summary(mLMEPop, infer = c(TRUE, TRUE))$emmeans 
# Estimated marginal means for condition difference pairwise comparison
summary(mLMEPop, infer = c(TRUE, TRUE))$contrasts

margMeansLMEPop <- summary(mLMEPop)$emmeans

#### plots-----

data_plot <- data_sample %>%
  group_by(KL.cat, distance, subj_num, time_point) %>%
  summarise(ind.mean_erp.p2p = mean(erp.p2p, na.rm=T),
            ind.mean_erp.n2 = mean(erp.n2, na.rm=T)) %>%
  group_by(KL.cat, distance, time_point) %>%
  summarise(mean_erp.p2p = mean(ind.mean_erp.p2p, na.rm=T),
            sd_erp.p2p = sd(ind.mean_erp.p2p, na.rm=T),
            n_erp.p2p = n(),
            mean_erp.n2 = mean(ind.mean_erp.n2, na.rm=T),
            sd_erp.n2 = sd(ind.mean_erp.n2, na.rm=T),
            n_erp.n2 = n()) 

data_plot_no_timepoint <- data_sample %>%
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

ggplot(data_plot, aes(x=distance, y=mean_erp.p2p, color = KL.cat, group=interaction(time_point, KL.cat))) +
  geom_point(aes(shape=time_point), position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=mean_erp.p2p-(sd_erp.p2p/sqrt(n_erp.p2p)),
                    ymax=mean_erp.p2p+(sd_erp.p2p/sqrt(n_erp.p2p))),
                width = .2, size=.65, position=position_dodge(.9)) +
  geom_line(aes(linetype=time_point), position=position_dodge(.9)) +
  geom_point(data = data_plot_no_timepoint, aes(x=distance, y=mean_erp.p2p, color = KL.cat, group = KL.cat), size=3) +
  geom_line(data = data_plot_no_timepoint, aes(x=distance, y=mean_erp.p2p, color = KL.cat, group = KL.cat), linewidth = 1.5) 



  

## N2 -----

### Ratio -----
# model.n2.ratio <- lmerTest::lmer(erp.n2 ~ ratio * time_point + (1|subj_num/time_point),
#                             data = data_sample, REML = T)
# model.n2.KL <- lmerTest::lmer(erp.n2 ~ KL.cat * time_point + (1|subj_num/time_point),
#                             data = data_sample, REML = T)
model.n2.ratio.full <- lmerTest::lmer(erp.n2 ~ ratio * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                       data = data_sample, REML = T)
model.n2.ratio.reduced <- lmerTest::lmer(erp.n2 ~ ratio + KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                          data = data_sample, REML = T)

# break down KL levels and test linear effect of ratio
model.n2.ratio.SS <- lmerTest::lmer(erp.n2 ~ ratio * time_point + (time_point|subj_num), # correlated slope & intercept
                                     data = subset(data_sample, KL.cat == "SS"), REML = T)
model.n2.ratio.CP <- lmerTest::lmer(erp.n2 ~ ratio * time_point + (time_point|subj_num), # correlated slope & intercept
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

#### emmeans -----
mLMEPop <- emmeans(model.n2.full, pairwise~KL.cat:ratio, mode = "satterthwaite",
                   lmerTest.limit = 240000
                   # , at = list(ratio = c("close"))
)
# Estimated marginal means for each emotion condition
summary(mLMEPop, infer = c(TRUE, TRUE))$emmeans 
# Estimated marginal means for condition difference pairwise comparison
summary(mLMEPop, infer = c(TRUE, TRUE))$contrasts

margMeansLMEPop <- summary(mLMEPop)$emmeans


#### plots-----
data_sample$ratio <- factor(data_sample$ratio, levels = c("close", "med", "far"))
data_plot <- data_sample %>%
  group_by(KL.cat, ratio, subj_num, time_point) %>%
  summarise(ind.mean_erp.n2 = mean(erp.n2, na.rm=T),
            ind.mean_erp.n2 = mean(erp.n2, na.rm=T)) %>%
  group_by(KL.cat, ratio, time_point) %>%
  summarise(mean_erp.n2 = mean(ind.mean_erp.n2, na.rm=T),
            sd_erp.n2 = sd(ind.mean_erp.n2, na.rm=T),
            n_erp.n2 = n(),
            mean_erp.n2 = mean(ind.mean_erp.n2, na.rm=T),
            sd_erp.n2 = sd(ind.mean_erp.n2, na.rm=T),
            n_erp.n2 = n()) 

data_plot_no_timepoint <- data_sample %>%
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

ggplot(data_plot, aes(x=ratio, y=mean_erp.n2, color = KL.cat, group=interaction(time_point, KL.cat))) +
  geom_point(aes(shape=time_point), position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=mean_erp.n2-(sd_erp.n2/sqrt(n_erp.n2)),
                    ymax=mean_erp.n2+(sd_erp.n2/sqrt(n_erp.n2))),
                width = .2, size=.65, position=position_dodge(.9)) +
  geom_line(aes(linetype=time_point), position=position_dodge(.9)) +
  geom_point(data = data_plot_no_timepoint, aes(x=ratio, y=mean_erp.n2, color = KL.cat, group = KL.cat), size=3) +
  geom_line(data = data_plot_no_timepoint, aes(x=ratio, y=mean_erp.n2, color = KL.cat, group = KL.cat), linewidth = 1.5) 

### Distance -----
# model.n2.ratio <- lmerTest::lmer(erp.n2 ~ ratio * time_point + (1|subj_num/time_point),
#                             data = data_sample, REML = T)
# model.n2.KL <- lmerTest::lmer(erp.n2 ~ KL.cat * time_point + (1|subj_num/time_point),
#                             data = data_sample, REML = T)
model.n2.distance.full <- lmerTest::lmer(erp.n2 ~ distance * KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                      data = data_sample, REML = T)
model.n2.distance.reduced <- lmerTest::lmer(erp.n2 ~ distance + KL.cat + time_point + (time_point|subj_num), # correlated slope & intercept
                                         data = data_sample, REML = T)

# break down KL levels and test linear effect of distance
model.n2.distance.SS <- lmerTest::lmer(erp.n2 ~ distance * time_point + (time_point|subj_num), # correlated slope & intercept
                                    data = subset(data_sample, KL.cat == "SS"), REML = T)
model.n2.distance.CP <- lmerTest::lmer(erp.n2 ~ distance * time_point + (time_point|subj_num), # correlated slope & intercept
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
  group_by(KL.cat, distance, subj_num, time_point) %>%
  summarise(ind.mean_erp.n2 = mean(erp.n2, na.rm=T),
            ind.mean_erp.n2 = mean(erp.n2, na.rm=T),
            ratio = ratio) %>%
  group_by(KL.cat, distance, time_point) %>%
  summarise(ratio = ratio,
            mean_erp.n2 = mean(ind.mean_erp.n2, na.rm=T),
            sd_erp.n2 = sd(ind.mean_erp.n2, na.rm=T),
            n_erp.n2 = n(),
            mean_erp.n2 = mean(ind.mean_erp.n2, na.rm=T),
            sd_erp.n2 = sd(ind.mean_erp.n2, na.rm=T),
            n_erp.n2 = n()) 

data_plot_no_timepoint <- data_sample %>%
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

ggplot(data_plot, aes(x=distance, y=mean_erp.n2, color = KL.cat, group=interaction(time_point, KL.cat))) +
  geom_point(aes(shape=time_point), position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=mean_erp.n2-(sd_erp.n2/sqrt(n_erp.n2)),
                    ymax=mean_erp.n2+(sd_erp.n2/sqrt(n_erp.n2))),
                width = .2, size=.65, position=position_dodge(.9)) +
  geom_line(aes(linetype=time_point), position=position_dodge(.9)) +
  geom_point(data = data_plot_no_timepoint, aes(x=distance, y=mean_erp.n2, color = KL.cat, group = KL.cat), size=3) +
  geom_line(data = data_plot_no_timepoint, aes(x=distance, y=mean_erp.n2, color = KL.cat, group = KL.cat), linewidth = 1.5) 

ggplot(data_plot, aes(x=ratio, y=mean_erp.n2, color = KL.cat, group=interaction(time_point, KL.cat))) +
  geom_point(aes(shape=time_point), position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=mean_erp.n2-(sd_erp.n2/sqrt(n_erp.n2)),
                    ymax=mean_erp.n2+(sd_erp.n2/sqrt(n_erp.n2))),
                width = .2, size=.65, position=position_dodge(.9)) +
  geom_line(aes(linetype=time_point), position=position_dodge(.9)) +
  geom_point(data = data_plot_no_timepoint, aes(x=ratio, y=mean_erp.n2, color = KL.cat, group = KL.cat), size=3) +
  geom_line(data = data_plot_no_timepoint, aes(x=ratio, y=mean_erp.n2, color = KL.cat, group = KL.cat), linewidth = 1.5) 
