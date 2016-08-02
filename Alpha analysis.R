# Data import packages
library(xlsx)
# Data manipulation packages
library(outliers)
library(dplyr)
library(tidyr)
library(reshape2)
# Plotting packages
library(lattice)
library(ggplot2)
library(gridExtra)
library(GGally)
# Analysis packages
library(polycor)
library(nlme)
library(gmodels)
library(lme4)
library(ggm)
library(pastecs)
library(car)
library(effsize)
library(multcomp)
library(lsmeans)
# Psychophysics packages
library(quickpsy)
library(MPDiR)
library(psyphy)
# Data report packages
library(knitr)

#-------------------------------------------------------------------------------------
dataset <- questionnaire.ERPs
# dataset <- rep_data_long3
# dataset <- rep_data_alpha3

# 0. Save graphical default settings
defaults <- par()

# 1. ERPs x alpha group
# set contrasts
contrasts(rep_data_long3$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long3$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long3$alpha.group) <- c(-1, 1) # setting contrasts for alpha.group

# 1.1. C1
# 1.1.1. ANOVA
alpha.C1.baseline <- lme(C1 ~ 1, 
                         random = ~1|Subject/configuration, 
                         data = rep_data_long3, 
                         method = "ML") #baseline
alpha.C1.config <- update(alpha.C1.baseline, .~. + configuration)
alpha.C1.alpha.group <- update(alpha.C1.config, .~. + alpha.group)
alpha.C1.lme <- update(alpha.C1.alpha.group, .~. + 
                         configuration:alpha.group)
anova(alpha.C1.baseline, alpha.C1.config, alpha.C1.alpha.group, 
      alpha.C1.lme)

# 1.1.2. Line plot
alpha.C1.line <- ggplot(rep_data_long3, 
                        aes(x = alpha.group, y = C1, 
                            colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "C1 x alpha group", x = "alpha group", y = "C1 amplitude", 
       colour = "configuration")
alpha.C1.line

# 1.2. P1
# 1.2.1. ANOVA
alpha.P1.baseline <- lme(P1 ~ 1, 
                         random = ~1|Subject/configuration, 
                         data = rep_data_long3, 
                         method = "ML") #baseline
alpha.P1.config <- update(alpha.P1.baseline, .~. + configuration)
alpha.P1.alpha.group <- update(alpha.P1.config, .~. + alpha.group)
alpha.P1.lme <- update(alpha.P1.alpha.group, .~. + 
                         configuration:alpha.group)
anova(alpha.P1.baseline, alpha.P1.config, alpha.P1.alpha.group, 
      alpha.P1.lme)

# 1.2.2. Line plot
alpha.P1.line <- ggplot(rep_data_long3, 
                        aes(x = alpha.group, y = P1, 
                            colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "P1 x alpha group", x = "alpha group", y = "P1 amplitude", 
       colour = "configuration")
alpha.P1.line

# 1.3. N1
# 1.3.1. ANOVA
alpha.N1.baseline <- lme(N1 ~ 1, 
                          random = ~1|Subject/configuration, 
                          data = rep_data_long3, 
                          method = "ML") #baseline
alpha.N1.config <- update(alpha.N1.baseline, .~. + configuration)
alpha.N1.alpha.group <- update(alpha.N1.config, .~. + alpha.group)
alpha.N1.lme <- update(alpha.N1.alpha.group, .~. + 
                          configuration:alpha.group)
anova(alpha.N1.baseline, alpha.N1.config, alpha.N1.alpha.group, 
      alpha.N1.lme)

# 1.3.2. Line plot
alpha.N1.line <- ggplot(rep_data_long3, 
                         aes(x = alpha.group, y = N1, 
                             colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "N1 x alpha group", x = "alpha group", y = "N1 amplitude", 
       colour = "configuration")
alpha.N1.line

# 1.4. Nd1
# 1.4.1. ANOVA
alpha.nd1.baseline <- lme(occ.nd1 ~ 1, 
                              random = ~1|Subject/configuration, 
                              data = rep_data_long3[rep_data_long3$session == 1,], 
                          method = "ML") #baseline
alpha.nd1.config <- update(alpha.nd1.baseline, .~. + configuration)
alpha.nd1.alpha.group <- update(alpha.nd1.config, .~. + alpha.group)
alpha.nd1.lme <- update(alpha.nd1.alpha.group, .~. + 
                              configuration:alpha.group)
anova(alpha.nd1.baseline, alpha.nd1.config, 
      alpha.nd1.alpha.group, 
      alpha.nd1.lme)

# 1.4.2. Line plot
alpha.nd1.line <- ggplot(rep_data_long3[rep_data_long3$session == 1,], 
                             aes(x = alpha.group, y = occ.nd1, 
                                                 colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "nd1 occ x alpha group", x = "alpha group", y = "nd1 occ amplitude", 
       colour = "configuration")
alpha.nd1.line

# 1.5. Nd2 (VAN) left
# 1.5.1. ANOVA
alpha.nd2.left.baseline <- lme(left.nd2 ~ 1, 
                               random = ~1|Subject/configuration, 
                               data = rep_data_long3[rep_data_long3$session == 1,], 
                               method = "ML") #baseline
alpha.nd2.left.config <- update(alpha.nd2.left.baseline, .~. + configuration)
alpha.nd2.left.alpha.group <- update(alpha.nd2.left.config, .~. + alpha.group)
alpha.nd2.left.lme <- update(alpha.nd2.left.alpha.group, .~. + 
                               configuration:alpha.group)
anova(alpha.nd2.left.baseline, alpha.nd2.left.config,
      alpha.nd2.left.alpha.group, 
      alpha.nd2.left.lme)

# 1.5.2. Line plot
alpha.nd2.left.line <- ggplot(rep_data_long3[rep_data_long3$session == 1,], 
                              aes(x = alpha.group, y = left.nd2, 
                                                  colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Nd2 left x alpha group", x = "alpha.group", 
       y = "Nd2 left amplitude", 
       colour = "configuration")
alpha.nd2.left.line

# 1.6. Nd2 (VAN) right
# 1.6.1. ANOVA
alpha.right.nd2.baseline <- lme(right.nd2 ~ 1, 
                               random = ~1|Subject/configuration, 
                               data = rep_data_long3, 
                               method = "ML") #baseline
alpha.right.nd2.config <- update(alpha.right.nd2.baseline, .~. + configuration)
alpha.right.nd2.alpha.group <- update(alpha.right.nd2.config, .~. + alpha.group)
alpha.right.nd2.lme <- update(alpha.right.nd2.alpha.group, .~. + 
                               configuration:alpha.group)
anova(alpha.right.nd2.baseline, alpha.right.nd2.config,
      alpha.right.nd2.alpha.group, 
      alpha.right.nd2.lme)


# 1.6.2. Post-hocs
lsmeans(alpha.right.nd2.lme, pairwise ~ configuration | alpha.group)

# 1.6.3. Line plot
alpha.right.nd2.line <- ggplot(rep_data_long3[rep_data_long3$session == 1,], 
                              aes(x = alpha.group, y = right.nd2, 
                                  colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Nd2 right x alpha group", x = "alpha.group", 
       y = "Nd2 right amplitude", 
       colour = "configuration")
alpha.right.nd2.line

# 1.7. Nd2 (VAN) RL
# 1.7.1. ANOVA
alpha.nd2.RL.baseline <- lme(RL.nd2 ~ 1, 
                                random = ~1|Subject/configuration, 
                                data = rep_data_long3[rep_data_long3$session == 1,], 
                                method = "ML") #baseline
alpha.nd2.RL.config <- update(alpha.nd2.RL.baseline, .~. + configuration)
alpha.nd2.RL.alpha.group <- update(alpha.nd2.RL.config, .~. + alpha.group)
alpha.nd2.RL.lme <- update(alpha.nd2.RL.alpha.group, .~. + 
                                configuration:alpha.group)
anova(alpha.nd2.RL.baseline, alpha.nd2.RL.config,
      alpha.nd2.RL.alpha.group, 
      alpha.nd2.RL.lme)

# 1.7.2. Line plot
alpha.nd2.RL.line <- ggplot(rep_data_long3[rep_data_long3$session == 1,], 
                               aes(x = alpha.group, y = RL.nd2, 
                                   colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Nd2 RL x alpha group", x = "alpha.group", 
       y = "Nd2 RL amplitude", 
       colour = "configuration")
alpha.nd2.RL.line


# 1.10. LP
# 1.10.1. ANOVA
alpha.LP.baseline <- lme(LP ~ 1, 
                             random = ~1|Subject/configuration, 
                             data = rep_data_long3, 
                             method = "ML") #baseline
alpha.LP.config <- update(alpha.LP.baseline, .~. + configuration)
alpha.LP.alpha.group <- update(alpha.LP.config, .~. + alpha.group)
alpha.LP.lme <- update(alpha.LP.alpha.group, .~. + 
                             configuration:alpha.group)
anova(alpha.LP.baseline, alpha.LP.config, alpha.LP.alpha.group, alpha.LP.lme)

# # 1.10.2. Line plot

# 2. Alpha power x awareness group
# 2.1. Line plots
# 2.1.1 New grouping
alpha.line <- ggplot(rep_data_long3, aes(x = group, y = alpha, 
                                         colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  labs(title = "Alpha power - new group", x = "new group", y = "Alpha mean", 
       colour = "configuration")
alpha.line

# 2.1.2. Alpha, original grouping
alpha.line <- ggplot(rep_data_long3, aes(x = group.original, y = alpha, 
                                         colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  labs(title = "Alpha power  - original group", x = "original group", y = "Alpha mean", 
       colour = "configuration")
alpha.line

# 2.1.3. Log alpha, new grouping
log.alpha.line <- ggplot(rep_data_long3, aes(x = group, y = log.alpha, 
                                             colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  labs(title = "Log alpha power - new group", x = "new group", 
       y = "log alpha mean power", 
       colour = "configuration")
log.alpha.line

# 2.1.4. Log alpha, original grouping
log.alpha.line <- ggplot(rep_data_long3, aes(x = group.original, y = log.alpha, 
                                             colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  labs(title = "Log alpha power - original group", x = "original group", 
       y = "log alpha mean power", 
       colour = "configuration")
log.alpha.line

# 2.2. Scatterplots
# 2.2.1. Alpha
par(mfrow = c(2, 2))
plot(dataset$Subject, dataset$alpha.sqr.1, col = 2, pch = 16, 
     main = "Square, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(dataset$Subject, dataset$alpha.sqr.2, col = 3, pch = 16,
     main = "Square, session 2", xlab = 'Subject', ylab = "mean alpha power")
plot(dataset$Subject, dataset$alpha.rand.1, col = 4, pch = 16,
     main = "Random, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(dataset$Subject, dataset$alpha.rand.2, col = 6, pch = 16,
     main = "Random, session 2", xlab = 'Subject', ylab = "mean alpha power")

# 2.2.2. Log alpha
plot(dataset$Subject, dataset$log.alpha.sqr.1, col = 2, pch = 16, 
     main = "Square, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(dataset$Subject, dataset$log.alpha.sqr.2, col = 3, pch = 16,
     main = "Square, session 2", xlab = 'Subject', ylab = "mean alpha power")
plot(dataset$Subject, dataset$log.alpha.rand.1, col = 4, pch = 16,
     main = "Random, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(dataset$Subject, dataset$log.alpha.rand.2, col = 6, pch = 16,
     main = "Random, session 2", xlab = 'Subject', ylab = "mean alpha power")



# 3.4.4.2. Scatterplots
# 3.4.4.2.1. Square, session 1
par(mfrow = c(1, 1))
plot(dataset[dataset$alpha.group == "high.alpha",]$Subject, 
     dataset[dataset$alpha.group == "high.alpha",]$right.sqr.nd2_1, 
     col = 2, pch = 16, main = "Nd2 x alpha, Square, session 1", 
     xlab = 'Subject', ylab = "mean nd2 right amplitude")
points(dataset[dataset$alpha.group == "low.alpha",]$Subject, 
       dataset[dataset$alpha.group == "low.alpha",]$right.sqr.nd2_1, 
       col = "blue", pch = 16)
abline(h=mean(dataset[dataset$alpha.group == "high.alpha",]$right.sqr.nd2_1), 
       col = "red")
abline(h=mean(dataset[dataset$alpha.group == "low.alpha",]$right.sqr.nd2_1), 
       col = "blue")
legend(2, -3, legend = levels(dataset$alpha.group), 
       col = c("red", "blue"), pch = 16)

# 3.4.4.2.2. Square, session 2
plot(dataset[dataset$alpha.group == "high.alpha",]$Subject, 
     dataset[dataset$alpha.group == "high.alpha",]$right.sqr.nd2_2, 
     col = 2, pch = 16, main = "Nd2 x alpha, Square, session 2", 
     xlab = 'Subject', ylab = "mean nd2 right amplitude")
points(dataset[dataset$alpha.group == "low.alpha",]$Subject, 
       dataset[dataset$alpha.group == "low.alpha",]$right.sqr.nd2_2, 
       col = "blue", pch = 16)
abline(h=mean(dataset[dataset$alpha.group == "high.alpha",]$right.sqr.nd2_2), 
       col = "red")
abline(h=mean(dataset[dataset$alpha.group == "low.alpha",]$right.sqr.nd2_2), 
       col = "blue")
legend(2, -3, legend = levels(dataset$alpha.group), 
       col = c("red", "blue"), pch = 16)


# 4. Create Median split groups by condition based on alpha power
# 4.1. Square, session 1
# 4.1.1. Create subject vectors
low.alpha.sqr.1 <- dataset[which(dataset$alpha.sqr.1 < 
                                         median(dataset$alpha.sqr.1)),]$Subject
high.alpha.sqr.1 <- dataset[which(dataset$alpha.sqr.1 > 
                                          median(dataset$alpha.sqr.1)),]$Subject

# 4.1.2. Create factor for median split group
dataset$alpha.sqr1.group <- numeric(nrow(dataset))
dataset[which(dataset$Subject %in% 
                  low.alpha.sqr.1),]$alpha.sqr1.group <- 'low alpha sqr1'
dataset[which(dataset$Subject %in% 
                  high.alpha.sqr.1),]$alpha.sqr1.group <- 'high alpha sqr1'
dataset$alpha.sqr1.group <- factor(dataset$alpha.sqr1.group)


# 4.2. Square, session 2
# 4.2.1. Create subject vectors
low.alpha.sqr2 <- dataset[which(dataset$alpha.sqr.2 < 
                                     median(dataset$alpha.sqr.2)),]$Subject
high.alpha.sqr2 <- dataset[which(dataset$alpha.sqr.2 > 
                                      median(dataset$alpha.sqr.2)),]$Subject

# 4.2.2. Create factor for median split group
dataset$alpha.sqr2.group <- numeric(nrow(dataset))
dataset[which(dataset$Subject %in% 
                  low.alpha.sqr2),]$alpha.sqr2.group <- 'low alpha sqr2'
dataset[which(dataset$Subject %in% 
                  high.alpha.sqr2),]$alpha.sqr2.group <- 'high alpha sqr2'
dataset$alpha.sqr2.group <- factor(dataset$alpha.sqr2.group)


# 4.3. Random, session 1
# 4.3.1. Create subject vectors
low.alpha.rand1 <- dataset[which(dataset$alpha.rand.1 < 
                                    median(dataset$alpha.rand.1)),]$Subject
high.alpha.rand1 <- dataset[which(dataset$alpha.rand.1 > 
                                     median(dataset$alpha.rand.1)),]$Subject

# 4.3.2. Create factor for median split group
dataset$alpha.rand1.group <- numeric(nrow(dataset))
dataset[which(dataset$Subject %in% 
                  low.alpha.rand1),]$alpha.rand1.group <- 'low alpha rand1'
dataset[which(dataset$Subject %in% 
                  high.alpha.rand1),]$alpha.rand1.group <- 'high alpha rand1'
dataset$alpha.rand1.group <- factor(dataset$alpha.rand1.group)


# 4.4. Random, session 2
# 4.4.1. Create subject vectors
low.alpha.rand2 <- dataset[which(dataset$alpha.rand.2 < 
                                     median(dataset$alpha.rand.2)),]$Subject
high.alpha.rand2 <- dataset[which(dataset$alpha.rand.2 > 
                                      median(dataset$alpha.rand.2)),]$Subject

# 4.4.2. Create factor for median split group
dataset$alpha.rand2.group <- numeric(nrow(dataset))
dataset[which(dataset$Subject %in% 
                  low.alpha.rand2),]$alpha.rand2.group <- 'low alpha rand2'
dataset[which(dataset$Subject %in% 
                  high.alpha.rand2),]$alpha.rand2.group <- 'high alpha rand2'
dataset$alpha.rand2.group <- factor(dataset$alpha.rand2.group)


# 1. T-tests for alpha mean power - aware x unaware
# 1.1. New grouping
# 1.1.1. Alpha
t.test(dataset[group.original == "aware",]$mean.alpha, 
       dataset[group.original == "unaware",]$mean.alpha)
# 1.1.2. Effect sizes
cohen.d(dataset[group.original == "aware",]$alpha, 
        dataset[group.original == "unaware",]$alpha)
# 1.2.1. Log alpha
t.test(dataset[group == "aware",]$mean.log.alpha, 
       dataset[group == "unaware",]$mean.log.alpha)
# 1.2.2. Effect sizes
cohen.d(dataset[group.original == "aware",]$log.alpha, 
        dataset[group.original == "unaware",]$log.alpha)


# Multiplot
# segmentation by subject
rep_data_long3 %>%
  gather(occ.nd1, left.nd2, right.nd2, RL.nd2, 
         key = "var", value = "value") %>% 
  ggplot(aes(x = alpha, y = value, color = configuration)) +
  geom_point() +
  stat_smooth(method = "lm", aes(color = configuration), se = F) +
  facet_wrap(~ var, scales = "free") +
  theme_bw() +
  labs(title = "ERPs x alpha power", x = "mean alpha power", 
       y = "mean ERP amplitude", 
       colour = "configuration")