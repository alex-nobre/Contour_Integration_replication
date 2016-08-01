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
# Psychophysics packages
library(quickpsy)
library(MPDiR)
library(psyphy)
# Data report packages
library(knitr)

#----------------------------------------------------------------------------------------
# 0. Save graphical default settings
defaults <- par()

# 1. T-tests for alpha mean power - aware x unaware
# 1.1. New grouping
# 1.1.1. Alpha
t.test(rep_data2[group == "aware",]$alpha, 
       rep_data2[group == "unaware",]$alpha)
# 1.1.2. Effect sizes
cohen.d(rep_data_long2[group.original == "aware",]$alpha, 
        rep_data_long2[group.original == "unaware",]$alpha)
# 1.2.1. Log alpha
t.test(rep_data2[group.original == "aware",]$log.alpha, 
       rep_data2[group.original == "unaware",]$log.alpha)
# 1.2.2. Effect sizes
cohen.d(rep_data_long2[group.original == "aware",]$log.alpha, 
        rep_data_long2[group.original == "unaware",]$log.alpha)

# 2. Plots
# 2.1. Line plots
# 2.1.1 Alpha, new grouping
alpha.line <- ggplot(rep_data_long2, aes(x = group, y = alpha, 
                                         colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Alpha mean power", x = "new group", y = "Alpha mean", 
       colour = "configuration")
alpha.line

# 2.1.2. Alpha, original grouping
alpha.line <- ggplot(rep_data_long2, aes(x = group.original, y = alpha, 
                                         colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Alpha mean power", x = "original group", y = "Alpha mean", 
       colour = "configuration")
alpha.line

# 2.1.3. Log alpha, new grouping
log.alpha.line <- ggplot(rep_data_long2, aes(x = group, y = log.alpha, 
                                             colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "log.alpha mean power new group", x = "new group", 
       y = "log.alpha mean", 
       colour = "configuration")
log.alpha.line

# 2.1.4. Log alpha, original grouping
log.alpha.line <- ggplot(rep_data_long2, aes(x = group.original, y = log.alpha, 
                                             colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "log.alpha mean power original group", x = "original group", 
       y = "log.alpha mean", 
       colour = "configuration")
log.alpha.line

# 2.2. Scatterplots
# 2.2.1. Alpha
defaults <- par()
par(mfrow = c(2, 2))
pdf()
plot(rep_data$Subject, rep_data$alpha.sqr.1, col = 2, pch = 16, 
     main = "Square, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(rep_data$Subject, rep_data$alpha.sqr.2, col = 3, pch = 16,
     main = "Square, session 2", xlab = 'Subject', ylab = "mean alpha power")
plot(rep_data$Subject, rep_data$alpha.rand.1, col = 4, pch = 16,
     main = "Random, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(rep_data$Subject, rep_data$alpha.rand.2, col = 6, pch = 16,
     main = "Random, session 2", xlab = 'Subject', ylab = "mean alpha power")

# 2.2.2. Log alpha
plot(rep_data$Subject, rep_data$log.alpha.sqr.1, col = 2, pch = 16, 
     main = "Square, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(rep_data$Subject, rep_data$log.alpha.sqr.2, col = 3, pch = 16,
     main = "Square, session 2", xlab = 'Subject', ylab = "mean alpha power")
plot(rep_data$Subject, rep_data$log.alpha.rand.1, col = 4, pch = 16,
     main = "Random, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(rep_data$Subject, rep_data$log.alpha.rand.2, col = 6, pch = 16,
     main = "Random, session 2", xlab = 'Subject', ylab = "mean alpha power")


# 3. Median split analysis of ERPs x alpha
# 3.1. Build groups based on median-split of alpha power
low.alpha.group <- rep_data[which(rep_data$mean.alpha < 
                                     median(rep_data$mean.alpha)),]$Subject
high.alpha.group <- rep_data[which(rep_data$mean.alpha > 
                                      median(rep_data$mean.alpha)),]$Subject

# 3.2. Create factor for median split group
rep_data$alpha.group <- numeric(nrow(rep_data))
rep_data[which(rep_data$Subject %in% 
                  low.alpha.group),]$alpha.group <- 'low.alpha'
rep_data[which(rep_data$Subject %in% 
                  high.alpha.group),]$alpha.group <- 'high.alpha'
rep_data$alpha.group <- factor(rep_data$alpha.group)

rep_data$group.original <- factor(rep_data$group.original)

# 3.3. Append to long data frame
rep_data_long2$alpha.group <- rep(rep_data$alpha.group, 4)


# 3.4. ANOVA
# 3.4.1. Nd2 (VAN) left
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$alpha.group) <- c(-1, 1) # setting contrasts for alpha.group
alpha_nd2_left_baseline <- lme(left.nd2 ~ 1, random = ~1|Subject/configuration/session, 
                               data = rep_data_long2, method = "ML") #baseline
alpha_nd2_left_config <- update(alpha_nd2_left_baseline, .~. + configuration)
alpha_nd2_left_session <- update(alpha_nd2_left_config, .~. + session)
alpha_nd2_left_alpha.group <- update(alpha_nd2_left_session, .~. + alpha.group)
alpha_nd2_left_config_session <- update(alpha_nd2_left_alpha.group, .~. + 
                                          configuration:session)
alpha_nd2_left_session_alpha.group <- update(alpha_nd2_left_config_session, .~. + 
                                               session:alpha.group)
alpha_nd2_left_config_alpha.group <- update(alpha_nd2_left_session_alpha.group, .~. + 
                                              configuration:alpha.group)
alpha_nd2_left_lme <- update(alpha_nd2_left_config_alpha.group, .~. + 
                               configuration:session:alpha.group)
anova(alpha_nd2_left_baseline, alpha_nd2_left_config, alpha_nd2_left_session, 
      alpha_nd2_left_alpha.group, 
      alpha_nd2_left_config_session, alpha_nd2_left_session_alpha.group, 
      alpha_nd2_left_config_alpha.group, 
      alpha_nd2_left_lme)

# 3.4.2. Nd2 left
alpha.nd2.left.line <- ggplot(rep_data_long2, aes(x = alpha.group, y = left.nd2, 
                                                  colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Nd2 left x alpha group", x = "alpha.group", y = "Nd2 left amplitude", 
       colour = "configuration")


# 3.4.3. Nd2 (VAN) right
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$alpha.group) <- c(-1, 1) # setting contrasts for alpha.group
alpha_nd2_right_baseline <- lme(right.nd2 ~ 1, random = ~1|Subject/configuration/session, 
                                data = rep_data_long2, method = "ML") #baseline
alpha_nd2_right_config <- update(alpha_nd2_right_baseline, .~. + configuration)
alpha_nd2_right_session <- update(alpha_nd2_right_config, .~. + session)
alpha_nd2_right_alpha.group <- update(alpha_nd2_right_session, .~. + alpha.group)
alpha_nd2_right_config_session <- update(alpha_nd2_right_alpha.group, .~. + 
                                           configuration:session)
alpha_nd2_right_session_alpha.group <- update(alpha_nd2_right_config_session, .~. + 
                                                session:alpha.group)
alpha_nd2_right_config_alpha.group <- update(alpha_nd2_right_session_alpha.group, .~. + 
                                               configuration:alpha.group)
alpha_nd2_right_lme <- update(alpha_nd2_right_config_alpha.group, .~. + 
                                configuration:session:alpha.group)
anova(alpha_nd2_right_baseline, alpha_nd2_right_config, alpha_nd2_right_session, 
      alpha_nd2_right_alpha.group, 
      alpha_nd2_right_config_session, alpha_nd2_right_session_alpha.group, 
      alpha_nd2_right_config_alpha.group, 
      alpha_nd2_right_lme)


# 3.4.4. Nd2 right (VAN)
alpha.nd2.right.line <- ggplot(rep_data_long2, aes(x = alpha.group, y = right.nd2, 
                                                   colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Nd2 right x alpha group", x = "alpha group", y = "Nd2 right amplitude", 
       colour = "configuration")

# 3.4.4.2. Scatterplots
# 3.4.4.2.1. Square, session 1
par(mfrow = c(1, 1))
plot(rep_data[rep_data$alpha.group == "high.alpha",]$Subject, 
     rep_data[rep_data$alpha.group == "high.alpha",]$right.sqr.nd2_1, 
     col = 2, pch = 16, main = "Nd2 x alpha, Square, session 1", 
     xlab = 'Subject', ylab = "mean nd2 right amplitude")
points(rep_data[rep_data$alpha.group == "low.alpha",]$Subject, 
       rep_data[rep_data$alpha.group == "low.alpha",]$right.sqr.nd2_1, 
       col = "blue", pch = 16)
abline(h=mean(rep_data[rep_data$alpha.group == "high.alpha",]$right.sqr.nd2_1), 
       col = "red")
abline(h=mean(rep_data[rep_data$alpha.group == "low.alpha",]$right.sqr.nd2_1), 
       col = "blue")
legend(2, -3, legend = levels(rep_data$alpha.group), 
       col = c("red", "blue"), pch = 16)

# 3.4.4.2.2. Square, session 2
plot(rep_data[rep_data$alpha.group == "high.alpha",]$Subject, 
     rep_data[rep_data$alpha.group == "high.alpha",]$right.sqr.nd2_2, 
     col = 2, pch = 16, main = "Nd2 x alpha, Square, session 2", 
     xlab = 'Subject', ylab = "mean nd2 right amplitude")
points(rep_data[rep_data$alpha.group == "low.alpha",]$Subject, 
       rep_data[rep_data$alpha.group == "low.alpha",]$right.sqr.nd2_2, 
       col = "blue", pch = 16)
abline(h=mean(rep_data[rep_data$alpha.group == "high.alpha",]$right.sqr.nd2_2), 
       col = "red")
abline(h=mean(rep_data[rep_data$alpha.group == "low.alpha",]$right.sqr.nd2_2), 
       col = "blue")
legend(2, -3, legend = levels(rep_data$alpha.group), 
       col = c("red", "blue"), pch = 16)


# 3.4.5. Nd2 (VAN) RL
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$alpha.group) <- c(-1, 1) # setting contrasts for alpha.group
alpha_nd2_RL_baseline <- lme(RL.nd2 ~ 1, random = ~1|Subject/configuration/session, 
                                data = rep_data_long2, method = "ML") #baseline
alpha_nd2_RL_config <- update(alpha_nd2_RL_baseline, .~. + configuration)
alpha_nd2_RL_session <- update(alpha_nd2_RL_config, .~. + session)
alpha_nd2_RL_alpha.group <- update(alpha_nd2_RL_session, .~. + alpha.group)
alpha_nd2_RL_config_session <- update(alpha_nd2_RL_alpha.group, .~. + 
                                           configuration:session)
alpha_nd2_RL_session_alpha.group <- update(alpha_nd2_RL_config_session, .~. + 
                                                session:alpha.group)
alpha_nd2_RL_config_alpha.group <- update(alpha_nd2_RL_session_alpha.group, .~. + 
                                               configuration:alpha.group)
alpha_nd2_RL_lme <- update(alpha_nd2_RL_config_alpha.group, .~. + 
                                configuration:session:alpha.group)
anova(alpha_nd2_RL_baseline, alpha_nd2_RL_config, alpha_nd2_RL_session, 
      alpha_nd2_RL_alpha.group, 
      alpha_nd2_RL_config_session, alpha_nd2_RL_session_alpha.group, 
      alpha_nd2_RL_config_alpha.group, 
      alpha_nd2_RL_lme)

# 3.4.6. Nd2 RL
alpha.nd2.RL.line <- ggplot(rep_data_long2, aes(x = alpha.group, y = RL.nd2, 
                                                colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "Nd2 RL x alpha group", x = "alpha group", y = "Nd2 RL amplitude", 
       colour = "configuration")


# 4. Create Median split groups based on alpha power
# 4.1. Square, session 1
# 4.1.1. Create subject vectors
low.alpha.sqr.1 <- rep_data[which(rep_data$alpha.sqr.1 < 
                                         median(rep_data$alpha.sqr.1)),]$Subject
high.alpha.sqr.1 <- rep_data[which(rep_data$alpha.sqr.1 > 
                                          median(rep_data$alpha.sqr.1)),]$Subject

# 4.1.2. Create factor for median split group
rep_data$alpha.sqr1.group <- numeric(nrow(rep_data))
rep_data[which(rep_data$Subject %in% 
                  low.alpha.sqr.1),]$alpha.sqr1.group <- 'low alpha sqr1'
rep_data[which(rep_data$Subject %in% 
                  high.alpha.sqr.1),]$alpha.sqr1.group <- 'high alpha sqr1'
rep_data$alpha.sqr1.group <- factor(rep_data$alpha.sqr1.group)


# 4.2. Square, session 2
# 4.2.1. Create subject vectors
low.alpha.sqr2 <- rep_data[which(rep_data$alpha.sqr.2 < 
                                     median(rep_data$alpha.sqr.2)),]$Subject
high.alpha.sqr2 <- rep_data[which(rep_data$alpha.sqr.2 > 
                                      median(rep_data$alpha.sqr.2)),]$Subject

# 4.2.2. Create factor for median split group
rep_data$alpha.sqr2.group <- numeric(nrow(rep_data))
rep_data[which(rep_data$Subject %in% 
                  low.alpha.sqr2),]$alpha.sqr2.group <- 'low alpha sqr2'
rep_data[which(rep_data$Subject %in% 
                  high.alpha.sqr2),]$alpha.sqr2.group <- 'high alpha sqr2'
rep_data$alpha.sqr2.group <- factor(rep_data$alpha.sqr2.group)


# 4.3. Random, session 1
# 4.3.1. Create subject vectors
low.alpha.rand1 <- rep_data[which(rep_data$alpha.rand.1 < 
                                    median(rep_data$alpha.rand.1)),]$Subject
high.alpha.rand1 <- rep_data[which(rep_data$alpha.rand.1 > 
                                     median(rep_data$alpha.rand.1)),]$Subject

# 4.3.2. Create factor for median split group
rep_data$alpha.rand1.group <- numeric(nrow(rep_data))
rep_data[which(rep_data$Subject %in% 
                  low.alpha.rand1),]$alpha.rand1.group <- 'low alpha rand1'
rep_data[which(rep_data$Subject %in% 
                  high.alpha.rand1),]$alpha.rand1.group <- 'high alpha rand1'
rep_data$alpha.rand1.group <- factor(rep_data$alpha.rand1.group)


# 4.4. Random, session 2
# 4.4.1. Create subject vectors
low.alpha.rand2 <- rep_data[which(rep_data$alpha.rand.2 < 
                                     median(rep_data$alpha.rand.2)),]$Subject
high.alpha.rand2 <- rep_data[which(rep_data$alpha.rand.2 > 
                                      median(rep_data$alpha.rand.2)),]$Subject

# 4.4.2. Create factor for median split group
rep_data$alpha.rand2.group <- numeric(nrow(rep_data))
rep_data[which(rep_data$Subject %in% 
                  low.alpha.rand2),]$alpha.rand2.group <- 'low alpha rand2'
rep_data[which(rep_data$Subject %in% 
                  high.alpha.rand2),]$alpha.rand2.group <- 'high alpha rand2'
rep_data$alpha.rand2.group <- factor(rep_data$alpha.rand2.group)


# 5. T-Tests
# 5.1. Left
# 5.1.1. Square, session 1
t.test(rep_data[rep_data$alpha.sqr1.group == "low alpha sqr1",]$left.sqr.nd2_1, 
       rep_data[rep_data$alpha.sqr1.group == "high alpha sqr1",]$left.sqr.nd2_1)

# 5.1.2. Square, session 2
t.test(rep_data[rep_data$alpha.sqr2.group == "low alpha sqr2",]$left.sqr.nd2_2, 
       rep_data[rep_data$alpha.sqr2.group == "high alpha sqr2",]$left.sqr.nd2_2)

# 5.1.3. Random, session 1
t.test(rep_data[rep_data$alpha.rand1.group == "low alpha rand1",]$left.rand.nd2_1, 
       rep_data[rep_data$alpha.rand1.group == "high alpha rand1",]$left.rand.nd2_1)

# 5.1.4. Random, session 2
t.test(rep_data[rep_data$alpha.rand2.group == "low alpha rand2",]$left.rand.nd2_2, 
       rep_data[rep_data$alpha.rand2.group == "high alpha rand2",]$left.rand.nd2_2)

# 5.2. Right
# 5.2.1. Square, session 1
t.test(rep_data[rep_data$alpha.sqr1.group == "low alpha sqr1",]$right.sqr.nd2_1, 
       rep_data[rep_data$alpha.sqr1.group == "high alpha sqr1",]$right.sqr.nd2_1)

# 5.2.2. Square, session 2
t.test(rep_data[rep_data$alpha.sqr2.group == "low alpha sqr2",]$right.sqr.nd2_2, 
       rep_data[rep_data$alpha.sqr2.group == "high alpha sqr2",]$right.sqr.nd2_2)

# 5.2.3. Random, session 1
t.test(rep_data[rep_data$alpha.rand1.group == "low alpha rand1",]$right.rand.nd2_1, 
       rep_data[rep_data$alpha.rand1.group == "high alpha rand1",]$right.rand.nd2_1)

# 5.2.4. Random, session 2
t.test(rep_data[rep_data$alpha.rand2.group == "low alpha rand2",]$right.rand.nd2_2, 
       rep_data[rep_data$alpha.rand2.group == "high alpha rand2",]$right.rand.nd2_2)

# 5.3. RL
# 5.3.1. Square, session 1
t.test(rep_data[rep_data$alpha.sqr1.group == "low alpha sqr1",]$RL.sqr.nd2_1, 
       rep_data[rep_data$alpha.sqr1.group == "high alpha sqr1",]$RL.sqr.nd2_1)

# 5.3.2. Square, session 2
t.test(rep_data[rep_data$alpha.sqr2.group == "low alpha sqr2",]$RL.sqr.nd2_2, 
       rep_data[rep_data$alpha.sqr2.group == "high alpha sqr2",]$RL.sqr.nd2_2)

# 5.3.3. Random, session 1
t.test(rep_data[rep_data$alpha.rand1.group == "low alpha rand1",]$RL.rand.nd2_1, 
       rep_data[rep_data$alpha.rand1.group == "high alpha rand1",]$RL.rand.nd2_1)

# 5.3.4. Random, session 2
t.test(rep_data[rep_data$alpha.rand2.group == "low alpha rand2",]$RL.rand.nd2_2, 
       rep_data[rep_data$alpha.rand2.group == "high alpha rand2",]$RL.rand.nd2_2)

# 6. Nd1
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$alpha.group) <- c(-1, 1) # setting contrasts for alpha.group
alpha_nd1_occ_baseline <- lme(occ.nd1 ~ 1, random = ~1|Subject/configuration/session, 
                                data = rep_data_long2, method = "ML") #baseline
alpha_nd1_occ_config <- update(alpha_nd1_occ_baseline, .~. + configuration)
alpha_nd1_occ_session <- update(alpha_nd1_occ_config, .~. + session)
alpha_nd1_occ_alpha.group <- update(alpha_nd1_occ_session, .~. + alpha.group)
alpha_nd1_occ_config_session <- update(alpha_nd1_occ_alpha.group, .~. + 
                                           configuration:session)
alpha_nd1_occ_session_alpha.group <- update(alpha_nd1_occ_config_session, .~. + 
                                                session:alpha.group)
alpha_nd1_occ_config_alpha.group <- update(alpha_nd1_occ_session_alpha.group, .~. + 
                                               configuration:alpha.group)
alpha_nd1_occ_lme <- update(alpha_nd1_occ_config_alpha.group, .~. + 
                                configuration:session:alpha.group)
anova(alpha_nd1_occ_baseline, alpha_nd1_occ_config, alpha_nd1_occ_session, 
      alpha_nd1_occ_alpha.group, 
      alpha_nd1_occ_config_session, alpha_nd1_occ_session_alpha.group, 
      alpha_nd1_occ_config_alpha.group, 
      alpha_nd1_occ_lme)

# line
alpha.nd1.occ.line <- ggplot(rep_data_long2, aes(x = alpha.group, y = occ.nd1, 
                                                colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  facet_grid(.~session) +
  labs(title = "nd1 occ x alpha group", x = "alpha group", y = "nd1 occ amplitude", 
       colour = "configuration")

# P1
contrasts(rep_data_long2$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(rep_data_long2$session) <- c(-1, 1) # setting contrasts for session
contrasts(rep_data_long2$alpha.group) <- c(-1, 1) # setting contrasts for alpha.group
alpha_nd1_P1_baseline <- lme(P1 ~ 1, random = ~1|Subject/configuration/session, 
                              data = rep_data_long2, method = "ML") #baseline
alpha_nd1_P1_config <- update(alpha_nd1_P1_baseline, .~. + configuration)
alpha_nd1_P1_session <- update(alpha_nd1_P1_config, .~. + session)
alpha_nd1_P1_alpha.group <- update(alpha_nd1_P1_session, .~. + alpha.group)
alpha_nd1_P1_config_session <- update(alpha_nd1_P1_alpha.group, .~. + 
                                         configuration:session)
alpha_nd1_P1_session_alpha.group <- update(alpha_nd1_P1_config_session, .~. + 
                                              session:alpha.group)
alpha_nd1_P1_config_alpha.group <- update(alpha_nd1_P1_session_alpha.group, .~. + 
                                             configuration:alpha.group)
alpha_nd1_P1_lme <- update(alpha_nd1_P1_config_alpha.group, .~. + 
                              configuration:session:alpha.group)
anova(alpha_nd1_P1_baseline, alpha_nd1_P1_config, alpha_nd1_P1_session, 
      alpha_nd1_P1_alpha.group, 
      alpha_nd1_P1_config_session, alpha_nd1_P1_session_alpha.group, 
      alpha_nd1_P1_config_alpha.group, 
      alpha_nd1_P1_lme)

# C1
alpha_nd1_C1_baseline <- lme(C1 ~ 1, random = ~1|Subject/configuration/session, 
                             data = rep_data_long2, method = "ML") #baseline
alpha_nd1_C1_config <- update(alpha_nd1_C1_baseline, .~. + configuration)
alpha_nd1_C1_session <- update(alpha_nd1_C1_config, .~. + session)
alpha_nd1_C1_alpha.group <- update(alpha_nd1_C1_session, .~. + alpha.group)
alpha_nd1_C1_config_session <- update(alpha_nd1_C1_alpha.group, .~. + 
                                        configuration:session)
alpha_nd1_C1_session_alpha.group <- update(alpha_nd1_C1_config_session, .~. + 
                                             session:alpha.group)
alpha_nd1_C1_config_alpha.group <- update(alpha_nd1_C1_session_alpha.group, .~. + 
                                            configuration:alpha.group)
alpha_nd1_C1_lme <- update(alpha_nd1_C1_config_alpha.group, .~. + 
                             configuration:session:alpha.group)
anova(alpha_nd1_C1_baseline, alpha_nd1_C1_config, alpha_nd1_C1_session, 
      alpha_nd1_C1_alpha.group, 
      alpha_nd1_C1_config_session, alpha_nd1_C1_session_alpha.group, 
      alpha_nd1_C1_config_alpha.group, 
      alpha_nd1_C1_lme)

# N1
alpha_nd1_N1_baseline <- lme(N1 ~ 1, random = ~1|Subject/configuration/session, 
                             data = rep_data_long2, method = "ML") #baseline
alpha_nd1_N1_config <- update(alpha_nd1_N1_baseline, .~. + configuration)
alpha_nd1_N1_session <- update(alpha_nd1_N1_config, .~. + session)
alpha_nd1_N1_alpha.group <- update(alpha_nd1_N1_session, .~. + alpha.group)
alpha_nd1_N1_config_session <- update(alpha_nd1_N1_alpha.group, .~. + 
                                        configuration:session)
alpha_nd1_N1_session_alpha.group <- update(alpha_nd1_N1_config_session, .~. + 
                                             session:alpha.group)
alpha_nd1_N1_config_alpha.group <- update(alpha_nd1_N1_session_alpha.group, .~. + 
                                            configuration:alpha.group)
alpha_nd1_N1_lme <- update(alpha_nd1_N1_config_alpha.group, .~. + 
                             configuration:session:alpha.group)
anova(alpha_nd1_N1_baseline, alpha_nd1_N1_config, alpha_nd1_N1_session, 
      alpha_nd1_N1_alpha.group, 
      alpha_nd1_N1_config_session, alpha_nd1_N1_session_alpha.group, 
      alpha_nd1_N1_config_alpha.group, 
      alpha_nd1_N1_lme)

# LP
alpha_nd1_LP_baseline <- lme(LP ~ 1, random = ~1|Subject/configuration/session, 
                             data = rep_data_long2, method = "ML") #baseline
alpha_nd1_LP_config <- update(alpha_nd1_LP_baseline, .~. + configuration)
alpha_nd1_LP_session <- update(alpha_nd1_LP_config, .~. + session)
alpha_nd1_LP_alpha.group <- update(alpha_nd1_LP_session, .~. + alpha.group)
alpha_nd1_LP_config_session <- update(alpha_nd1_LP_alpha.group, .~. + 
                                        configuration:session)
alpha_nd1_LP_session_alpha.group <- update(alpha_nd1_LP_config_session, .~. + 
                                             session:alpha.group)
alpha_nd1_LP_config_alpha.group <- update(alpha_nd1_LP_session_alpha.group, .~. + 
                                            configuration:alpha.group)
alpha_nd1_LP_lme <- update(alpha_nd1_LP_config_alpha.group, .~. + 
                             configuration:session:alpha.group)
anova(alpha_nd1_LP_baseline, alpha_nd1_LP_config, alpha_nd1_LP_session, 
      alpha_nd1_LP_alpha.group, 
      alpha_nd1_LP_config_session, alpha_nd1_LP_session_alpha.group, 
      alpha_nd1_LP_config_alpha.group, 
      alpha_nd1_LP_lme)

ses1.data <- subset(rep_data_long2, session == 1)

# anova
contrasts(ses1.data$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ses1.data$alpha.group) <- c(-1, 1) # setting contrasts for alpha.group
alpha_nd2_left_baseline <- lme(left.nd2 ~ 1, random = ~1|Subject/configuration, 
                               data = ses1.data, method = "ML") #baseline
alpha_nd2_left_config <- update(alpha_nd2_left_baseline, .~. + configuration)
alpha_nd2_left_alpha.group <- update(alpha_nd2_left_config, .~. + alpha.group)
alpha_nd2_left_lme <- update(alpha_nd2_left_alpha.group, .~. + 
                                              configuration:alpha.group)
anova(alpha_nd2_left_baseline, alpha_nd2_left_config, 
      alpha_nd2_left_alpha.group,
      alpha_nd2_left_lme)

# 3.4.2. Nd2 left
alpha.nd2.left.ses1 <- ggplot(ses1.data, aes(x = alpha.group, y = left.nd2, 
                                                  colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Nd2 left x alpha group", x = "alpha.group", y = "Nd2 left amplitude", 
       colour = "configuration")

# anova
contrasts(ses1.data$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ses1.data$alpha.group) <- c(-1, 1) # setting contrasts for alpha.group
alpha_nd2_right_baseline <- lme(right.nd2 ~ 1, random = ~1|Subject/configuration, 
                               data = ses1.data, method = "ML") #baseline
alpha_nd2_right_config <- update(alpha_nd2_right_baseline, .~. + configuration)
alpha_nd2_right_alpha.group <- update(alpha_nd2_right_config, .~. + alpha.group)
alpha_nd2_right_lme <- update(alpha_nd2_right_alpha.group, .~. + 
                               configuration:alpha.group)
anova(alpha_nd2_right_baseline, alpha_nd2_right_config, 
      alpha_nd2_right_alpha.group,
      alpha_nd2_right_lme)

# 3.4.2. Nd2 right
alpha.nd2.right.ses1 <- ggplot(ses1.data, aes(x = alpha.group, y = right.nd2, 
                                             colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Nd2 right x alpha group", x = "alpha.group", y = "Nd2 right amplitude", 
       colour = "configuration")

# anova
contrasts(ses1.data$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ses1.data$alpha.group) <- c(-1, 1) # setting contrasts for alpha.group
alpha_nd2_occ_baseline <- lme(RL.nd2 ~ 1, random = ~1|Subject/configuration, 
                                data = ses1.data, method = "ML") #baseline
alpha_nd2_RL_config <- update(alpha_nd2_RL_baseline, .~. + configuration)
alpha_nd2_RL_alpha.group <- update(alpha_nd2_RL_config, .~. + alpha.group)
alpha_nd2_RL_lme <- update(alpha_nd2_RL_alpha.group, .~. + 
                                configuration:alpha.group)
anova(alpha_nd2_RL_baseline, alpha_nd2_RL_config, 
      alpha_nd2_RL_alpha.group,
      alpha_nd2_RL_lme)

# 3.4.2. Nd2 RL
alpha.nd2.RL.ses1 <- ggplot(ses1.data, aes(x = alpha.group, y = RL.nd2, 
                                              colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Nd2 RL x alpha group", x = "alpha.group", y = "Nd2 RL amplitude", 
       colour = "configuration")


# anova
contrasts(ses1.data$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ses1.data$alpha.group) <- c(-1, 1) # setting contrasts for alpha.group
alpha_nd1_occ_baseline <- lme(occ.nd1 ~ 1, random = ~1|Subject/configuration, 
                             data = ses1.data, method = "ML") #baseline
alpha_nd1_occ_config <- update(alpha_nd1_occ_baseline, .~. + configuration)
alpha_nd1_occ_alpha.group <- update(alpha_nd1_occ_config, .~. + alpha.group)
alpha_nd1_occ_lme <- update(alpha_nd1_occ_alpha.group, .~. + 
                             configuration:alpha.group)
anova(alpha_nd1_occ_baseline, alpha_nd1_occ_config, 
      alpha_nd1_occ_alpha.group,
      alpha_nd1_occ_lme)

# anova
contrasts(ses1.data$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ses1.data$alpha.group) <- c(-1, 1) # setting contrasts for alpha.group
alpha_P1_baseline <- lme(P1 ~ 1, random = ~1|Subject/configuration, 
                             data = ses1.data, method = "ML") #baseline
alpha_P1_config <- update(alpha_P1_baseline, .~. + configuration)
alpha_P1_alpha.group <- update(alpha_P1_config, .~. + alpha.group)
alpha_P1_lme <- update(alpha_P1_alpha.group, .~. + 
                             configuration:alpha.group)
anova(alpha_P1_baseline, alpha_P1_config, 
      alpha_P1_alpha.group,
      alpha_P1_lme)

contrasts(ses1.data$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ses1.data$alpha.group) <- c(-1, 1) # setting contrasts for alpha.group
alpha_C1_baseline <- lme(C1 ~ 1, random = ~1|Subject/configuration, 
                         data = ses1.data, method = "ML") #baseline
alpha_C1_config <- update(alpha_C1_baseline, .~. + configuration)
alpha_C1_alpha.group <- update(alpha_C1_config, .~. + alpha.group)
alpha_C1_lme <- update(alpha_C1_alpha.group, .~. + 
                         configuration:alpha.group)
anova(alpha_C1_baseline, alpha_C1_config, 
      alpha_C1_alpha.group,
      alpha_C1_lme)

contrasts(ses1.data$configuration) <- c(-1, 1) # setting contrasts for config
contrasts(ses1.data$alpha.group) <- c(-1, 1) # setting contrasts for alpha.group
alpha_N1_baseline <- lme(N1 ~ 1, random = ~1|Subject/configuration, 
                         data = ses1.data, method = "ML") #baseline
alpha_N1_config <- update(alpha_N1_baseline, .~. + configuration)
alpha_N1_alpha.group <- update(alpha_N1_config, .~. + alpha.group)
alpha_N1_lme <- update(alpha_N1_alpha.group, .~. + 
                         configuration:alpha.group)
anova(alpha_N1_baseline, alpha_N1_config, 
      alpha_N1_alpha.group,
      alpha_N1_lme)

t.test(ses1.data)

# 3.4.2. Nd2 RL
alpha.N1.ses1 <- ggplot(ses1.data, aes(x = alpha.group, y = N1, 
                                           colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "N1 x alpha group", x = "alpha.group", y = "N1 amplitude", 
       colour = "configuration")


# 4. Regression
# Linear model
alpha.right.nd2.lm <- lm(right.nd2 ~ log.alpha, 
                    data = rep_data_long3)

summary(alpha.right.nd2.lm)
plot(questionnaire.ERPs$alpha.group)