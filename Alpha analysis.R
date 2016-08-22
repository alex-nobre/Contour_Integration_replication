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
# dataset <- rep.data.long2
# dataset <- rep_data_alpha3

# 0. Save graphical default settings
defaults <- par()

# 1. ERPs x alpha group
# set contrasts
contrasts(rep.data.long2$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep.data.long2$session) <- c(-1, 1) # contrasts for session
contrasts(rep.data.long2$alpha.group) <- c(-1, 1) # contrasts for alpha.group

# 1.1. C1
# 1.1.1. ANOVA
alpha.C1.baseline <- lme(C1 ~ 1, 
                         random = ~1|Subject/configuration, 
                         data = rep.data.long2[rep.data.long2$session == 1,], 
                         method = "ML") #baseline
alpha.C1.config <- update(alpha.C1.baseline, .~. + configuration)
alpha.C1.alpha.group <- update(alpha.C1.config, .~. + alpha.group)
alpha.C1.lme <- update(alpha.C1.alpha.group, .~. + 
                         configuration:alpha.group)
anova(alpha.C1.baseline, alpha.C1.config, alpha.C1.alpha.group, 
      alpha.C1.lme)
# 1.1.2. Line plot
alpha.C1.line <- ggplot(rep.data.long2[rep.data.long2$session == 1,], 
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
                         data = rep.data.long2[rep.data.long2$session == 1,], 
                         method = "ML") #baseline
alpha.P1.config <- update(alpha.P1.baseline, .~. + configuration)
alpha.P1.alpha.group <- update(alpha.P1.config, .~. + alpha.group)
alpha.P1.lme <- update(alpha.P1.alpha.group, .~. + 
                         configuration:alpha.group)
anova(alpha.P1.baseline, alpha.P1.config, alpha.P1.alpha.group, 
      alpha.P1.lme)

# 1.2.2. Line plot
alpha.P1.line <- ggplot(rep.data.long2[rep.data.long2$session == 1,], 
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
                         rep.data.long2[rep.data.long2$session == 1,], 
                          method = "ML") #baseline
alpha.N1.config <- update(alpha.N1.baseline, .~. + configuration)
alpha.N1.alpha.group <- update(alpha.N1.config, .~. + alpha.group)
alpha.N1.lme <- update(alpha.N1.alpha.group, .~. + 
                          configuration:alpha.group)
anova(alpha.N1.baseline, alpha.N1.config, alpha.N1.alpha.group, 
      alpha.N1.lme)

# 1.3.2. Line plot
alpha.N1.line <- ggplot(rep.data.long2[rep.data.long2$session == 1,], 
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
                              data = rep.data.long2[rep.data.long2$session == 1,], 
                          method = "ML") #baseline
alpha.nd1.config <- update(alpha.nd1.baseline, .~. + configuration)
alpha.nd1.alpha.group <- update(alpha.nd1.config, .~. + alpha.group)
alpha.nd1.lme <- update(alpha.nd1.alpha.group, .~. + 
                              configuration:alpha.group)
anova(alpha.nd1.baseline, alpha.nd1.config, 
      alpha.nd1.alpha.group, 
      alpha.nd1.lme)

# 1.4.2. Line plot
alpha.nd1.line <- ggplot(rep.data.long2[rep.data.long2$session == 1,], 
                             aes(x = alpha.group, y = occ.nd1, 
                                                 colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "nd1 occ x alpha group", x = "alpha group", y = "nd1 occ amplitude", 
       colour = "configuration")
alpha.nd1.line

# 1.4.3. Scatterplot
layout(rbind(1,2), heights=c(5,1))
plot(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$occ.diff.1, 
     col = "red", pch = 16, main = "mean alpha 1 x nd1  amplitude",
     xlab = "mean alpha", ylab = "Mean amplitude")
points(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$occ.diff.2,
       col = " black", pch = 16)
abline(lm(questionnaire.ERPs$occ.diff.1 ~ questionnaire.ERPs$mean.log.alpha), 
       col = "red")
abline(lm(questionnaire.ERPs$occ.diff.2 ~ questionnaire.ERPs$mean.log.alpha), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("nd1 diff 1", "nd1 diff 2"), 
       col = c("red", "black"),
       pch = 16)
par(defaults)


# 1.5. Nd2 left
# 1.5.1. ANOVA
alpha.left.nd2.baseline <- lme(left.nd2 ~ 1, 
                               random = ~1|Subject/configuration, 
                               data = rep.data.long2[rep.data.long2$session == 1,], 
                               method = "ML") #baseline
alpha.left.nd2.config <- update(alpha.left.nd2.baseline, .~. + configuration)
alpha.left.nd2.alpha.group <- update(alpha.left.nd2.config, .~. + alpha.group)
alpha.left.nd2.lme <- update(alpha.left.nd2.alpha.group, .~. + 
                               configuration:alpha.group)
anova(alpha.left.nd2.baseline, alpha.left.nd2.config,
      alpha.left.nd2.alpha.group, 
      alpha.left.nd2.lme)

# 1.5.2. Post-hocs
lsmeans(alpha.left.nd2.lme, pairwise ~ configuration | alpha.group)

plot(lsmeans(alpha.left.nd2.lme, pairwise ~ configuration | alpha.group)$contrasts, 
     main = "CI nd2 left: configuration x alpha group")

# 1.5.3. Line plot
alpha.nd2.left.line <- ggplot(rep.data.long2[rep.data.long2$session == 1,], 
                              aes(x = alpha.group, y = left.nd2, 
                                                  colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Nd2 left x alpha group", x = "alpha.group", 
       y = "Nd2 left amplitude", 
       colour = "configuration")
alpha.nd2.left.line

# 1.5.4. Scatterplot - log mean alpha x left nd2 diff
layout(rbind(1,2), heights=c(5,1))
plot(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$left.diff.1, 
     col = "red", pch = 16, main = "mean alpha 1 x left Nd2 amplitude",
     xlab = "mean alpha", ylab = "Mean amplitude")
points(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$left.diff.2,
       col = " black", pch = 16)
abline(lm(questionnaire.ERPs$left.diff.1 ~ questionnaire.ERPs$mean.log.alpha), 
       col = "red")
abline(lm(questionnaire.ERPs$left.diff.2 ~ questionnaire.ERPs$mean.log.alpha), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("left diff 1", "left diff 2"), 
       col = c("red", "black"),
       pch = 16)
par(defaults)

# 1.6. Nd2 left
# 1.6.1. ANOVA
alpha.right.nd2.baseline <- lme(right.nd2 ~ 1, 
                               random = ~1|Subject/configuration, 
                               data = rep.data.long2[rep.data.long2$session == 1,], 
                               method = "ML") #baseline
alpha.right.nd2.config <- update(alpha.right.nd2.baseline, .~. + configuration)
alpha.right.nd2.alpha.group <- update(alpha.right.nd2.config, .~. + alpha.group)
alpha.right.nd2.lme <- update(alpha.right.nd2.alpha.group, .~. + 
                               configuration:alpha.group)
anova(alpha.right.nd2.baseline, alpha.right.nd2.config,
      alpha.right.nd2.alpha.group, 
      alpha.right.nd2.lme)

summary(alpha.right.nd2.lme)


# 1.6.2. Post-hocs
lsmeans(alpha.right.nd2.lme, pairwise ~ configuration | alpha.group)

plot(lsmeans(alpha.right.nd2.lme, pairwise ~ configuration | alpha.group)$contrasts, 
     main = "CI nd2 right: configuration x alpha group")

# 1.6.3. Line plot
alpha.right.nd2.line <- ggplot(rep.data.long2[rep.data.long2$session == 1,], 
                              aes(x = alpha.group, y = right.nd2, 
                                  colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Nd2 right x alpha group", x = "alpha.group", 
       y = "Nd2 right amplitude", 
       colour = "configuration")
alpha.right.nd2.line

# 1.6.4. Correlations scatterplot
layout(rbind(1,2), heights=c(5,1))
plot(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$right.diff.1, 
     col = "red", pch = 16, main = "mean alpha 1 x right Nd2 amplitude",
     xlab = "mean alpha", ylab = "Mean amplitude")
points(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$right.diff.2,
       col = " black", pch = 16)
abline(lm(questionnaire.ERPs$right.diff.1 ~ questionnaire.ERPs$mean.log.alpha), 
       col = "red")
abline(lm(questionnaire.ERPs$right.diff.2 ~ questionnaire.ERPs$mean.log.alpha), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("right diff 1", "right diff 2"), 
       col = c("red", "black"),
       pch = 16)
par(defaults)


# 1.7. Nd2 RL
# 1.7.1. ANOVA
alpha.nd2.RL.baseline <- lme(RL.nd2 ~ 1, 
                                random = ~1|Subject/configuration, 
                                data = rep.data.long2[rep.data.long2$session == 1,], 
                                method = "ML") #baseline
alpha.nd2.RL.config <- update(alpha.nd2.RL.baseline, .~. + configuration)
alpha.nd2.RL.alpha.group <- update(alpha.nd2.RL.config, .~. + alpha.group)
alpha.nd2.RL.lme <- update(alpha.nd2.RL.alpha.group, .~. + 
                                configuration:alpha.group)
anova(alpha.nd2.RL.baseline, alpha.nd2.RL.config,
      alpha.nd2.RL.alpha.group, 
      alpha.nd2.RL.lme)

# 1.7.2. Line plot
alpha.nd2.RL.line <- ggplot(rep.data.long2[rep.data.long2$session == 1,], 
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
                             data = rep.data.long2[rep.data.long2$session == 1,], 
                             method = "ML") #baseline
alpha.LP.config <- update(alpha.LP.baseline, .~. + configuration)
alpha.LP.alpha.group <- update(alpha.LP.config, .~. + alpha.group)
alpha.LP.lme <- update(alpha.LP.alpha.group, .~. + 
                             configuration:alpha.group)
anova(alpha.LP.baseline, alpha.LP.config, alpha.LP.alpha.group, alpha.LP.lme)

# 1.10.2. Line plot
alpha.nd2.LP.line <- ggplot(rep.data.long2[rep.data.long2$session == 1,], 
                            aes(x = alpha.group, y = LP, 
                                colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Nd2 LP x alpha group", x = "alpha.group", 
       y = "LP amplitude", 
       colour = "configuration")
alpha.nd2.LP.line

# 2. Alpha power x awareness group
# 2.1. Line plots
# 2.1.1. Log alpha, new grouping
log.alpha.line <- ggplot(rep.data.long2, aes(x = group, y = log.alpha, 
                                             colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  labs(title = "Log alpha power - new group", x = "new group", 
       y = "log alpha mean power", 
       colour = "configuration")
log.alpha.line

# 2.1.2. Log alpha, original grouping
log.alpha.line <- ggplot(rep.data.long2, aes(x = group.original, y = log.alpha, 
                                             colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  labs(title = "Log alpha power - original group", x = "original group", 
       y = "log alpha mean power", 
       colour = "configuration")
log.alpha.line

# 2.2. Mean alpha scatterplots
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
par(defaults)

# 2.2.2. Log alpha
par(mfrow = c(2, 2))
plot(dataset$Subject, dataset$log.alpha.sqr.1, col = 2, pch = 16, 
     main = "Square, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(dataset$Subject, dataset$log.alpha.sqr.2, col = 3, pch = 16,
     main = "Square, session 2", xlab = 'Subject', ylab = "mean alpha power")
plot(dataset$Subject, dataset$log.alpha.rand.1, col = 4, pch = 16,
     main = "Random, session 1", xlab = 'Subject', ylab = "mean alpha power")
plot(dataset$Subject, dataset$log.alpha.rand.2, col = 6, pch = 16,
     main = "Random, session 2", xlab = 'Subject', ylab = "mean alpha power")
par(defaults)


# 2.3. Scatterplots - Right nd2 x alpha group
# 2.3.1. Square, session 1
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

# 2.3.2. Square, session 2
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

# 2.4. Correlations scatterplots - log alpha x ERPs
# 2.4.1. Occ nd1 differences
layout(rbind(1,2), heights=c(5,1))
plot(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$occ.diff.1, 
     col = "red", pch = 16, main = "mean alpha 1 x occ Nd2 different",
     xlab = "mean alpha", ylab = "Mean amplitude")
points(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$occ.diff.2,
       col = " black", pch = 16)
abline(lm(questionnaire.ERPs$occ.diff.1 ~ questionnaire.ERPs$mean.log.alpha), 
       col = "red")
abline(lm(questionnaire.ERPs$occ.diff.2 ~ questionnaire.ERPs$mean.log.alpha), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("occ diff 1", "occ diff 2"), 
       col = c("red", "black"),
       pch = 16)
par(defaults)
# 2.4.2. Left Nd2 difference
layout(rbind(1,2), heights=c(5,1))
plot(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$left.diff.1, 
     col = "red", pch = 16, main = "mean alpha 1 x left Nd2 different",
     xlab = "mean alpha", ylab = "Mean amplitude")
points(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$left.diff.2,
       col = " black", pch = 16)
abline(lm(questionnaire.ERPs$left.diff.1 ~ questionnaire.ERPs$mean.log.alpha), 
       col = "red")
abline(lm(questionnaire.ERPs$left.diff.2 ~ questionnaire.ERPs$mean.log.alpha), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("left diff 1", "left diff 2"), 
       col = c("red", "black"),
       pch = 16)
par(defaults)

cor.test(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$left.diff.1)
cor.test(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$left.diff.2)

# 2.4.3. Right Nd2 difference
layout(rbind(1,2), heights=c(5,1))
plot(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$right.diff.1, 
     col = "red", pch = 16, main = "mean alpha 1 x right Nd2 different",
     xlab = "mean alpha", ylab = "Mean amplitude")
points(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$right.diff.2,
       col = " black", pch = 16)
abline(lm(questionnaire.ERPs$right.diff.1 ~ questionnaire.ERPs$mean.log.alpha), 
       col = "red")
abline(lm(questionnaire.ERPs$right.diff.2 ~ questionnaire.ERPs$mean.log.alpha), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("right diff 1", "right diff 2"), 
       col = c("red", "black"),
       pch = 16)
par(defaults)

cor.test(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$right.diff.1)
cor.test(x = questionnaire.ERPs$mean.log.alpha, y = questionnaire.ERPs$right.diff.2)

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
# 1.1.1. Log alpha
t.test(dataset[group.original == "aware",]$mean.log.alpha, 
       dataset[group.original == "unaware",]$mean.log.alpha)
# 1.1.2. Effect sizes
cohen.d(dataset[group.original == "aware",]$mean.log.alpha, 
        dataset[group.original == "unaware",]$mean.log.alpha)


# Multiplot
# segmentation by subject
rep.data.long2 %>%
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

# Correlations
cor.test(log(questionnaire.ERPs$mean.alpha.1), questionnaire.ERPs$occ.sqr.nd1_1)

plot(log(questionnaire.ERPs$mean.alpha.1), questionnaire.ERPs$occ.sqr.nd1_1)

# behavioral data
# Plot block end intensities values by awareness group
plot(x = questionnaire.ERPs$Subject, y = questionnaire.ERPs$ses1.Ph, 
     col = questionnaire.ERPs$alpha.group, pch = 16, main = "Main task thresholds",
     xlab = "Subject", ylab = "Threshold")
legend(0, -0.2, legend = levels(questionnaire.ERPs$alpha.group), 
       col = c("black", "red"),
       pch = 16)
abline(h = mean(questionnaire.ERPs[questionnaire.ERPs$alpha.group == 'high.alpha', ]$ses1.Ph))
abline(h = mean(questionnaire.ERPs[questionnaire.ERPs$alpha.group == 'low.alpha', ]$ses1.Ph), 
       col = 'red')
