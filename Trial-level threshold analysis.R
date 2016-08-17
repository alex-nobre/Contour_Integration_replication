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
library(psyphy)
# Data report packages
library(knitr)

defaults <- par()

#--------------------------------------------ERPS-----------------------------------------
# 2.1. Set contrasts
contrasts(rep.data.bin3$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep.data.bin3$intensity.bin) <- c(-1, 1) # contrasts for decrement bin
contrasts(rep.data.bin3$session) <- c(-1, 1) # contrasts for session

# 2.2. C1
# 2.2.1. ANOVA
bin.C1.baseline <- lme(C1 ~ 1, random = ~1|Subject/configuration/intensity.bin, 
                         data = rep.data.bin3[rep.data.bin3$session == 1,], 
                         method = "ML") #baseline
bin.C1.config <- update(bin.C1.baseline, .~. + configuration)
bin.C1.intensity.bin <- update(bin.C1.config, .~. + intensity.bin)
bin.C1.lme <- update(bin.C1.intensity.bin, .~. + configuration:intensity.bin)
anova(bin.C1.baseline, bin.C1.config, bin.C1.intensity.bin,
      bin.C1.lme)

# 2.2.2. Line plot
bin.C1.line <- ggplot(rep.data.bin3[rep.data.bin3$session == 1,], 
                        aes(x = intensity.bin, y = C1, 
                            colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "C1 mean amplitude", x = "bin power", y = "C1 mean amplitude", 
       colour = "configuration")
bin.C1.line

# 2.3. P1
# 2.3.1. ANOVA
bin.P1.baseline <- lme(P1 ~ 1, random = ~1|Subject/configuration/intensity.bin, 
                         data = rep.data.bin3[rep.data.bin3$session == 1,], 
                         method = "ML") #baseline
bin.P1.config <- update(bin.P1.baseline, .~. + configuration)
bin.P1.intensity.bin <- update(bin.P1.config, .~. + intensity.bin)
bin.P1.lme <- update(bin.P1.intensity.bin, .~. + configuration:intensity.bin)
anova(bin.P1.baseline, bin.P1.config, bin.P1.intensity.bin,
      bin.P1.lme)

# 2.3.2. Line plot
bin.P1.line <- ggplot(rep.data.bin3[rep.data.bin3$session == 1,], 
                        aes(x = intensity.bin, y = P1, 
                            colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "P1 mean amplitude", x = "bin power", y = "P1 mean amplitude", 
       colour = "configuration")
bin.P1.line

# 2.4. N1
# 2.4.1. ANOVA
bin.N1.baseline <- lme(N1 ~ 1, random = ~1|Subject/configuration/intensity.bin, 
                         data = rep.data.bin3[rep.data.bin3$session == 1,], 
                         method = "ML") #baseline
bin.N1.config <- update(bin.N1.baseline, .~. + configuration)
bin.N1.intensity.bin <- update(bin.N1.config, .~. + intensity.bin)
bin.N1.lme <- update(bin.N1.intensity.bin, .~. + configuration:intensity.bin)
anova(bin.N1.baseline, bin.N1.config, bin.N1.intensity.bin,
      bin.N1.lme)

# 2.4.2. Line plot
bin.N1.line <- ggplot(rep.data.bin3[rep.data.bin3$session == 1,], 
                        aes(x = intensity.bin, y = N1, 
                            colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "N1 mean amplitude", x = "bin power", y = "N1 mean amplitude", 
       colour = "configuration")
bin.N1.line

# 2.5. Nd1
# 2.5.1. Scatterplot
# 2.5.1.1. Low bin
plot(x = rep_data6$Subject, y = rep_data6$low.bin.occ.nd1.rand_1, 
     col = "red", pch = 16, main = "Nd1 in low decrement bin",
     xlab = "Subject", ylab = "Mean amplitude")
points(x = rep_data6$Subject, y = rep_data6$low.bin.occ.nd1.sqr_1,
       col = " black", pch = 16)
legend(0, -0.2, legend = levels(rep.data.bin3$configuration), 
       col = c("black", "red"),
       pch = 16)
abline(h = mean(rep_data6$low.bin.occ.nd1.rand_1), col = "red")
abline(h = mean(rep_data6$low.bin.occ.nd1.sqr_1), 
       col = 'black')

# 2.5.1.2. High bin
plot(x = rep_data6$Subject, y = rep_data6$high.bin.occ.nd1.rand_1, 
     col = "red", pch = 16, main = "Nd1 in high decrement bin",
     xlab = "Subject", ylab = "Mean amplitude")
points(x = rep_data6$Subject, y = rep_data6$high.bin.occ.nd1.sqr_1,
       col = " black", pch = 16)
legend(0, -0.2, legend = levels(rep.data.bin3$configuration), 
       col = c("black", "red"),
       pch = 16)
abline(h = mean(rep_data6$high.bin.occ.nd1.rand_1), col = "red")
abline(h = mean(rep_data6$high.bin.occ.nd1.sqr_1), 
       col = 'black')

# 2.5.2. ANOVA
bin.nd1.baseline <- lme(occ.nd1 ~ 1, random = ~1|Subject/configuration/intensity.bin, 
                          data = rep.data.bin3[rep.data.bin3$session == 1,], 
                          method = "ML") #baseline
bin.nd1.config <- update(bin.nd1.baseline, .~. + configuration)
bin.nd1.intensity.bin <- update(bin.nd1.config, .~. + intensity.bin)
bin.nd1.lme <- update(bin.nd1.intensity.bin, .~. + configuration:intensity.bin)
anova(bin.nd1.baseline, bin.nd1.config, bin.nd1.intensity.bin,
      bin.nd1.lme)

# 2.5.1. Post-hocs
bin.nd1.lme <- lme(occ.nd1 ~ configuration * intensity.bin * group.original, 
                     random = ~1|Subject/configuration/intensity.bin, 
                     data = rep.data.bin3[rep.data.bin3$session == 1,], 
                     method = "ML")
anova(bin.nd1.lme)
summary(bin.nd1.lme)

# 2.5.2. using lsmeans
lsmeans(bin.nd1.lme, pairwise ~ configuration | intensity.bin)


# ANOVA TESTING
bin.nd1.model <- lme(occ.nd1 ~ configuration * intensity.bin, 
                       random = ~1|Subject/configuration/intensity.bin, 
                       data = rep.data.bin3, method = "ML")
anova(bin.nd1.model)

tanova <- aov(occ.nd1 ~ (configuration * intensity.bin) +
                Error(Subject/(configuration * intensity.bin)), 
              data = rep.data.bin3)

summary(bin.nd1.baseline)

# Plots
bin.nd1.line <- ggplot(rep.data.bin3[rep.data.bin3$session == 1,], 
                         aes(x = intensity.bin, y = occ.nd1, 
                             colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_grid(.~bin.group)
labs(title = "Nd1 mean amplitude", x = "bin power", y = "Nd1 mean amplitude", 
     colour = "configuration")
bin.nd1.line

plot(lsmeans(bin.nd1.lme, pairwise ~ configuration | bin.group * 
               intensity.bin)$contrasts, 
     main = "Confidence intervals contrasts nd1")

# 2.6. Left nd2
# 2.6.1. Scatteplots
# 2.6.1.1. Low bin
layout(rbind(1,2), heights=c(5,1))
plot(x = rep_data6$Subject, y = rep_data6$low.bin.left.nd2.rand_1, 
     col = "red", pch = 16, main = "Left Nd2 in low decrement bin",
     xlab = "Subject", ylab = "Mean amplitude")
points(x = rep_data6$Subject, y = rep_data6$low.bin.left.nd2.sqr_1,
       col = " black", pch = 16)
abline(h = mean(rep_data6$low.bin.left.nd2.rand_1), col = "red")
abline(h = mean(rep_data6$low.bin.left.nd2.sqr_1), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = levels(rep.data.bin3$configuration), 
       col = c("black", "red"),
       pch = 16)
par(defaults)
# 2.6.1.2. High bin
layout(rbind(1,2), heights=c(5,1))
plot(x = rep_data6$Subject, y = rep_data6$high.bin.left.nd2.rand_1, 
     col = "red", pch = 16, main = "nd2 in high decrement bin",
     xlab = "Subject", ylab = "Mean amplitude")
points(x = rep_data6$Subject, y = rep_data6$high.bin.left.nd2.sqr_1,
       col = " black", pch = 16)
abline(h = mean(rep_data6$high.bin.left.nd2.rand_1), col = "red")
abline(h = mean(rep_data6$high.bin.left.nd2.sqr_1), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = levels(rep.data.bin3$configuration), 
       col = c("black", "red"),
       pch = 16)
par(defaults)

# 2.6.1. ANOVA
bin.left.nd2.baseline <- lme(left.nd2 ~ 1, 
                               random = ~1|Subject/configuration/intensity.bin, 
                               data = rep.data.bin3[rep.data.bin3$session == 1,], 
                               method = "ML") #baseline
bin.left.nd2.config <- update(bin.left.nd2.baseline, .~. + configuration)
bin.left.nd2.intensity.bin <- update(bin.left.nd2.config, .~. + intensity.bin)
bin.left.nd2.lme <- update(bin.left.nd2.intensity.bin, .~. + 
                               configuration:intensity.bin)
anova(bin.left.nd2.baseline, bin.left.nd2.config, bin.left.nd2.intensity.bin,
      bin.left.nd2.lme)

# 2.6.2. Post-hocs
bin.left.nd2.lme<- lme(left.nd2 ~ configuration * intensity.bin * bin.group, 
                         random = ~1|Subject/configuration/intensity.bin, 
                         data = rep.data.bin3[rep.data.bin3$session == 1,], 
                         method = "ML")
anova(bin.left.nd2.lme)

lsmeans(bin.left.nd2.lme, pairwise ~ configuration | intensity.bin)

plot(lsmeans(bin.left.nd2.lme, pairwise ~ configuration | intensity.bin)$lsmeans, 
     main = "Confidence intervals left nd2")

# 2.6.3. Line plot
bin.left.nd2.line <- ggplot(rep.data.bin3[rep.data.bin3$session == 1,], 
                              aes(x = intensity.bin, y = left.nd2, 
                                  colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Left Nd2 mean amplitude", x = "decrement bin", 
       y = "Left Nd2 mean amplitude", 
       colour = "configuration")
bin.left.nd2.line

# 2.7. Right nd2
# 2.7.1. Scatterplots
# 2.7.1.1. Low bin
layout(rbind(1,2), heights=c(5,1))
plot(x = rep_data6$Subject, y = rep_data6$low.bin.right.nd2.rand_1, 
     col = "red", pch = 16, main = "right Nd2 in low decrement bin",
     xlab = "Subject", ylab = "Mean amplitude")
points(x = rep_data6$Subject, y = rep_data6$low.bin.right.nd2.sqr_1,
       col = " black", pch = 16)
abline(h = mean(rep_data6$low.bin.right.nd2.rand_1), col = "red")
abline(h = mean(rep_data6$low.bin.right.nd2.sqr_1), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = levels(rep.data.bin3$configuration), 
       col = c("black", "red"),
       pch = 16)
par(defaults)
# 2.7.1.2. High bin
layout(rbind(1,2), heights=c(5,1))
plot(x = rep_data6$Subject, y = rep_data6$high.bin.right.nd2.rand_1, 
     col = "red", pch = 16, main = "right nd2 in high decrement bin",
     xlab = "Subject", ylab = "Mean amplitude")
points(x = rep_data6$Subject, y = rep_data6$high.bin.right.nd2.sqr_1,
       col = " black", pch = 16)
abline(h = mean(rep_data6$high.bin.right.nd2.rand_1), col = "red")
abline(h = mean(rep_data6$high.bin.right.nd2.sqr_1), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = levels(rep.data.bin3$configuration), 
       col = c("black", "red"),
       pch = 16)
par(defaults)

# 2.7.2. ANOVA
bin.right.nd2.baseline <- lme(right.nd2 ~ 1, 
                                random = ~1|Subject/configuration/intensity.bin, 
                                data = rep.data.bin3[rep.data.bin3$session == 1,], 
                                method = "ML") #baseline
bin.right.nd2.config <- update(bin.right.nd2.baseline, .~. + configuration)
bin.right.nd2.intensity.bin <- update(bin.right.nd2.config, .~. + intensity.bin)
bin.right.nd2.lme <- update(bin.right.nd2.intensity.bin, .~. + 
                                configuration:intensity.bin)
anova(bin.right.nd2.baseline, bin.right.nd2.config, bin.right.nd2.intensity.bin,
      bin.right.nd2.lme)

summary(bin.right.nd2.lme)

# 2.7.3. Line plot
bin.right.nd2.line <- ggplot(rep.data.bin3[rep.data.bin3$session == 1,], 
                             aes(x = intensity.bin, y = right.nd2, 
                                 colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Right Nd2 mean amplitude", x = "bin power",
       y = "Right Nd2 mean amplitude", 
       colour = "configuration")
bin.right.nd2.line

# 2.7.4. alpha power x difference
# right scatterplot
layout(rbind(1,2), heights=c(5,1))
plot(x = rep_data6$mean.log.alpha, y = rep_data6$low.bin.right.nd2.diff.1, 
     col = "red", pch = 16, main = "mean alpha x right Nd2 diff 1",
     xlab = "mean alpha", ylab = "Mean amplitude difference")
points(x = rep_data6$mean.log.alpha, y = rep_data6$high.bin.right.nd2.diff.1,
       col = " black", pch = 16)
abline(lm(rep_data6$low.bin.right.nd2.diff.1 ~ rep_data6$mean.log.alpha), 
       col = "red")
abline(lm(rep_data6$high.bin.right.nd2.diff.1 ~ rep_data6$mean.log.alpha), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("right diff low bin", "right diff high bin"), 
       col = c("red", "black"),
       pch = 16)
par(defaults)

cor.test(rep_data6$mean.log.alpha, rep_data6$low.bin.right.nd2.diff.1)
cor.test(rep_data6$mean.log.alpha, rep_data6$high.bin.right.nd2.diff.1)

# left scatterplot
layout(rbind(1,2), heights=c(5,1))
plot(x = rep_data6$mean.log.alpha, y = rep_data6$low.bin.left.nd2.diff.1, 
     col = "red", pch = 16, main = "mean alpha x left Nd2 diff 1",
     xlab = "mean alpha", ylab = "Mean amplitude difference")
points(x = rep_data6$mean.log.alpha, y = rep_data6$high.bin.left.nd2.diff.1,
       col = " black", pch = 16)
abline(lm(rep_data6$low.bin.left.nd2.diff.1 ~ rep_data6$mean.log.alpha), 
       col = "red")
abline(lm(rep_data6$high.bin.left.nd2.diff.1 ~ rep_data6$mean.log.alpha), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("left diff low bin", "left diff high bin"), 
       col = c("red", "black"),
       pch = 16)
par(defaults)

# nd1 scatterplot
layout(rbind(1,2), heights=c(5,1))
plot(x = rep_data6$mean.log.alpha, y = rep_data6$low.bin.occ.nd1.diff.1, 
     col = "red", pch = 16, main = "mean alpha x nd1 diff 1",
     xlab = "mean alpha", ylab = "Mean amplitude difference")
points(x = rep_data6$mean.log.alpha, y = rep_data6$high.bin.occ.nd1.diff.1,
       col = " black", pch = 16)
abline(lm(rep_data6$low.bin.occ.nd1.diff.1 ~ rep_data6$mean.log.alpha), 
       col = "red")
abline(lm(rep_data6$high.bin.occ.nd1.diff.1 ~ rep_data6$mean.log.alpha), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = c("nd1 diff low bin", "nd1 diff high bin"), 
       col = c("red", "black"),
       pch = 16)
par(defaults)


# 2.8. RL nd2
# 2.8.1. ANOVA
bin.RL.nd2.baseline <- lme(RL.nd2 ~ 1, 
                             random = ~1|Subject/configuration/intensity.bin, 
                             data = rep.data.bin3[rep.data.bin3$session == 1,], 
                             method = "ML") #baseline
bin.RL.nd2.config <- update(bin.RL.nd2.baseline, .~. + configuration)
bin.RL.nd2.intensity.bin <- update(bin.RL.nd2.config, .~. + intensity.bin)
bin.RL.nd2.lme <- update(bin.RL.nd2.intensity.bin, .~. + 
                             configuration:intensity.bin)
anova(bin.RL.nd2.baseline, bin.RL.nd2.config, bin.RL.nd2.intensity.bin,
      bin.RL.nd2.lme)

# 2.8.2. Post-hocs

bin.RL.nd2.lme <- lme(RL.nd2 ~ configuration * intensity.bin * bin.group, 
                        random = ~1|Subject/configuration/intensity.bin, 
                        data = rep.data.bin3[rep.data.bin3$session == 1,], 
                        method = "ML")
anova(bin.RL.nd2.lme)

posthoc.bin.RL.nd2 <- lsmeans(bin.RL.nd2.lme, pairwise ~ 
                                  configuration | intensity.bin)

plot(lsmeans(posthoc.bin.RL.nd2$lsmeans, 
             main = "Confidence intervals RL nd2"))

plot(lsmeans(bin.RL.nd2.lme, pairwise ~ configuration | bin.group * 
               intensity.bin)$contrasts, 
     main = "Confidence intervals contrasts RL nd2")

qqnorm(bin.RL.nd2.lme)
qqline(bin.RL.nd2.lme)

# using multcomp
# 1.
# compute means for all combinations
tmp <- expand.grid(configuration = unique(rep.data.bin3$configuration),
                   intensity.bin = unique(rep.data.bin3$intensity.bin))
x <- model.matrix(~ configuration * intensity.bin, data = tmp)
glht(bin.RL.nd2.lme, linfct = x)

# construct contrast matrix
Tukey <- contrMat(table(rep.data.bin3$configuration), "Tukey")
K1 <- cbind(Tukey, matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)))
rownames(K1) <- paste(levels(rep.data.bin3$intensity.bin)[1], rownames(K1), 
                      sep = ":")
K2 <- cbind(matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), Tukey)
rownames(K2) <- paste(levels(rep.data.bin3$intensity.bin)[2], rownames(K2), 
                      sep = ":")
K <- rbind(K1, K2)
colnames(K) <- c(colnames(Tukey), colnames(Tukey))
#test
summary(glht(bin.RL.nd2.lme, linfct = K %*% x))

# 2.8.3. Plots
bin.RL.nd2.line <- ggplot(rep.data.bin3[rep.data.bin3$session == 1,], 
                            aes(x = intensity.bin, y = RL.nd2, 
                                colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "RL Nd2 mean amplitude", x = "bin power",
       y = "RL Nd2 mean amplitude", 
       colour = "configuration")
bin.RL.nd2.line

# 2.9. N2
# 2.9.1. ANOVA
bin.N2.baseline <- lme(N2 ~ 1, random = ~1|Subject/configuration/intensity.bin, 
                       data = rep.data.bin3[rep.data.bin3$session == 1,],
                       method = "ML") #baseline
bin.N2.config <- update(bin.N2.baseline, .~. + configuration)
bin.N2.intensity.bin <- update(bin.N2.config, .~. + intensity.bin)
bin.N2.lme <- update(bin.N2.intensity.bin, .~. + configuration:intensity.bin)
anova(bin.N2.baseline, bin.N2.config, bin.N2.intensity.bin,
      bin.N2.lme)

# 2.9.2 Line plot
bin.N2.line <- ggplot(rep.data.bin3[rep.data.bin3$session == 1,], 
                      aes(x = intensity.bin, y = N2, 
                          colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "N2 mean amplitude", x = "bin power",
       y = "N2 mean amplitude", 
       colour = "configuration")
bin.N2.line

# 2.10. LP
# 2.10.1. ANOVA
bin.LP.baseline <- lme(LP ~ 1, random = ~1|Subject/configuration/intensity.bin, 
                         data = rep.data.bin3[rep.data.bin3$session == 1,], 
                         method = "ML") #baseline
bin.LP.config <- update(bin.LP.baseline, .~. + configuration)
bin.LP.intensity.bin <- update(bin.LP.config, .~. + intensity.bin)
bin.LP.lme <- update(bin.LP.intensity.bin, .~. + configuration:intensity.bin)
anova(bin.LP.baseline, bin.LP.config, bin.LP.intensity.bin,
      bin.LP.lme)

# 2.10.2 Line plot
bin.LP.line <- ggplot(rep.data.bin3[rep.data.bin3$session == 1,], 
                      aes(x = intensity.bin, y = LP, 
                          colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "LP mean amplitude", x = "bin power",
       y = "LP mean amplitude", 
       colour = "configuration")
bin.LP.line

by(rep.data.bin3$LP, rep.data.bin3$configuration, 
   stat.desc, basic = FALSE)

#--------------------------------------Alpha power----------------------------------------
# 3.
# 2.1. Set contrasts
contrasts(rep.data.bin3$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep.data.bin3$intensity.bin) <- c(-1, 1) # contrasts for bin
contrasts(rep.data.bin3$session) <- c(-1, 1) # contrasts for session
rep.data.bin3$Subject <- factor(rep.data.bin3$Subject)

# 2.6.1.1. Low bin
layout(rbind(1,2), heights=c(5,1))
plot(x = rep_data6$Subject, y = rep_data6$low.bin.log.alpha.rand1, 
     col = "red", pch = 16, main = "Alpha power in low decrement bin",
     xlab = "Subject", ylab = "Log alpha power")
points(x = rep_data6$Subject, y = rep_data6$low.bin.log.alpha.sqr1,
       col = " black", pch = 16)
abline(h = mean(rep_data6$low.bin.log.alpha.rand1), col = "red")
abline(h = mean(rep_data6$low.bin.log.alpha.sqr1), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = levels(rep.data.bin3$configuration), 
       col = c("black", "red"),
       pch = 16)
par(defaults)
# 3.6.1.1. high bin
layout(rbind(1,2), heights=c(5,1))
plot(x = rep_data6$Subject, y = rep_data6$high.bin.log.alpha.rand1, 
     col = "red", pch = 16, main = "Alpha power in high decrement bin",
     xlab = "Subject", ylab = "Log alpha power")
points(x = rep_data6$Subject, y = rep_data6$high.bin.log.alpha.sqr1,
       col = " black", pch = 16)
abline(h = mean(rep_data6$high.bin.log.alpha.rand1), col = "red")
abline(h = mean(rep_data6$high.bin.log.alpha.sqr1), 
       col = 'black')
par(mar=c(0, 0, 0, 0))
plot.new()
legend("center", legend = levels(rep.data.bin3$configuration), 
       col = c("black", "red"),
       pch = 16)
par(defaults)

# 3.2.1. ANOVA
bin.log.alpha.baseline <- lme(alpha ~ 1, random = ~1|Subject/configuration/intensity.bin, 
                              data = rep.data.bin3[rep.data.bin3$session == 1,], 
                              method = "ML") #baseline
bin.log.alpha.config <- update(bin.log.alpha.baseline, .~. + configuration)
bin.log.alpha.intensity.bin <- update(bin.log.alpha.config, .~. + intensity.bin)
bin.log.alpha.lme <- update(bin.log.alpha.intensity.bin, .~. + configuration:intensity.bin)
anova(bin.log.alpha.baseline, bin.log.alpha.config, bin.log.alpha.intensity.bin,
      bin.log.alpha.lme)

# 3.4. Correlations
