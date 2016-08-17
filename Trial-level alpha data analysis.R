# Data import packages
library(xlsx)
# Data manipulation packages
library(outliers)
library(dplyr)
library(tidyr)
library(reshape2)
# Plotting packages
library(lattice)
library(Hmisc)
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

#---------------------------Exploratory data analysis---------------------------
defaults <- par() #save graphical parameters defaults

# 1.1. Compute variances and means
by(rep_data_alpha3[,7:20], list(rep_data_alpha3$configuration, 
                              rep_data_alpha3$session, rep_data_alpha3$group.original), 
   stat.desc, basic = FALSE)

# 1.2. Normality tests
# 1.2.1. Function to test normality of data
normality.test <- function(data, variable, color) {
  print(variable)
  print(shapiro.test(data))
  qqnorm(data, main = variable);qqline(data, col = color)
}

# 1.2.2. Compute normality and plot qqplot for all dependent variables
par(mfrow = c(3,3))
normal.tests <- mapply(normality.test, rep_data_alpha3[,c(7:10,13,15,17:20)], 
                       colnames(rep_data_alpha3[,c(7:10,13,15,17:20)]), 
                       1:ncol(rep_data_alpha3[,c(7:10,13,15,17:20)]))
par(mfrow = c(1,1))

# 1.3. Histograms for normality checks
# 1.3..1. Nd1
hist(rep_data_alpha3$occ.nd1, main = "nd1 mean amplitude", ylim = c(0, 0.25),
     xlab = "amplitude", col = 1, prob = T)
lines(density(rep_data_alpha3$occ.nd1), col = 2)
# 1.3.2. Nd2 left
par(mfrow = c(1,2))
hist(rep_data_alpha3$left.nd2, main = "left nd2 mean amplitude", ylim = c(0, 0.5),
     xlab = "amplitude", col = 2, prob = T)
lines(density(rep_data_alpha3$left.nd2), col = 4, lwd = 3)
# 1.3.3. Nd2 right
hist(rep_data_alpha3$right.nd2, main = "Right nd2 mean amplitude", ylim = c(0, 0.5),
     xlab = "amplitude", col = 3, prob = T)
lines(density(rep_data_alpha3$right.nd2), col = 9, lwd = 3)
par(mfrow = c(1,1))
# 1.3.4. RL nd2 right
hist(rep_data_alpha3$RL.nd2, main = "RL nd2 mean amplitude", ylim = c(0, 0.5),
     xlab = "amplitude", col = 4, prob = T)
lines(density(rep_data_alpha3$RL.nd2), col = 10, lwd = 3)

# rep_data_alpha3$rev.RL.nd2 <- max(rep_data_alpha3$RL.nd2) - rep_data_alpha3$RL.nd2
# 
# hist(rep_data_alpha3$rev.RL.nd2, main = "rev RL nd2 mean amplitude", ylim = c(0, 0.5),
#      xlab = "amplitude", col = 4, prob = T)
# lines(density(rep_data_alpha3$rev.RL.nd2), col = 10, lwd = 3)
# 
# rep_data_alpha3$log.rev.RL.nd2 <- log(rep_data_alpha3$rev.RL.nd2)
# 
# rep_data_alpha3$sqrt.rev.RL.nd2 <- sqrt(rep_data_alpha3$rev.RL.nd2)
# 
# hist(rep_data_alpha3$sqrt.rev.RL.nd2, main = "sqrt rev RL nd2 mean amplitude", 
#      ylim = c(0, 1.6),
#      xlab = "amplitude", col = 4, prob = T)
# lines(density(rep_data_alpha3$sqrt.rev.RL.nd2), col = 10, lwd = 3)
# 
# alpha.sqrt.rev.RL.nd2.baseline <- lme(sqrt.rev.RL.nd2 ~ 1, 
#                              random = ~1|Subject/configuration/alpha.power, 
#                              data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
#                              method = "ML") #baseline
# alpha.sqrt.rev.RL.nd2.config <- update(alpha.sqrt.rev.RL.nd2.baseline, .~. + configuration)
# alpha.sqrt.rev.RL.nd2.alpha.power <- update(alpha.sqrt.rev.RL.nd2.config, .~. + alpha.power)
# alpha.sqrt.rev.RL.nd2.lme <- update(alpha.sqrt.rev.RL.nd2.alpha.power, .~. + 
#                              configuration:alpha.power)
# anova(alpha.sqrt.rev.RL.nd2.baseline, alpha.sqrt.rev.RL.nd2.config, alpha.sqrt.rev.RL.nd2.alpha.power,
#       alpha.sqrt.rev.RL.nd2.lme)

#-------------------------------------Tests------------------------------------
# 2. ANOVAs
# 2.1. Set contrasts
contrasts(rep_data_alpha3$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep_data_alpha3$alpha.power) <- c(-1, 1) # contrasts for alpha.power
contrasts(rep_data_alpha3$alpha.group) <- c(-1, 1) # contrasts for alpha.group
contrasts(rep_data_alpha3$session) <- c(-1, 1) # contrasts for session
rep_data_alpha3$Subject <- factor(rep_data_alpha3$Subject)

# 2.2. C1
# 2.2.1. ANOVA
alpha.C1.baseline <- lme(C1 ~ 1, random = ~1|Subject/configuration/alpha.power, 
                         data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                         method = "ML") #baseline
alpha.C1.config <- update(alpha.C1.baseline, .~. + configuration)
alpha.C1.alpha.power <- update(alpha.C1.config, .~. + alpha.power)
alpha.C1.lme <- update(alpha.C1.alpha.power, .~. + configuration:alpha.power)
anova(alpha.C1.baseline, alpha.C1.config, alpha.C1.alpha.power,
      alpha.C1.lme)

# 2.2.2. Line plot
alpha.C1.line <- ggplot(rep_data_alpha3[rep_data_alpha3$session == 1,], 
                        aes(x = alpha.power, y = C1, 
                            colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "C1 mean amplitude", x = "alpha power", y = "C1 mean amplitude", 
       colour = "configuration")
alpha.C1.line

# 2.3. P1
# 2.3.1. ANOVA
alpha.P1.baseline <- lme(P1 ~ 1, random = ~1|Subject/configuration/alpha.power, 
                         data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                         method = "ML") #baseline
alpha.P1.config <- update(alpha.P1.baseline, .~. + configuration)
alpha.P1.alpha.power <- update(alpha.P1.config, .~. + alpha.power)
alpha.P1.lme <- update(alpha.P1.alpha.power, .~. + configuration:alpha.power)
anova(alpha.P1.baseline, alpha.P1.config, alpha.P1.alpha.power,
      alpha.P1.lme)

# 2.3.2. Line plot
alpha.P1.line <- ggplot(rep_data_alpha3[rep_data_alpha3$session == 1,], 
                        aes(x = alpha.power, y = P1, 
                            colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "P1 mean amplitude", x = "alpha power", y = "P1 mean amplitude", 
       colour = "configuration")
alpha.P1.line

# 2.4. N1
# 2.4.1. ANOVA
alpha.N1.baseline <- lme(N1 ~ 1, random = ~1|Subject/configuration/alpha.power, 
                         data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                         method = "ML") #baseline
alpha.N1.config <- update(alpha.N1.baseline, .~. + configuration)
alpha.N1.alpha.power <- update(alpha.N1.config, .~. + alpha.power)
alpha.N1.lme <- update(alpha.N1.alpha.power, .~. + configuration:alpha.power)
anova(alpha.N1.baseline, alpha.N1.config, alpha.N1.alpha.power,
      alpha.N1.lme)

# 2.4.2. Line plot
alpha.N1.line <- ggplot(rep_data_alpha3[rep_data_alpha3$session == 1,], 
                         aes(x = alpha.power, y = N1, 
                             colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "N1 mean amplitude", x = "alpha power", y = "N1 mean amplitude", 
       colour = "configuration")
alpha.N1.line

# 2.5. nd1
alpha.nd1.baseline <- lme(occ.nd1 ~ 1, random = ~1|Subject/configuration/alpha.power, 
                    data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                    method = "ML") #baseline
alpha.nd1.config <- update(alpha.nd1.baseline, .~. + configuration)
alpha.nd1.alpha.power <- update(alpha.nd1.config, .~. + alpha.power)
alpha.nd1.lme <- update(alpha.nd1.alpha.power, .~. + configuration:alpha.power)
anova(alpha.nd1.baseline, alpha.nd1.config, alpha.nd1.alpha.power,
      alpha.nd1.lme)

# 2.5.1. Post-hocs
alpha.nd1.lme <- lme(occ.nd1 ~ configuration * alpha.power * alpha.group, 
                     random = ~1|Subject/configuration/alpha.power, 
                     data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                     method = "ML")
anova(alpha.nd1.lme)
summary(alpha.nd1.lme)

# 2.5.2. using lsmeans
lsmeans(alpha.nd1.lme, pairwise ~ configuration | alpha.power)

TukeyHSD(alpha.nd1.lme, c(
                rep_data_alpha3[rep_data_alpha3$session == 1,]$alpha.power * 
                  rep_data_alpha3[rep_data_alpha3$session == 1,]$configuration),
                conf.level = 0.95)

# # using multcomp
# # 1.
# # compute means for all combinations
# tmp <- expand.grid(configuration = unique(rep_data_alpha3$configuration),
#                    alpha.power = unique(rep_data_alpha3$alpha.power))
# x <- model.matrix(~ configuration * alpha.power, data = tmp)
# glht(alpha.nd1.lme, linfct = x)
# 
# # construct contrast matrix
# Tukey <- contrMat(table(rep_data_alpha3$configuration), "Tukey")
# K1 <- cbind(Tukey, matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)))
# rownames(K1) <- paste(levels(rep_data_alpha3$alpha.power)[1], rownames(K1), 
#                       sep = ":")
# K2 <- cbind(matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), Tukey)
# rownames(K2) <- paste(levels(rep_data_alpha3$alpha.power)[2], rownames(K2), 
#                       sep = ":")
# K <- rbind(K1, K2)
# colnames(K) <- c(colnames(Tukey), colnames(Tukey))
# #test
# summary(glht(alpha.nd1.lme, linfct = K %*% x))


# ANOVA TESTING
alpha.nd1.model <- lme(occ.nd1 ~ configuration * alpha.power, 
                       random = ~1|Subject/configuration/alpha.power, 
                          data = rep_data_alpha3, method = "ML")
anova(alpha.nd1.model)

tanova <- aov(occ.nd1 ~ (configuration * alpha.power) +
              Error(Subject/(configuration * alpha.power)), 
              data = rep_data_alpha3)
      
summary(alpha.nd1.baseline)

# Plots
alpha.nd1.line <- ggplot(rep_data_alpha3[rep_data_alpha3$session == 1,], 
                         aes(x = alpha.power, y = occ.nd1, 
                                              colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_grid(.~alpha.group)
  labs(title = "Nd1 mean amplitude", x = "alpha power", y = "Nd1 mean amplitude", 
       colour = "configuration")
alpha.nd1.line

plot(lsmeans(alpha.nd1.lme, pairwise ~ configuration | alpha.group * 
               alpha.power)$contrasts, 
     main = "Confidence intervals contrasts nd1")

# 2.6. left nd2
# 2.6.1. ANOVA
alpha.left.nd2.baseline <- lme(left.nd2 ~ 1, 
                          random = ~1|Subject/configuration/alpha.power, 
                          data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                          method = "ML") #baseline
alpha.left.nd2.config <- update(alpha.left.nd2.baseline, .~. + configuration)
alpha.left.nd2.alpha.power <- update(alpha.left.nd2.config, .~. + alpha.power)
alpha.left.nd2.lme <- update(alpha.left.nd2.alpha.power, .~. + 
                               configuration:alpha.power)
anova(alpha.left.nd2.baseline, alpha.left.nd2.config, alpha.left.nd2.alpha.power,
      alpha.left.nd2.lme)

# 2.6.2. Post-hocs
alpha.left.nd2.lme<- lme(left.nd2 ~ configuration * alpha.power * alpha.group, 
                       random = ~1|Subject/configuration/alpha.power, 
                       data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                       method = "ML")
anova(alpha.left.nd2.lme)

lsmeans(alpha.left.nd2.lme, pairwise ~ configuration | alpha.power)

plot(lsmeans(alpha.left.nd2.lme, pairwise ~ configuration | alpha.power)$lsmeans, 
     main = "Confidence intervals left nd2")

# 2.6.3. Line plot
alpha.left.nd2.line <- ggplot(rep_data_alpha3[rep_data_alpha3$session == 1,], 
                              aes(x = alpha.power, y = left.nd2, 
                                                   colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_grid(.~alpha.group) +
  labs(title = "Left Nd2 mean amplitude", x = "alpha power", 
       y = "Left Nd2 mean amplitude", 
       colour = "configuration")
alpha.left.nd2.line

# 2.7. right nd2
# 2.7.1. ANOVA
alpha.right.nd2.baseline <- lme(right.nd2 ~ 1, 
                                random = ~1|Subject/configuration/alpha.power, 
                                data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                          method = "ML") #baseline
alpha.right.nd2.config <- update(alpha.right.nd2.baseline, .~. + configuration)
alpha.right.nd2.alpha.power <- update(alpha.right.nd2.config, .~. + alpha.power)
alpha.right.nd2.lme <- update(alpha.right.nd2.alpha.power, .~. + 
                                configuration:alpha.power)
anova(alpha.right.nd2.baseline, alpha.right.nd2.config, alpha.right.nd2.alpha.power,
      alpha.right.nd2.lme)

summary(alpha.right.nd2.lme)

# 2.7.2. Post-hocs
alpha.right.nd2.lme <- lme(right.nd2 ~ configuration * alpha.power * alpha.group, 
                            random = ~1|Subject/configuration/alpha.power, 
                            data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                            method = "ML")
anova(alpha.right.nd2.lme)

lsmeans(alpha.right.nd2.lme, pairwise ~ configuration | alpha.group * alpha.power)

plot(lsmeans(alpha.right.nd2.lme, pairwise ~ configuration | alpha.power)$lsmeans, 
     main = "Confidence intervals right nd2")

plot(lsmeans(alpha.right.nd2.lme, pairwise ~ configuration | alpha.group * 
               alpha.power)$contrasts, 
     main = "Confidence intervals contrasts right nd2")

# 2.7.3. Line plot
alpha.right.nd2.line <- ggplot(rep_data_alpha3[rep_data_alpha3$session == 1,], 
                               aes(x = alpha.power, y = right.nd2, 
                                                    colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_grid(.~alpha.group) +
  labs(title = "Right Nd2 mean amplitude", x = "alpha power",
       y = "Left Nd2 mean amplitude", 
       colour = "configuration")
alpha.right.nd2.line

# 2.7.4 Scatterplot
plot(x = questionnaire.ERPs$Subject, y = questionnaire.ERPs$low.alpha.right.nd2.sqr_1, 
     col = "red", pch = 16, main = "Right nd2 for low alpha trials",
     xlab = "Subject", ylab = "Mean amplitude")
points(x = questionnaire.ERPs$Subject, y = questionnaire.ERPs$low.alpha.right.nd2.rand_1,
       col = " black", pch = 16)
legend(0, -0.2, legend = levels(rep.data.long2$configuration), 
       col = c("black", "red"),
       pch = 16)
abline(h = mean(questionnaire.ERPs$low.alpha.right.nd2.sqr_1), col = "red")
abline(h = mean(questionnaire.ERPs$low.alpha.right.nd2.rand_1), 
       col = 'black')

# 2.8. RL nd2
# 2.8.1. ANOVA
alpha.RL.nd2.baseline <- lme(RL.nd2 ~ 1, 
                             random = ~1|Subject/configuration/alpha.power, 
                          data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                          method = "ML") #baseline
alpha.RL.nd2.config <- update(alpha.RL.nd2.baseline, .~. + configuration)
alpha.RL.nd2.alpha.power <- update(alpha.RL.nd2.config, .~. + alpha.power)
alpha.RL.nd2.lme <- update(alpha.RL.nd2.alpha.power, .~. + 
                             configuration:alpha.power)
anova(alpha.RL.nd2.baseline, alpha.RL.nd2.config, alpha.RL.nd2.alpha.power,
      alpha.RL.nd2.lme)

# 2.8.2. Post-hocs

alpha.RL.nd2.lme <- lme(RL.nd2 ~ configuration * alpha.power * alpha.group, 
                           random = ~1|Subject/configuration/alpha.power, 
                           data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                           method = "ML")
anova(alpha.RL.nd2.lme)

posthoc.alpha.RL.nd2 <- lsmeans(alpha.RL.nd2.lme, pairwise ~ 
                                  configuration | alpha.power)

plot(lsmeans(posthoc.alpha.RL.nd2$lsmeans, 
     main = "Confidence intervals RL nd2"))

plot(lsmeans(alpha.RL.nd2.lme, pairwise ~ configuration | alpha.group * 
               alpha.power)$contrasts, 
     main = "Confidence intervals contrasts RL nd2")

qqnorm(alpha.RL.nd2.lme)
qqline(alpha.RL.nd2.lme)

# using multcomp
# 1.
# compute means for all combinations
tmp <- expand.grid(configuration = unique(rep_data_alpha3$configuration),
                   alpha.power = unique(rep_data_alpha3$alpha.power))
x <- model.matrix(~ configuration * alpha.power, data = tmp)
glht(alpha.RL.nd2.lme, linfct = x)

# construct contrast matrix
Tukey <- contrMat(table(rep_data_alpha3$configuration), "Tukey")
K1 <- cbind(Tukey, matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)))
rownames(K1) <- paste(levels(rep_data_alpha3$alpha.power)[1], rownames(K1), 
                      sep = ":")
K2 <- cbind(matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), Tukey)
rownames(K2) <- paste(levels(rep_data_alpha3$alpha.power)[2], rownames(K2), 
                      sep = ":")
K <- rbind(K1, K2)
colnames(K) <- c(colnames(Tukey), colnames(Tukey))
#test
summary(glht(alpha.RL.nd2.lme, linfct = K %*% x))

# 2.8.3. Plots
alpha.RL.nd2.line <- ggplot(rep_data_alpha3[rep_data_alpha3$session == 1,], 
                               aes(x = alpha.power, y = RL.nd2, 
                                   colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "RL Nd2 mean amplitude", x = "alpha power",
       y = "RL Nd2 mean amplitude", 
       colour = "configuration")
alpha.RL.nd2.line

# 2.9. N2
# 2.9.1. ANOVA
alpha.N2.baseline <- lme(N2 ~ 1, random = ~1|Subject/configuration/alpha.power, 
                             data = rep_data_alpha3[rep_data_alpha3$session == 1,],
                         method = "ML") #baseline
alpha.N2.config <- update(alpha.N2.baseline, .~. + configuration)
alpha.N2.alpha.power <- update(alpha.N2.config, .~. + alpha.power)
alpha.N2.lme <- update(alpha.N2.alpha.power, .~. + configuration:alpha.power)
anova(alpha.N2.baseline, alpha.N2.config, alpha.N2.alpha.power,
      alpha.N2.lme)

# 2.9.2 Line plot
alpha.N2.line <- ggplot(rep_data_alpha3[rep_data_alpha3$session == 1,], 
                            aes(x = alpha.power, y = N2, 
                                colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "N2 mean amplitude", x = "alpha power",
       y = "N2 mean amplitude", 
       colour = "configuration")
alpha.N2.line

# 2.10. LP
# 2.10.1. ANOVA
alpha.LP.baseline <- lme(LP ~ 1, random = ~1|Subject/configuration/alpha.power, 
                         data = rep_data_alpha3[rep_data_alpha3$session == 1,], 
                         method = "ML") #baseline
alpha.LP.config <- update(alpha.LP.baseline, .~. + configuration)
alpha.LP.alpha.power <- update(alpha.LP.config, .~. + alpha.power)
alpha.LP.lme <- update(alpha.LP.alpha.power, .~. + configuration:alpha.power)
anova(alpha.LP.baseline, alpha.LP.config, alpha.LP.alpha.power,
      alpha.LP.lme)

# 2.10.2 Line plot
alpha.LP.line <- ggplot(rep_data_alpha3[rep_data_alpha3$session == 1,], 
                        aes(x = alpha.power, y = LP, 
                            colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "LP mean amplitude", x = "alpha power",
       y = "LP mean amplitude", 
       colour = "configuration")
alpha.LP.line

by(rep_data_alpha3$LP, rep_data_alpha3$configuration, 
   stat.desc, basic = FALSE)

# 3. ANOVAs With session as factor
contrasts(rep_data_alpha3$session) <- c(-1, 1) # setting contrasts for session
# 3.4. nd1
alpha.nd1.baseline <- lme(occ.nd1 ~ 1, 
                               random = ~1|Subject/configuration/session/alpha.power, 
                               data = rep_data_alpha3, method = "ML") #baseline
alpha.nd1.config <- update(alpha.nd1.baseline, .~. + configuration)
alpha.nd1.session <- update(alpha.nd1.config, .~. + session)
alpha.nd1.alpha.power <- update(alpha.nd1.session, .~. + alpha.power)
alpha.nd1.config.session <- update(alpha.nd1.alpha.power, .~. + 
                                          configuration:session)
alpha.nd1.session.alpha.power <- update(alpha.nd1.config.session, .~. + 
                                               session:alpha.power)
alpha.nd1.config.alpha.power <- update(alpha.nd1.session.alpha.power, .~. + 
                                              configuration:alpha.power)
alpha.nd1.lme <- update(alpha.nd1.config.alpha.power, .~. + 
                               configuration:session:alpha.power)
anova(alpha.nd1.baseline, alpha.nd1.config, alpha.nd1.session, 
      alpha.nd1.alpha.power, alpha.nd1.config.session, 
      alpha.nd1.session.alpha.power, alpha.nd1.config.alpha.power, 
      alpha.nd1.lme)

# 3.5. left nd2
alpha.left.nd2.baseline <- lme(left.nd2 ~ 1, 
                               random = ~1|Subject/configuration/session/alpha.power, 
                         data = rep_data_alpha3, method = "ML") #baseline
alpha.left.nd2.config <- update(alpha.left.nd2.baseline, .~. + configuration)
alpha.left.nd2.session <- update(alpha.left.nd2.config, .~. + session)
alpha.left.nd2.alpha.power <- update(alpha.left.nd2.session, .~. + alpha.power)
alpha.left.nd2.config.session <- update(alpha.left.nd2.alpha.power, .~. + 
                                          configuration:session)
alpha.left.nd2.session.alpha.power <- update(alpha.left.nd2.config.session, .~. + 
                                               session:alpha.power)
alpha.left.nd2.config.alpha.power <- update(alpha.left.nd2.session.alpha.power, .~. + 
                                              configuration:alpha.power)
alpha.left.nd2.lme <- update(alpha.left.nd2.config.alpha.power, .~. + 
                               configuration:session:alpha.power)
anova(alpha.left.nd2.baseline, alpha.left.nd2.config, alpha.left.nd2.session, 
      alpha.left.nd2.alpha.power, alpha.left.nd2.config.session, 
      alpha.left.nd2.session.alpha.power, alpha.left.nd2.config.alpha.power, 
      alpha.left.nd2.lme)

# 3.6. right nd2
alpha.right.nd2.baseline <- lme(right.nd2 ~ 1, 
                               random = ~1|Subject/configuration/session/alpha.power, 
                               data = rep_data_alpha3, method = "ML") #baseline
alpha.right.nd2.config <- update(alpha.right.nd2.baseline, .~. + configuration)
alpha.right.nd2.session <- update(alpha.right.nd2.config, .~. + session)
alpha.right.nd2.alpha.power <- update(alpha.right.nd2.session, .~. + alpha.power)
alpha.right.nd2.config.session <- update(alpha.right.nd2.alpha.power, .~. + 
                                          configuration:session)
alpha.right.nd2.session.alpha.power <- update(alpha.right.nd2.config.session, .~. + 
                                               session:alpha.power)
alpha.right.nd2.config.alpha.power <- update(alpha.right.nd2.session.alpha.power, .~. + 
                                              configuration:alpha.power)
alpha.right.nd2.lme <- update(alpha.right.nd2.config.alpha.power, .~. + 
                               configuration:session:alpha.power)
anova(alpha.right.nd2.baseline, alpha.right.nd2.config, alpha.right.nd2.session, 
      alpha.right.nd2.alpha.power, alpha.right.nd2.config.session, 
      alpha.right.nd2.session.alpha.power, alpha.right.nd2.config.alpha.power, 
      alpha.right.nd2.lme)

  
# Multiplot
rep_data_alpha3[rep_data_alpha3$session == 1,] %>%
   gather(occ.nd1, left.nd2, right.nd2, RL.nd2, 
          key = "var", value = "value") %>% 
  ggplot(aes(x = alpha.power, y = value, color = configuration, shape = alpha.group)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()