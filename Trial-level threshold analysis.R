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


#--------------------------------------------ERPS-----------------------------------------
# 2. ANOVAs
# 2.1. Set contrasts
contrasts(rep.data.bin3$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep.data.bin3$intensity.bin) <- c(-1, 1) # contrasts for bin
contrasts(rep.data.bin3$session) <- c(-1, 1) # contrasts for session
rep.data.bin3$Subject <- factor(rep.data.bin3$Subject)

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

# 2.5. nd1
bin.nd1.baseline <- lme(occ.nd1 ~ 1, random = ~1|Subject/configuration/intensity.bin, 
                          data = rep.data.bin3[rep.data.bin3$session == 1,], 
                          method = "ML") #baseline
bin.nd1.config <- update(bin.nd1.baseline, .~. + configuration)
bin.nd1.intensity.bin <- update(bin.nd1.config, .~. + intensity.bin)
bin.nd1.lme <- update(bin.nd1.intensity.bin, .~. + configuration:intensity.bin)
anova(bin.nd1.baseline, bin.nd1.config, bin.nd1.intensity.bin,
      bin.nd1.lme)

# 2.5.1. Post-hocs
bin.nd1.lme <- lme(occ.nd1 ~ configuration * intensity.bin * bin.group, 
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

# 2.6. left nd2
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

# 2.7. right nd2
# 2.7.1. ANOVA
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

# 2.7.2. Post-hocs
bin.right.nd2.lme <- lme(right.nd1 ~ configuration * intensity.bin * bin.group, 
                           random = ~1|Subject/configuration/intensity.bin, 
                           data = rep.data.bin3[rep.data.bin3$session == 1,], 
                           method = "ML")
anova(bin.right.nd2.lme)

lsmeans(bin.right.nd2.lme, pairwise ~ configuration | bin.group * intensity.bin)

plot(lsmeans(bin.right.nd2.lme, pairwise ~ configuration | intensity.bin)$lsmeans, 
     main = "Confidence intervals right nd2")

plot(lsmeans(bin.right.nd2.lme, pairwise ~ configuration | bin.group * 
               intensity.bin)$contrasts, 
     main = "Confidence intervals contrasts right nd2")

# 2.7.3. Line plot
bin.right.nd2.line <- ggplot(rep.data.bin3[rep.data.bin3$session == 1,], 
                             aes(x = intensity.bin, y = right.nd2, 
                                 colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Right Nd2 mean amplitude", x = "bin power",
       y = "Left Nd2 mean amplitude", 
       colour = "configuration")
bin.right.nd2.line

# 2.7.4 Scatterplot
plot(x = questionnaire.ERPs$Subject, y = questionnaire.ERPs$low.bin.right.nd2.sqr_1, 
     col = "red", pch = 16, main = "Right nd2 for low bin trials",
     xlab = "Subject", ylab = "Mean amplitude")
points(x = questionnaire.ERPs$Subject, y = questionnaire.ERPs$low.bin.right.nd2.rand_1,
       col = " black", pch = 16)
legend(0, -0.2, legend = levels(rep.data.long2$configuration), 
       col = c("black", "red"),
       pch = 16)
abline(h = mean(questionnaire.ERPs$low.bin.right.nd2.sqr_1), col = "red")
abline(h = mean(questionnaire.ERPs$low.bin.right.nd2.rand_1), 
       col = 'black')

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
# 2. ANOVAs
# 2.1. Set contrasts
contrasts(rep.data.bin3$configuration) <- c(-1, 1) # contrasts for config
contrasts(rep.data.bin3$intensity.bin) <- c(-1, 1) # contrasts for bin
contrasts(rep.data.bin3$session) <- c(-1, 1) # contrasts for session
rep.data.bin3$Subject <- factor(rep.data.bin3$Subject)

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

# 2.5. nd1
bin.nd1.baseline <- lme(occ.nd1 ~ 1, random = ~1|Subject/configuration/intensity.bin, 
                        data = rep.data.bin3[rep.data.bin3$session == 1,], 
                        method = "ML") #baseline
bin.nd1.config <- update(bin.nd1.baseline, .~. + configuration)
bin.nd1.intensity.bin <- update(bin.nd1.config, .~. + intensity.bin)
bin.nd1.lme <- update(bin.nd1.intensity.bin, .~. + configuration:intensity.bin)
anova(bin.nd1.baseline, bin.nd1.config, bin.nd1.intensity.bin,
      bin.nd1.lme)

# 2.5.1. Post-hocs
bin.nd1.lme <- lme(occ.nd1 ~ configuration * intensity.bin * bin.group, 
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

# 2.6. left nd2
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

# 2.7. right nd2
# 2.7.1. ANOVA
bin.right.nd2.baseline <- lme(log.alpha ~ 1, 
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

# 2.7.2. Post-hocs
bin.right.nd2.lme <- lme(right.nd1 ~ configuration * intensity.bin * bin.group, 
                         random = ~1|Subject/configuration/intensity.bin, 
                         data = rep.data.bin3[rep.data.bin3$session == 1,], 
                         method = "ML")
anova(bin.right.nd2.lme)

lsmeans(bin.right.nd2.lme, pairwise ~ configuration | bin.group * intensity.bin)

plot(lsmeans(bin.right.nd2.lme, pairwise ~ configuration | intensity.bin)$lsmeans, 
     main = "Confidence intervals right nd2")

plot(lsmeans(bin.right.nd2.lme, pairwise ~ configuration | bin.group * 
               intensity.bin)$contrasts, 
     main = "Confidence intervals contrasts right nd2")

# 2.7.3. Line plot
bin.right.nd2.line <- ggplot(rep.data.bin3[rep.data.bin3$session == 1,], 
                             aes(x = intensity.bin, y = right.nd2, 
                                 colour = configuration)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = configuration)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(title = "Right Nd2 mean amplitude", x = "bin power",
       y = "Left Nd2 mean amplitude", 
       colour = "configuration")
bin.right.nd2.line

# 2.7.4 Scatterplot
plot(x = questionnaire.ERPs$Subject, y = questionnaire.ERPs$low.bin.right.nd2.sqr_1, 
     col = "red", pch = 16, main = "Right nd2 for low bin trials",
     xlab = "Subject", ylab = "Mean amplitude")
points(x = questionnaire.ERPs$Subject, y = questionnaire.ERPs$low.bin.right.nd2.rand_1,
       col = " black", pch = 16)
legend(0, -0.2, legend = levels(rep.data.long2$configuration), 
       col = c("black", "red"),
       pch = 16)
abline(h = mean(questionnaire.ERPs$low.bin.right.nd2.sqr_1), col = "red")
abline(h = mean(questionnaire.ERPs$low.bin.right.nd2.rand_1), 
       col = 'black')

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