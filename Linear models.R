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
# 0. Contrasts
contrasts(rep.data.long2$configuration) <- contr.treatment(2, base = 1)
contrasts(rep.data.long2$group.original) <- contr.treatment(2, base = 2)

# Occ
# 1. Model occ 1
occ.model1 <- lm(occ.nd1 ~ configuration + group.original + log.alpha, 
                 data = rep.data.long2)
plot(occ.model1)

# 2. Model occ 2
occ.model2 <- lm(occ.nd1 ~ configuration + group + log.alpha, 
                 data = rep.data.long2)
plot(occ.model1)

anova(occ.model1)

# 2. Left nd2
# 2.1. Model 1
left.nd2.model1 <- lm(left.nd2 ~ configuration + group.original + log.alpha, 
                 data = rep.data.long2)
plot(left.nd2.model1)

anova(left.nd2.model1)

# 2.2. Model 2
left.nd2.model2 <- lm(left.nd2 ~ configuration + group.original + log.alpha + threshold, 
                      data = rep.data.long2)
plot(left.nd2.model2)

anova(left.nd2.model2)

# 3. Right nd2
# Correct positive skew with square root transform on inverse values
rep.data.long2$transf.right.nd2 <- sqrt(max(rep.data.long2$right.nd2) - 
                                          rep.data.long2$right.nd2)

questionnaire.ERPs$transf.right.sqr.nd2_1 <- sqrt(max(questionnaire.ERPs$right.sqr.nd2_1) - 
                                          questionnaire.ERPs$right.sqr.nd2_1)


# 3.1. Model 1
right.nd2.model1 <- lm(transf.right.nd2 ~ configuration + conf.4 + log.alpha, 
                      data = rep.data.long2)


right.nd2.model1 <- lm(transf.right.sqr.nd2_1 ~ conf.4_1 + log.alpha.1, 
                       data = questionnaire.ERPs)

right.diff.model1 <- lm(right.diff ~ mean.log.alpha, 
                      data = rep.data.long2[rep.data.long2$session == 1,])

anova(right.diff.model)

# 4. Confidence ratings

plot(right.nd2.model1)

anova(right.nd2.model1)

resid(right.nd2.model1)

plot(right.nd2.model1$residuals)

plot(rep.data.long2$log.alpha, rep.data.long2$right.nd2)
abline(lm(rep.data.long2$right.nd2 ~ rep.data.long2$log.alpha))


anova(right.nd2.model1)

# 3.2. Model 2
hist(sqrt(max(rep.data.long2$right.nd2) - rep.data.long2$right.nd2))
rep.data.long2$log.right.nd2 < log()
right.nd2.model2 <- lm(log(right.nd2) ~ configuration + group.original + 
                         log.alpha * threshold, 
                       data = rep.data.long2)

plot(right.nd2.model2)
anova(right.nd2.model2)

plot(rep.data.long2$log.alpha, rep.data.long2$right.nd2)
abline(lm(rep.data.long2$right.nd2 ~ rep.data.long2$log.alpha))

plot(rep.data.long2$threshold, rep.data.long2$right.nd2)
abline(lm(rep.data.long2$right.nd2 ~ rep.data.long2$threshold))

# 3.3. Model 3
right.nd2.model3 <- lm(right.nd2 ~ configuration + group.original + 
                         log.alpha * RT.mean, 
                       data = rep.data.long2)

plot(right.nd2.model3)
anova(right.nd2.model3)

plot(rep.data.long2$log.alpha, rep.data.long2$right.nd2)
abline(lm(rep.data.long2$right.nd2 ~ rep.data.long2$log.alpha))

plot(rep.data.long2$RT.mean, rep.data.long2$right.nd2)
abline(lm(rep.data.long2$right.nd2 ~ rep.data.long2$RT.mean))

# 3.4. Model 4
right.nd2.model4 <- lm(right.nd2 ~ configuration + group.original + 
                         log.alpha + Pc, 
                       data = rep.data.long2)

anova(right.nd2.model4)


#----------------------------------Alpha power-----------------------------------
alpha.model1 <- lm(rep.data.long2$log.alpha ~ rep.data.long2$RT)

anova(alpha.model1)

alpha.model2 <- lm(rep.data.long2$Pc ~ rep.data.long2$log.alpha)

anova(alpha.model2)

plot(rep.data.long2$log.alpha, rep.data.long2$Pc)
abline(lm(rep.data.long2$Pc ~ rep.data.long2$log.alpha))

# Assess assumptions - multicollinearity, normality of residuals, independence

# cross-validation

